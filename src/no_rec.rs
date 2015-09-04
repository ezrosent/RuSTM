//#![allow(unused)]

use std::sync::atomic::{AtomicUsize, Ordering};
use std::vec::Vec;
use std::collections::HashMap;
use std::cell::UnsafeCell;
use std::hash::{Hash, Hasher};
use std::mem::{align_of, size_of_val, transmute};
use std::slice::bytes::copy_memory;

use ::Result;

pub struct GlobalState {
    version : AtomicUsize
}

/// Baseline implementation of GlobalState, currently do not hqve the aid of the compiler, so this
/// may have major issues.
/// TODO: make a proper implementation of the GlobalState trait
impl GlobalState {
    fn newLocal<'v, 'g>(&'g self) -> LocalState<'v, 'g> {
        LocalState {
            reads: Vec::new(),
            writes: HashMap::new(),
            global: self,
            snapshot: self.version.load(Ordering::SeqCst),
        }
    }

    pub fn run<A, F>(&self, mut func : F) -> A
    where F: FnMut(&mut LocalState) -> Result<A> {
        let mut local = self.newLocal();
        match func(&mut local).spec_move() {
            Ok(r) => r,
            _ => self.run(func)
        }
    }

    pub fn or_else<A, F, G>(&self, mut func1 : F, func2 : G) -> A
        where F: FnMut(&mut LocalState) -> Result<A>,
              G: FnMut(&mut LocalState) -> Result<A> {
            let mut local1 = self.newLocal();
            match func1(&mut local1).spec_move() {
                Ok(r) => r,
                //TODO: this may not work, compiler may not know that
                //local1, which borrows self, has relinquised ownership
                _ => self.run(func2)
            }
    }
}

pub struct LocalState<'v, 'g : 'v> {
    /// the norec paper has the reads stored as a list of <address, value> pairs. For us it
    /// would probably be better to have versioned TVars. That way we always have a fast
    /// comparison operation and avoid doing extra dynamic dispatch when validating reads.
    reads : Vec<(TVarAddr<'v, 'g>, usize)>,

    /// for writes we will probably have to cast unsafely to get this to work. TVars just hold
    /// arbitrary types and we only have one table.  XXX: gross
    writes : HashMap<TVarAddr<'v, 'g>, Box<[u8]>>,

    /// STM environment we are using
    /// TODO: how are we getting away with this not being a mutable reference?
    global : &'g GlobalState,

    /// last version at which our view of the global state was valid
    snapshot : usize
}

impl<'v, 'g : 'v> LocalState<'v, 'g> {

    /// Validate the current local state against the state of the world,
    /// Aborts if a TVar in our readset has been written, otherwise
    /// returns the version number to with we are consistent
    pub fn validate(&self) -> Result<usize> {
        loop {
            let time = self.global.version.load(Ordering::SeqCst);
            if (time & 1) == 0 {
                continue;
            }
            for &(addr, version) in &self.reads {
                if addr.version().load(Ordering::SeqCst) != version {
                    return Result::abort();
                }
            }
            return Result::ret(time);
        }
    }

    /// Acquire reference to value in TVar within the transaction
    pub fn read<'l, A>(&'l mut self, tvar : &'v TVar<'g, A>) -> Result<&'l A>
        where 'v : 'l
    {
        let addr : TVarAddr<'v, 'g> = unsafe { tvar.to_addr() } ;

        // Should be &&'l ?
        if let Some(v) = self.writes.get(&addr) {
            let r : && [u8] = &&**v;
            assert_eq!(r.len(), ::std::mem::size_of::<A>());
            let r2 : && A = unsafe { transmute(r) };
            return Result::ret(r2)
        }

        if self.snapshot == self.global.version.load(Ordering::SeqCst) {
            self.reads.push((addr, tvar.version.load(Ordering::SeqCst)));
            Result::ret(unsafe {tvar.get_val()})
        } else {
            self.validate().bind( | v | {
                assert!(self.snapshot <= v);
                self.snapshot = v;
                self.reads.push((addr, tvar.version.load(Ordering::SeqCst)));
                Result::ret(unsafe {tvar.get_val()})
            })
        }
    }

    /// Add the TVar and value to our writeset, these are thread-local until the
    /// transaction commits
    pub fn write<A : Copy>(&mut self, tvar : &'v mut TVar<'g, A>, val : A) -> Result<()> {
        let b : Box<[A]> = Box::new([val]);
        unsafe {
            self.writes.insert(tvar.to_addr(), transmute(b));
        }
        Result::ret(())
    }


    /// if (read−only transaction)
    ///     return
    /// while (!CAS(&global lock, snapshot, snapshot + 1))
    ///     snapshot = Validate()
    /// for each (addr, val) in writes
    ///     ∗addr = val
    /// global lock = snapshot + 2 // one more than CAS above
    pub fn commit(&mut self) -> Result<()> {
        fn commit_loop(l : &mut LocalState) -> Result<()> {
            if l.global.version.compare_and_swap(l.snapshot, l.snapshot + 1, Ordering::SeqCst) == l.snapshot {
                Result::ret(())
            } else {
                l.validate().bind(|v| {
                    l.snapshot = v;
                    commit_loop(l)
                })
            }
        }
        if self.writes.is_empty() {
            Result::ret(())
        } else {
            commit_loop(self).bind(| _ | {
                for (addr, value) in &self.writes {
                    let dst = unsafe { addr.aligned_mem() };
                    debug_assert_eq!(value.len(), dst.len());
                    copy_memory(value, dst);
                    addr.addr.version.fetch_add(1, Ordering::SeqCst);
                }
                self.global.version.compare_and_swap(self.snapshot + 1,
                                                     self.snapshot + 2, Ordering::SeqCst);
                Result::ret(())
            })
        }
    }
}


/// Haskell-style container of mutable state within a transaction
/// TODO: impl TVar trait
pub struct TVar<'a, A: ?Sized> {
    version : AtomicUsize,
    global : &'a GlobalState,
    val : UnsafeCell<A>,
}

impl<'a, A: ?Sized> TVar<'a, A> {
    unsafe fn get_val(&self) -> &A {
        &*self.val.get()
    }

    unsafe fn get_val_mut(&self) -> &mut A {
        &mut *self.val.get()
    }
}
impl<'a, A> TVar<'a, A> {
    unsafe fn to_addr<'b>(&'b self) -> TVarAddr<'b, 'a> {
        use std::raw::Slice;
        let fat_ptr = Slice::<u8> {
            data: transmute(self),
            len: size_of_val(self),
        };
        //let ptr : &*mut TVar<'a, [u8]> = transmute(self));
        TVarAddr {
            align: align_of::<A>(),
            addr: transmute(fat_ptr),
        }
    }
}

#[derive(Copy, Clone)]
/// Lies... Lies
struct TVarAddr<'a, 'b : 'a> {
    align: usize,
    addr: &'a TVar<'b, [u8]>, //*mut
    //_phan: PhantomData<&'a TVar<'b, ()>>,
}

impl<'a, 'b : 'a> Hash for TVarAddr<'a, 'b> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.raw_addr().hash(state);
    }
}

impl<'a, 'b : 'a> Eq for TVarAddr<'a, 'b> {}

impl<'a, 'b : 'a> PartialEq for TVarAddr<'a, 'b> {
    fn eq(&self, other: &Self) -> bool {
        let c = self.raw_addr() == other.raw_addr();
        if c {
            debug_assert_eq!(self.raw_len(), other.raw_len());
            debug_assert_eq!(self.align, other.align);
        }
        c
    }
}

impl<'a, 'b : 'a> TVarAddr<'a, 'b> {
    fn raw_addr(self) -> usize {
        self.addr as *const _ as *const () as usize
    }

    fn raw_len(self) -> usize {
        let y = self.addr.val.get();
        unsafe { transmute::<_,&[u8]>(y).len() }
    }

    unsafe fn aligned_mem(self) -> &'a mut [u8] {
        let buf = self.addr.val.get();
        let p = buf as *mut () as usize;
        // Assumes alignment is power of 2
        let o = ((p + (self.align - 1)) & self.align) - p;
        &mut (*buf)[o..]
    }

    fn version(self) -> &'a AtomicUsize {
        &(*self.addr).version
    }
}

pub fn align_down_mut<T>(sp: *mut T, n: usize) -> *mut T {
  let sp = (sp as usize) & !(n - 1);
  sp as *mut T
}
