#![allow(unused)]
#![allow(raw_pointer_derive)]

use std::sync::atomic::{AtomicUsize, Ordering};
use std::vec::Vec;
use std::collections::HashMap;
use std::cell::UnsafeCell;
use std::marker::PhantomData;
use std::mem::transmute;

use ::Result;

pub struct GlobalState {
    version : AtomicUsize
}

#[allow(raw_pointer_derive)]
#[derive(Hash, Eq, PartialEq, Copy, Clone)]
/// Lies... Lies
struct TVarAddr<'a, 'b : 'a>(*mut TVar<'b, ()>, PhantomData<&'a TVar<'b, ()>>);

impl<'a, 'b : 'a> TVarAddr<'a, 'b> {
    fn version(self) -> &'a AtomicUsize {
        unsafe { &(*self.0).version }
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
    global : &'g GlobalState,

    /// last version at which our view of the global state was valid
    snapshot : usize
}

impl<'v, 'g : 'v> LocalState<'v, 'g> {

    /// Validate the current local state against the state of the world,
    /// Aborts if a TVar in our readset has been written, otherwise
    /// returns the version number to with we are consistent
    fn validate(&self) -> Result<usize> {
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
    fn read<'l, A>(&'l mut self, tvar : &'v TVar<'g, A>) -> Result<&'l A>
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
    fn write<A : Copy>(&mut self, tvar : &'v mut TVar<'g, A>, val : A) -> Result<()> {
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
    fn commit(&mut self) -> Result<()> {
        fn commit_loop(l : &mut LocalState) -> Result<()> {
            if l.global.version.compare_and_swap(l.snapshot, l.snapshot + 1, Ordering::SeqCst)
                == l.snapshot {
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
                    //XXX: how do we safely write back the values now? we need to
                    //store some size/type-related information now.
                    //maybe we do want Any? or are we better off just with a
                    //struct {
                    //  size
                    //  A /* size of A is size */
                    //}
                    let tvar : &TVar<u8> =  unsafe { transmute(addr.0) };
                    tvar.version.fetch_add(1, Ordering::SeqCst);
                }
                self.global.version.compare_and_swap(self.snapshot + 1,
                                                     self.snapshot + 2, Ordering::SeqCst);
                Result::ret(())
            })
        }
    }
}

/// Haskell-style container of mutable state within a transaction
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
    unsafe fn to_addr<'b>(&'b self) -> TVarAddr<'b, 'a> {
        let ptr : &*mut TVar<'a, ()> = transmute(&self);
        TVarAddr(*ptr, PhantomData)
    }
}
