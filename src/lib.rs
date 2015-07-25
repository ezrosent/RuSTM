#![allow(dead_code)]
pub mod stm {
    use std::result::Result as Either;
    use std::mem::{transmute, forget, uninitialized};
    use std::ptr::swap;
    /// We cannot make a real monad, so this is going to be a fake linear type a la Clean.
    pub struct Result<A>(Either<A, ()>);

    impl<A> Result<A> {
        pub fn ret(a: A) -> Result<A> {
            Result(Either::Ok(a))
        }

        pub fn abort() -> Result<A> {
            Result(Either::Err(()))
        }

        /// just don't look
        fn spec_move(mut self) -> Either<A, ()> {
            unsafe {
                let mut x = uninitialized();
                swap(&mut x, transmute(&mut self));
                forget(self);
                x
            }
        }

        pub fn bind<B, F>(self, f : F) -> Result<B> where F : FnOnce(A) -> Result<B> {
            Result(self.spec_move().and_then(|x| f(x).spec_move()))
        }
    }

    impl<A> Drop for Result<A> {
        // Best we can do absent a true linear type
        fn drop(&mut self) {
            panic!()
        }
    }

    pub trait GlobalState {
        /// A hook for transaction-local state
        type LocalState;

        fn run<A, F>(&self, F) -> A
            where F: FnMut(&mut Self::LocalState) -> Result<A>;

        fn or_else<A, F, G>(&self, F, G) -> Result<A>
            where F: FnMut(&mut Self::LocalState) -> Result<A>,
                  G: FnMut(&mut Self::LocalState) -> Result<A>;
    }

    pub trait TVar<A> {
        /// A wrapper around an object whose access is mediated by the STM implementation
        type LocalState;

        fn read(&mut self, Self::LocalState) -> Result<A>;

        fn write(&mut self, Self::LocalState, A) -> Result<()>;
    }
}
pub mod no_rec {
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::vec::Vec;
    use std::collections::HashMap;
    use std::cell::UnsafeCell;
    use std::marker::PhantomData;
    use std::mem::transmute;
    use stm::Result;

    pub struct GlobalState {
        version : AtomicUsize
    }

    #[allow(raw_pointer_derive)]
    #[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
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
        fn read<A>(&mut self, tvar : &'v TVar<'g, A>) -> Result<&A> {
            let addr = unsafe { tvar.to_addr() } ;
            if let Some(v) =  self.writes.get(&addr) {
                // TODO(ezrosent) may not be transmuting the right thing here
                return unsafe { Result::ret(transmute(v)) };
            }

            if self.snapshot == self.global.version.load(Ordering::SeqCst) {
                self.reads.push((addr, tvar.version.load(Ordering::SeqCst)));
                Result::ret(unsafe {tvar.get_val()})
            } else {
                self.validate().bind( | v | {
                    self.snapshot = v;
                    self.reads.push((addr, tvar.version.load(Ordering::SeqCst)));
                    Result::ret(unsafe {tvar.get_val()})
                })
            }
        }

        /// Add the TVar and value to our writeset, these are thread-local until the
        /// transaction commits
        fn write<A : Clone>(&mut self, tvar : &'v mut TVar<'g, A>, val : A) -> Result<()> {
            //XXX: transmute doesn't work here. It says that Box<[u8]> could be 128 bits, while
            //Box<A> could be 64 bits.

            /*
            unsafe {
                self.writes.insert(tvar.to_addr(), transmute(Box::new(val.clone())));
            }
            */

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
                        let tvar : &mut TVar<()> =  unsafe { transmute(addr.0) };
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
    pub struct TVar<'a, A> {
        version : AtomicUsize,
        val : UnsafeCell<A>,
        global : &'a GlobalState
    }

    impl<'a, A> TVar<'a, A> {
        unsafe fn get_val(&self) -> &A {
            &*self.val.get()
        }

        unsafe fn get_val_mut(&self) -> &mut A {
            &mut *self.val.get()
        }
        unsafe fn to_addr<'b>(&'b self) -> TVarAddr<'b, 'a> {
            let ptr : *mut TVar<'a, ()> = transmute(self);
            TVarAddr(ptr, PhantomData)
        }
    }

}

#[test]
fn it_works() {
}
