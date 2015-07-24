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
    use stm::{Result};

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
        /*
         * while (true)
         * time = global lock
         * if ((time&1)!=0)
         *  continue
         * for each (addr, val) in reads
         *      if (∗addr != val)
         *          TXAbort() // abort will longjmp if (time == global lock)
         * return time
         */
        fn validate(&mut self) -> Result<usize> {
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
    }

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
    }

    //impl<'a, A> stm::TVar<A> for TVar<'a, A> {
    //}
}

#[test]
fn it_works() {
}
