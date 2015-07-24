mod STM {
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
        pub fn spec_move(mut self) -> Either<A, ()> {
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

    trait Stm {
        /// A hook for transaction-local state
        type LocalState;

        fn run<A, F>(&self, F) -> A
            where F: FnMut(&mut Self::LocalState) -> Result<A>,
                  Self::LocalState: StmHelper<A>;

        fn or_else<A, F, G>(&self, F, G) -> Result<A>
            where F: FnMut(&mut Self::LocalState) -> Result<A>,
                  G: FnMut(&mut Self::LocalState) -> Result<A>,
                  Self::LocalState: StmHelper<A>;
    }

    trait StmHelper<A> {
        /// A wrapper around an object whose access is mediated by the STM implementation
        type TVar;

        /// Read/Write need to return Result<A>
        fn read(&mut self, Self::TVar) -> A;

        fn write(&mut self, Self::TVar, A);
    }
}
mod NoRec {
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::vec::Vec;
    use std::collections::HashMap;
    use std::cell::UnsafeCell;
    use std::marker::PhantomData;
    use STM;

    struct GlobalState {
        version : AtomicUsize
    }

    #[derive(Hash, Eq, PartialEq, Debug, Clone)]
    /// Lies... Lies
    struct TVarAddr<'a, 'b : 'a>(*mut (), PhantomData<&'a TVar<'b, ()>>);


<<<<<<< HEAD
    struct LocalState<'v, 'g : 'v> {
        /// the norec paper has the reads stored as a list of <address, value> pairs. For us it
        /// would probably be better to have versioned TVars. That way we always have a fast
        /// comparison operation and avoid doing extra dynamic dispatch when validating reads.
        reads : Vec<(TVarAddr<'v, 'g>, usize)>,

        /// for writes we will probably have to cast unsafely to get this to work. TVars just hold
        /// arbitrary types and we only have one table.
        /// XXX: gross
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
        fn validate(&mut self, glob : &'g GlobalState) -> STM::Result<()> {
            loop {
                let time = glob.version.load(Ordering::SeqCst);
            }
            STM::Result::abort()
        }
    }


    struct TVar<'a,A> {
        version : AtomicUsize,
        val : UnsafeCell<A>,
        global : &'a GlobalState
    }

    impl<'a, A> TVar<'a, A> {
        unsafe fn getVal(&self) -> &mut A {
            &mut *self.val.get()
        }
    }
}

#[test]
fn it_works() {
}
