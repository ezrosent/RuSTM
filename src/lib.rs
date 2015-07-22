use std::result::Result as Either;

/// We cannot make a real monad, so this is going to be a fake linear type a la Clean.
pub struct Result<A>(Either<A, ()>);

impl<A> Result<A> {
    pub fn ret(a: A) -> Result<A> {
        Result(Either::Ok(a))
    }

    pub fn abort() -> Result<A> {
        Result(Either::Err(()))
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

    fn read(&mut self, &Self::TVar) -> A;

    fn write(&mut self, &Self::TVar, A);
}

#[test]
fn it_works() {
}
