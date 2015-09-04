//#![feature(raw, slice_bytes)]

use std::result::Result as Either;
use std::mem::{transmute, forget, uninitialized};
use std::ptr::swap;
/// We cannot make a real monad, so this is going to be a fake linear type a la Clean.
pub struct Result<A>(Either<A, ()>);

pub mod no_rec;

impl<A> Result<A> {
    pub fn ret(a: A) -> Result<A> {
        Result(Either::Ok(a))
    }

    pub fn abort() -> Result<A> {
        Result(Either::Err(()))
    }

    /// just don't look
    /// TODO: use a different linear-types library. Currently using this
    /// in calling code
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

#[test]
fn it_works() {
}
