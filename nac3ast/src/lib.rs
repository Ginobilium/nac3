#[macro_use]
extern crate lazy_static;

mod ast_gen;
mod constant;
#[cfg(feature = "fold")]
mod fold_helpers;
mod impls;
mod location;

pub use ast_gen::*;
pub use location::{Location, FileName};

pub type Suite<U = ()> = Vec<Stmt<U>>;
