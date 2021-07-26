#![warn(clippy::all)]
#![allow(clippy::clone_double_ref)]

#[cfg(test)]
extern crate test_case;

extern crate num_bigint;
extern crate inkwell;
extern crate rustpython_parser;
extern crate indoc;

mod typecheck;

