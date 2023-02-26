#[macro_use]
extern crate lazy_static;

pub mod eval;
pub mod ir;
pub mod lexer;
pub mod parser;

#[cfg(test)]
mod lexer_tests;
