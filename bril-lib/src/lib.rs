#[macro_use]
extern crate lazy_static;

pub mod analysis;
pub mod eval;
pub mod ir;
pub mod lexer;
pub mod parser;

#[cfg(test)]
mod lexer_tests;

#[cfg(test)]
mod parser_tests;

#[cfg(test)]
mod eval_tests;
