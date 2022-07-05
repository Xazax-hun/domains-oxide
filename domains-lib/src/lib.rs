#[macro_use]
extern crate lazy_static;
extern crate rand;

pub mod ast;
pub mod cfg;
pub mod eval;
pub mod lexer;
pub mod parser;
pub mod render;

#[cfg(test)]
mod lexer_tests;

#[cfg(test)]
mod parser_tests;

#[cfg(test)]
mod cfg_tests;

#[cfg(test)]
mod eval_tests;
