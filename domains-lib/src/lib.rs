#[macro_use]
extern crate lazy_static;

pub mod ast;
pub mod cfg;
pub mod lexer;
pub mod parser;

#[cfg(test)]
mod lexer_tests;

#[cfg(test)]
mod parser_tests;

#[cfg(test)]
mod cfg_tests;
