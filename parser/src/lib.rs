#![allow(dead_code)] // TODO: remove

#[macro_use]
extern crate lazy_static;

mod ast;
mod lexer;
mod parser;

#[cfg(test)]
mod lexer_tests;

#[cfg(test)]
mod parser_tests;
