//! This crate contains a set of helpers to build static analysis tools based
//! on abstract interpretation. The building blocks include helpers for
//! control flow graphs, creating lattice domains, and common analyses like
//! calculating the dominator sets. There are also a set of concrete lattice
//! implementations like bitset lattice, map lattice, and sign lattice.
//!
//! In the future, I plan to extend this crate with more elaborate numerical
//! domains, as well as helpers to build analyses based on symbolic execution.
//!
//! Look at the domains-lib crate for an example how to define analyses using
//! the helpers in this crate.

/// Collection of commonly used analyses like dominator sets. Most of these
/// are independent of the actual operations, only based on the shape of the
/// control flow graph.
pub mod analyses;

/// Trait for defining a control flow graph, and some algorithms and data
/// structures to make it easier to work with them.
pub mod cfg;

/// A curated collection of semi-lattices and lattices, including some
/// transformers to help building larger lattices from smaller ones.
pub mod domains;

/// Implementations of fixed-point iteration algorithms using worklists.
pub mod solvers;

#[cfg(test)]
mod cfg_tests;

#[cfg(test)]
mod analyses_tests;
