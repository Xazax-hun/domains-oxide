//! This crate contains a set of helpers to build static analysis tools based
//! on [abstract interpretation](https://en.wikipedia.org/wiki/Abstract_interpretation).
//! The building blocks include helpers for
//! [control flow graphs](https://en.wikipedia.org/wiki/Control-flow_graph),
//! creating [lattice](https://en.wikipedia.org/wiki/Lattice_(order)) domains,
//! and common analyses like
//! calculating the [dominator sets](https://en.wikipedia.org/wiki/Dominator_(graph_theory)).
//! There are also a set of concrete lattice
//! implementations like bitset lattice, map lattice, and sign lattice.
//!
//! In the future, I plan to extend this crate with more elaborate numerical
//! domains, as well as helpers to build analyses based on symbolic execution.
//!
//! Look at the domains-lib crate for an example how to define analyses using
//! the helpers in this crate.
//! 
//! Some resources to learn more about abstract interpretation:
//! * [Static Program Analysis, Anders Møller and Michael I. Schwartzbach](https://cs.au.dk/~amoeller/spa/)
//! * [Introduction to Static Analysis, Xavier Rival and Kwangkeun Yi](https://mitpress.mit.edu/9780262043410/introduction-to-static-analysis/)
//! * [Principles of Abstract Interpretation](https://mitpress.mit.edu/9780262044905/principles-of-abstract-interpretation/)
//! * [Data Flow Analysis: Theory and Practice](https://www.amazon.com/Data-Flow-Analysis-Theory-Practice/dp/0849328802)
//! * [Data flow analysis: an informal introduction](https://clang.llvm.org/docs/DataFlowAnalysisIntro.html)
//! 
//! Other libraries to help implement static analysis tools:
//! * [APRON](https://antoinemine.github.io/Apron/doc/)
//! * [ELINA](http://elina.ethz.ch/)
//! * [SPARTA](https://github.com/facebook/SPARTA)
//! 
//! Solvers:
//! * [z3](https://microsoft.github.io/z3guide/)
//! * [CVC5](https://cvc5.github.io/)
//! * [Souffle](https://souffle-lang.github.io/)
//! 
//! Frameworks:
//! * [IKOS](https://github.com/NASA-SW-VnV/ikos)
//! * [PHASAR](https://phasar.org/)
//! * [Infer](https://fbinfer.com/)
//! * [cclyzer++](https://github.com/GaloisInc/cclyzerpp)
//! * [MATE](https://galoisinc.github.io/MATE/)

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
