# Domains Oxide ![Build status](https://github.com/Xazax-hun/domains-oxide/actions/workflows/rust.yml/badge.svg)

This project is an abstract interpretation framework along with some implementations
of small example languages and static analyses to demo its capabilities.
The framework include:
* Traits for common building blocks like control flow graph, lattice
* Numerical domains for abstract interpretation
* Building blocks to create larger lattices from smaller ones
* Common algorithms like dominator sets
* Common data structures like reverse post-order worklist
* Fixed-point iteration-based solvers

For more details, look at the documentation of the `analysis` crate.
For examples, look at the [transform language documentation](transform-driver/README.md)
and its source code.
