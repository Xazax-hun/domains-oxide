use core::cmp::Ordering;
use core::fmt::Debug;
use core::hash::Hash;
use core::ops::Add;
use core::ops::Neg;
use std::collections::HashSet;

use fixedbitset::FixedBitSet;

/////////////////////////
// Traits for domains. //
/////////////////////////

pub trait JoinSemiLattice: Eq + PartialOrd + Clone + Debug {
    // For some lattices, like the power set lattice, we need to
    // store somewhere the top or the bottom value. When we need
    // no such values, set this to unit.
    type LatticeContext;

    /// Required to be the smallest element according to the ordering.
    fn bottom(ctx: &Self::LatticeContext) -> Self;

    /// Requirements:
    /// * a.join(a) == a
    /// * a.join(b) == b.join(a)
    /// * a.join(b) >= a
    /// * a.join(b) >= b
    /// * top.join(b) == top
    /// * bottom.join(b) == b
    fn join(&self, other: &Self) -> Self;

    /// Requirements:
    /// * a.widen(a, x) == a
    /// * b.widen(a, x) == b if a <= b
    fn widen(&self, _previous: &Self, _iteration: usize) -> Self {
        self.clone()
    }
}

pub trait Lattice: JoinSemiLattice {
    /// Requirements:
    /// Top is the greatest element of the lattice.
    fn top(ctx: &Self::LatticeContext) -> Self;

    fn meet(&self, other: &Self) -> Self;
}

/////////////////////////////////////
// Concrete domain implementations //
/////////////////////////////////////

impl JoinSemiLattice for () {
    type LatticeContext = ();

    fn bottom(_: &Self::LatticeContext) -> Self {}

    fn join(&self, _: &Self) -> Self {}
}

impl Lattice for () {
    fn top(_: &Self::LatticeContext) -> Self {}

    fn meet(&self, _: &Self) -> Self {}
}

impl JoinSemiLattice for bool {
    type LatticeContext = ();

    fn bottom(_ctx: &Self::LatticeContext) -> Self {
        false
    }

    fn join(&self, other: &Self) -> Self {
        *self || *other
    }
}

impl Lattice for bool {
    fn top(_ctx: &Self::LatticeContext) -> Self {
        true
    }

    fn meet(&self, other: &Self) -> Self {
        *self && *other
    }
}

impl JoinSemiLattice for u64 {
    type LatticeContext = ();

    fn bottom(_ctx: &Self::LatticeContext) -> Self {
        0
    }

    fn join(&self, other: &Self) -> Self {
        *self.min(other)
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct PowerSetDomain<T: Eq + Hash>(pub HashSet<T>);

pub struct PowerSetTop<T: Eq + Hash>(pub PowerSetDomain<T>);

impl<T: Eq + Hash> PartialOrd for PowerSetDomain<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self.0.is_superset(&other.0), other.0.is_superset(&self.0)) {
            (true, true) => Some(Ordering::Equal),
            (true, false) => Some(Ordering::Greater),
            (false, true) => Some(Ordering::Less),
            (_, _) => None,
        }
    }
}

impl<T: Eq + Hash + Debug> Debug for PowerSetDomain<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut elements = self
            .0
            .iter()
            .map(|x| format!("{x:?}"))
            .collect::<Vec<String>>();
        elements.sort();
        write!(f, "{{{}}}", elements.join(", "))
    }
}

impl<T: Eq + Hash + Debug + Clone> JoinSemiLattice for PowerSetDomain<T> {
    type LatticeContext = PowerSetTop<T>;

    fn bottom(_: &Self::LatticeContext) -> Self {
        Self(HashSet::new())
    }

    fn join(&self, other: &Self) -> Self {
        Self(self.0.union(&other.0).cloned().collect::<HashSet<T>>())
    }
}

impl<T: Eq + Hash + Debug + Clone> Lattice for PowerSetDomain<T> {
    // TODO: some impls want to return a value, some want to
    //       return a reference. Try to make the trait flexible,
    //       e.g.: https://stackoverflow.com/questions/43323250/trait-method-that-can-be-implemented-to-either-return-a-reference-or-an-owned-va
    fn top(ctx: &Self::LatticeContext) -> Self {
        ctx.0.clone()
    }

    fn meet(&self, other: &Self) -> Self {
        Self(
            self.0
                .intersection(&other.0)
                .cloned()
                .collect::<HashSet<T>>(),
        )
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct BitSetDomain(pub FixedBitSet);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BitSetTop(pub usize);

impl BitSetDomain {
    pub fn from(ctx: &BitSetTop, values: &[usize]) -> Self {
        let mut inner = FixedBitSet::with_capacity(ctx.0);
        for &v in values {
            inner.insert(v);
        }
        Self(inner)
    }
}

impl PartialOrd for BitSetDomain {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self.0.is_superset(&other.0), other.0.is_superset(&self.0)) {
            (true, true) => Some(Ordering::Equal),
            (true, false) => Some(Ordering::Greater),
            (false, true) => Some(Ordering::Less),
            (_, _) => None,
        }
    }
}

impl Debug for BitSetDomain {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.0
                .ones()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl JoinSemiLattice for BitSetDomain {
    type LatticeContext = BitSetTop;

    fn bottom(ctx: &Self::LatticeContext) -> Self {
        Self(FixedBitSet::with_capacity(ctx.0))
    }

    fn join(&self, other: &Self) -> Self {
        let mut result = self.0.clone();
        result.union_with(&other.0);
        Self(result)
    }
}

impl Lattice for BitSetDomain {
    fn top(ctx: &Self::LatticeContext) -> Self {
        let mut result = FixedBitSet::with_capacity(ctx.0);
        result.toggle_range(..);
        Self(result)
    }

    fn meet(&self, other: &Self) -> Self {
        let mut result = self.0.clone();
        result.intersect_with(&other.0);
        Self(result)
    }
}

mod numerical;
pub use numerical::*;

mod transformers;
pub use transformers::*;

#[cfg(test)]
mod domains_tests;
