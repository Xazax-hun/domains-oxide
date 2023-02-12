use core::cmp::Ordering;
use core::fmt::Debug;
use core::hash::Hash;
use core::ops::{Add, Deref, DerefMut, Neg, Sub};
use std::collections::HashSet;

use fixedbitset::FixedBitSet;

/////////////////////////
// Traits for domains. //
/////////////////////////

/// A join semi-lattice is a partially ordered set that where the least upper
/// bound exists for every subset. Usually, the ordering relation can be viewed
/// as "safe approximation". For example, the interval \[4, 7\] is a safe
/// approximation of \[5, 6\]. The goal of abstract interpretation is to calculate
/// a precise but safe approximation of the program behavior. In this library,
/// Top represents the biggest element (largest approximation), Bottom represents
/// the smallest one.
pub trait JoinSemiLattice: Eq + PartialOrd + Clone + Debug {
    /// A type to hold some information about the lattice on the side.
    ///
    /// For some lattices, like the power set lattice, we need to
    /// store somewhere the top or the bottom value. When we need
    /// no such values, set this to unit.
    type LatticeContext;

    /// The unit element of the join operation. Strictly speaking a join
    /// semi-lattice does not need to have a bottom element, but having one
    /// makes certain computations simpler. In case a domain lacks a bottom
    /// element consider using the [Option] transformer to introduce one.
    /// Bottom values in the analysis result often stand for dead code.
    ///
    /// Required to be the smallest element according to the ordering.
    fn bottom(ctx: &Self::LatticeContext) -> Self;

    /// Given two elements of the lattice the join operation will compute a
    /// precise and safe over approximation of its arguments. It computes
    /// the least upper bound. It is typically useful to calculate the
    /// analysis state after merge points where the program location after
    /// the branching needs to over approximate all predecessors.
    ///
    /// Requirements:
    /// * Reflexive: a.join(a) == a
    /// * Commutative: a.join(b) == b.join(a)
    /// * Bottom is unit: bottom.join(b) == b
    /// * Upper bound: a.join(b) >= a and a.join(b) >= b
    /// * Top is the largest: top.join(b) == top
    /// * Ordering is respected: a <= b => a.join(b) == b
    fn join(&self, other: &Self) -> Self;

    /// In case a lattice has infinite (or very long) ascending chains,
    /// the widening operation can ensure convergence. Other lattices
    /// can use the default implementation. Widening is a larger
    /// over approximation step, usually by removing certain constraints
    /// that did not stabilize from a numerical domain. The analysis
    /// state from the previous iteration can inform this operation what
    /// parts of the state needs widened. Widening also gets an iteration
    /// number that is an approximation of the number iterations over
    /// the whole control flow graph. This number can be helpful to
    /// implement a tiered widening, where later tiers are approximating
    /// more aggressively.
    ///
    /// Requirements:
    /// * Reflexive: a.widen(a, x, i) == a
    /// * b.widen(a, x, i) == b if a <= b
    fn widen(&self, _previous: &Self, _iteration: usize) -> Self {
        self.clone()
    }
}

/// A lattice is a join semi-lattice that is also a meet semi-lattice, i.e.,
/// the greatest lower bound (meet) also exists for all subsets.
pub trait Lattice: JoinSemiLattice {
    /// The unit element of the meet operation, the largest element of the
    /// lattice.
    ///
    /// Requirements:
    /// Top is the greatest element of the lattice.
    fn top(ctx: &Self::LatticeContext) -> Self;

    /// Given two elements of the lattice the meet operation will compute the
    /// greatest lower bound. This is usually useful to exclude infeasible
    /// program states. Often used to implement the evaluation of conditions
    /// or assertions.
    ///
    /// * Reflexive: a.meet(a) == a
    /// * Commutative: a.meet(b) == b.meet(a)
    /// * Top is unit: top.meet(b) == b
    /// * Lower bound: a.meet(b) <= a and a.meet(b) <= b
    /// * Bottom is the smallest: bottom.meet(b) == bottom
    /// * Ordering is respected: a <= b => a.meet(b) == a
    // TODO: add algebraic identities.
    fn meet(&self, other: &Self) -> Self;

    // TODO: consider a narrow operation as the opposite of widen for lattices
    //       that have infinite descending chains.
}

// TODO: add helper to compute meet/join of a set of lattice elements.

/////////////////////////////////////
// Concrete domain implementations //
/////////////////////////////////////

/// The unit lattice is useful for testing, as a placeholder,
/// or as a building block in one of the lattice construction
/// methods (transformers) like the product lattice.
impl JoinSemiLattice for () {
    type LatticeContext = ();

    fn bottom(_: &Self::LatticeContext) -> Self {}

    fn join(&self, _: &Self) -> Self {}
}

impl Lattice for () {
    fn top(_: &Self::LatticeContext) -> Self {}

    fn meet(&self, _: &Self) -> Self {}
}

/// Bool is a lattice, where false is bottom and true is top,
/// join is or, meet is and.
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

/// The set of natural numbers is a join semi-lattice, where
/// 0 is the largest element and minimum is the join operation.
impl JoinSemiLattice for u64 {
    type LatticeContext = ();

    fn bottom(_ctx: &Self::LatticeContext) -> Self {
        u64::MIN
    }

    fn join(&self, other: &Self) -> Self {
        *self.min(other)
    }
}

/// In the power set lattice, the empty set is bottom, union is join
/// intersect is meet, and the full set is top. Note that, we rarely
/// need a general power set lattice. Usually, we can get a more
/// efficient implementation by using a bit set lattice by creating
/// a mapping between the natural numbers and the elements of the
/// set.
#[derive(PartialEq, Eq, Clone)]
pub struct PowerSetDomain<T: Eq + Hash>(pub HashSet<T>);

impl<T: Eq + Hash> Deref for PowerSetDomain<T> {
    type Target = HashSet<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Eq + Hash> DerefMut for PowerSetDomain<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct PowerSetTop<T: Eq + Hash>(pub PowerSetDomain<T>);

impl<T: Eq + Hash> PartialOrd for PowerSetDomain<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self.is_superset(other), other.is_superset(self)) {
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
        Self(self.union(other).cloned().collect::<HashSet<T>>())
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
        Self(self.intersection(other).cloned().collect::<HashSet<T>>())
    }
}

/// An efficient implementation of a power set lattice. Use this over
/// the PowerSetDomain whenever possible.
#[derive(PartialEq, Eq, Clone)]
pub struct BitSetDomain(pub FixedBitSet);

impl Deref for BitSetDomain {
    type Target = FixedBitSet;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for BitSetDomain {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

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
        match (self.is_superset(other), other.is_superset(self)) {
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
            self.ones()
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
        let mut result = self.clone();
        result.union_with(other);
        result
    }
}

impl Lattice for BitSetDomain {
    fn top(ctx: &Self::LatticeContext) -> Self {
        let mut result = FixedBitSet::with_capacity(ctx.0);
        result.toggle_range(..);
        Self(result)
    }

    fn meet(&self, other: &Self) -> Self {
        let mut result = self.clone();
        result.intersect_with(other);
        result
    }
}

mod numerical;
pub use numerical::*;

mod transformers;
pub use transformers::*;

#[cfg(test)]
mod domains_tests;
