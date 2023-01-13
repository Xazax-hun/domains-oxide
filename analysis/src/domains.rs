use core::cmp::Ordering;
use std::collections::HashSet;
use core::fmt::Display;
use core::hash::Hash;
use core::ops::Add;
use core::ops::Neg;

use fixedbitset::FixedBitSet;

/////////////////////////
// Traits for domains. //
/////////////////////////

pub trait JoinSemiLattice: Eq + PartialOrd + Clone + Display {
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

// TODO:
// Add more general building blocks for finite domains and
// port SignDomain to those facilities.

#[derive(PartialEq, Eq, PartialOrd, Clone, Copy)]
pub struct UnitDomain;

impl Display for UnitDomain {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "UnitDomain")
    }
}

impl JoinSemiLattice for UnitDomain {
    type LatticeContext = ();

    fn bottom(_: &Self::LatticeContext) -> Self {
        Self
    }

    fn join(&self, _: &Self) -> Self {
        Self
    }
}

impl Lattice for UnitDomain {
    fn top(_: &Self::LatticeContext) -> Self {
        Self
    }

    fn meet(&self, _: &Self) -> Self {
        Self
    }
}

///     Top
///   /  |  \
///   N  Z  P
///   \  |  /
///    Bottom
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum SignDomain {
    Top,
    Bottom,
    Negative,
    Zero,
    Positive,
}

impl From<i32> for SignDomain {
    fn from(val: i32) -> Self {
        match val.cmp(&0) {
            Ordering::Less => SignDomain::Negative,
            Ordering::Equal => SignDomain::Zero,
            Ordering::Greater => SignDomain::Positive,
        }
    }
}

impl Display for SignDomain {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl PartialOrd for SignDomain {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            return Some(Ordering::Equal);
        }
        match other {
            SignDomain::Bottom => return Some(Ordering::Greater),
            SignDomain::Top => return Some(Ordering::Less),
            _ => {}
        }
        match self {
            SignDomain::Bottom => Some(Ordering::Less),
            SignDomain::Top => Some(Ordering::Greater),
            _ => None,
        }
    }
}

impl JoinSemiLattice for SignDomain {
    type LatticeContext = ();

    fn bottom(_: &Self::LatticeContext) -> Self {
        SignDomain::Bottom
    }

    fn join(&self, other: &Self) -> Self {
        if self == other || *other == SignDomain::Bottom {
            return *self;
        }

        if *self == SignDomain::Bottom {
            return *other;
        }

        SignDomain::Top
    }
}

impl Lattice for SignDomain {
    fn top(_: &Self::LatticeContext) -> Self {
        SignDomain::Top
    }

    fn meet(&self, other: &Self) -> Self {
        if self == other || *other == SignDomain::Top {
            return *self;
        }

        if *self == SignDomain::Top {
            return *other;
        }

        SignDomain::Bottom
    }
}

impl Add for SignDomain {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        use SignDomain::*;
        // TODO: can we format this as a table?
        match (self, rhs) {
            (Top, _) | (_, Top) => Top,
            (Bottom, _) | (_, Bottom) => Bottom,
            (Zero, s) | (s, Zero) => s,
            (s1, s2) if s1 == s2 => s1,
            _ => Top,
        }
    }
}

impl Neg for SignDomain {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            SignDomain::Negative => SignDomain::Positive,
            SignDomain::Positive => SignDomain::Negative,
            _ => self,
        }
    }
}

pub const INF: i64 = i64::MAX;
pub const NEG_INF: i64 = i64::MIN;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct IntervalDomain {
    pub min: i64,
    pub max: i64,
}

impl From<i32> for IntervalDomain {
    fn from(val: i32) -> Self {
        Self {
            min: val.into(),
            max: val.into(),
        }
    }
}

impl Display for IntervalDomain {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let to_str = |x: i64| match x {
            INF => "inf".to_owned(),
            NEG_INF => "-inf".to_owned(),
            _ => x.to_string(),
        };
        write!(f, "[{}, {}]", to_str(self.min), to_str(self.max))
    }
}

impl PartialOrd for IntervalDomain {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            return Some(Ordering::Equal);
        }
        if self.min <= other.min && self.max >= other.max {
            return Some(Ordering::Greater);
        }
        if self.min >= other.min && self.max <= other.max {
            return Some(Ordering::Less);
        }

        None
    }
}

impl JoinSemiLattice for IntervalDomain {
    type LatticeContext = ();

    fn bottom(_: &Self::LatticeContext) -> Self {
        Self {
            min: INF,
            max: NEG_INF,
        }
    }

    fn join(&self, other: &Self) -> Self {
        Self {
            min: self.min.min(other.min),
            max: self.max.max(other.max),
        }
    }

    fn widen(&self, transferred_state: &Self, _: usize) -> Self {
        if *self == IntervalDomain::bottom(&()) {
            return *transferred_state;
        }
        Self {
            min: if transferred_state.min < self.min {
                NEG_INF
            } else {
                self.min
            },
            max: if transferred_state.max > self.max {
                INF
            } else {
                self.max
            },
        }
    }
}

impl Lattice for IntervalDomain {
    fn top(_: &Self::LatticeContext) -> Self {
        Self {
            min: NEG_INF,
            max: INF,
        }
    }

    fn meet(&self, other: &Self) -> Self {
        let result = IntervalDomain {
            min: self.min.max(other.min),
            max: self.max.min(other.max),
        };

        // We only want one canonical representation for bottom.
        if result.min > result.max {
            IntervalDomain::bottom(&())
        } else {
            result
        }
    }
}

impl core::ops::Add<IntervalDomain> for IntervalDomain {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        // Cannot do arithmetic on bottom.
        assert!(self.min != INF && rhs.min != INF);
        assert!(self.max != NEG_INF && rhs.max != NEG_INF);
        Self {
            min: if self.min == NEG_INF || rhs.min == NEG_INF {
                NEG_INF
            } else {
                self.min + rhs.min
            },
            max: if self.max == INF || rhs.max == INF {
                INF
            } else {
                self.max + rhs.max
            },
        }
    }
}

impl core::ops::Neg for IntervalDomain {
    type Output = Self;

    fn neg(self) -> Self {
        Self {
            min: if self.max == INF { NEG_INF } else { -self.max },
            max: if self.min == NEG_INF { INF } else { -self.min },
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
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

impl<T: Eq + Hash + Display> Display for PowerSetDomain<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.0
                .iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl<T: Eq + Hash + Display + Clone> JoinSemiLattice for PowerSetDomain<T> {
    type LatticeContext = PowerSetTop<T>;

    fn bottom(_: &Self::LatticeContext) -> Self {
        Self(HashSet::new())
    }

    fn join(&self, other: &Self) -> Self {
        Self(self.0.union(&other.0).cloned().collect::<HashSet<T>>())
    }
}

impl<T: Eq + Hash + Display + Clone> Lattice for PowerSetDomain<T> {
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

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct BitSetDomain(pub FixedBitSet);

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

impl Display for BitSetDomain {
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

/////////////////////////
// Domain transformers //
/////////////////////////

#[derive(PartialEq, Eq, PartialOrd, Clone, Debug)]
pub struct Vec2Domain<T: JoinSemiLattice> {
    pub x: T,
    pub y: T,
}

impl<T: JoinSemiLattice> Display for Vec2Domain<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{{ x: {}, y: {} }}", self.x, self.y)
    }
}

impl<T: JoinSemiLattice> JoinSemiLattice for Vec2Domain<T> {
    type LatticeContext = T::LatticeContext;

    fn bottom(ctx: &Self::LatticeContext) -> Self {
        Self {
            x: T::bottom(ctx),
            y: T::bottom(ctx),
        }
    }

    fn join(&self, other: &Self) -> Self {
        Self {
            x: self.x.join(&other.x),
            y: self.y.join(&other.y),
        }
    }

    fn widen(&self, other: &Self, iteration: usize) -> Self {
        Self {
            x: self.x.widen(&other.x, iteration),
            y: self.y.widen(&other.y, iteration),
        }
    }
}

impl<T: Lattice> Lattice for Vec2Domain<T> {
    fn top(ctx: &Self::LatticeContext) -> Self {
        Self {
            x: T::top(ctx),
            y: T::top(ctx),
        }
    }

    fn meet(&self, other: &Self) -> Self {
        Self {
            x: self.x.meet(&other.x),
            y: self.y.meet(&other.y),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Clone, Debug)]
pub struct Flipped<T: Lattice>(pub T);

impl<T: Display + Lattice> Display for Flipped<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T: Lattice> JoinSemiLattice for Flipped<T> {
    type LatticeContext = T::LatticeContext;

    fn bottom(ctx: &Self::LatticeContext) -> Self {
        Self(T::top(ctx))
    }

    fn join(&self, other: &Self) -> Self {
        Self(self.0.meet(&other.0))
    }
}

impl<T: Lattice> Lattice for Flipped<T> {
    fn top(ctx: &Self::LatticeContext) -> Self {
        Self(T::bottom(ctx))
    }

    fn meet(&self, other: &Self) -> Self {
        Self(self.0.join(&other.0))
    }
}

// TODO:
// Add operations to built lattices
// * Product, Pair
// * Reduced product
// * Disjoint union
// * Stacking
// * Lifting
// * Finite lattices
