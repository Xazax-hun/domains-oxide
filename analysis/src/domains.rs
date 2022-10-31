use std::cmp::Ordering;
use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;

///////////////////////////
/// Traits for domains. ///
///////////////////////////

pub trait Domain: Eq + PartialOrd + Clone + Display {
    /// Required to be the smallest element according to the ordering.
    fn bottom() -> Self;

    /// Requirements:
    /// * a.join(a) == a
    /// * a.join(b) == b.join(a)
    /// * a.join(b) >= a
    /// * a.join(b) >= b
    /// * top.join(b) == top
    /// * bottom.join(b) == b
    fn join(&self, other: &Self) -> Self;
}

pub trait WidenableDomain: Domain {
    /// Requirements:
    /// * bottom.widen(a) == a
    /// * a.widen(a) == a
    /// * b.widen(a) == b if a <= b
    fn widen(&self, other: &Self) -> Self;
}

pub trait Top: Domain {
    /// Requirements:
    /// Top is the greatest element of the lattice.
    fn top() -> Self;
}

///////////////////////////////////////
/// Concrete domain implementations ///
///////////////////////////////////////

// TODO:
// Add more general building blocks for finite domains and
// post SignDomain to those facilities.

#[derive(PartialEq, Eq, PartialOrd, Clone)]
pub struct UnitDomain;

impl Display for UnitDomain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UnitDomain")
    }
}

impl Domain for UnitDomain {
    fn bottom() -> Self {
        Self
    }

    fn join(&self, _: &Self) -> Self {
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

impl Top for SignDomain {
    fn top() -> Self {
        SignDomain::Top
    }
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
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

impl Domain for SignDomain {
    fn bottom() -> Self {
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

#[derive(PartialEq, Eq, PartialOrd, Clone, Debug)]
pub struct Vec2Domain<T: Domain> {
    pub x: T,
    pub y: T,
}

impl<T: Domain> Display for Vec2Domain<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ x: {}, y: {} }}", self.x, self.y)
    }
}

impl<T: Domain> Domain for Vec2Domain<T> {
    fn bottom() -> Self {
        Vec2Domain {
            x: T::bottom(),
            y: T::bottom(),
        }
    }

    fn join(&self, other: &Self) -> Self {
        Vec2Domain {
            x: self.x.join(&other.x),
            y: self.y.join(&other.y),
        }
    }
}

impl<T: WidenableDomain> WidenableDomain for Vec2Domain<T> {
    fn widen(&self, other: &Self) -> Self {
        Vec2Domain {
            x: self.x.widen(&other.x),
            y: self.y.widen(&other.y),
        }
    }
}

impl<T: Top> Top for Vec2Domain<T> {
    fn top() -> Self {
        Vec2Domain {
            x: T::top(),
            y: T::top(),
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
        IntervalDomain {
            min: val.into(),
            max: val.into(),
        }
    }
}

impl Display for IntervalDomain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

impl Domain for IntervalDomain {
    fn bottom() -> Self {
        IntervalDomain {
            min: INF,
            max: NEG_INF,
        }
    }

    fn join(&self, other: &Self) -> Self {
        IntervalDomain {
            min: self.min.min(other.min),
            max: self.max.max(other.max),
        }
    }
}

impl Top for IntervalDomain {
    fn top() -> Self {
        IntervalDomain {
            min: NEG_INF,
            max: INF,
        }
    }
}

impl WidenableDomain for IntervalDomain {
    fn widen(&self, transferred_state: &Self) -> Self {
        if *self == IntervalDomain::bottom() {
            return *transferred_state;
        }
        IntervalDomain {
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

impl std::ops::Add<IntervalDomain> for IntervalDomain {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        // Cannot do arithmetic on bottom.
        assert!(self.min != INF && rhs.min != INF);
        assert!(self.max != NEG_INF && rhs.max != NEG_INF);
        IntervalDomain {
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

impl std::ops::Neg for IntervalDomain {
    type Output = Self;

    fn neg(self) -> Self {
        IntervalDomain {
            min: if self.max == INF { NEG_INF } else { -self.max },
            max: if self.min == NEG_INF { INF } else { -self.min },
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct PowerSetDomain<T: Eq + Hash>(pub HashSet<T>);

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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.0
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl<T: Eq + Hash + Display + Clone> Domain for PowerSetDomain<T> {
    fn bottom() -> Self {
        Self(HashSet::new())
    }

    fn join(&self, other: &Self) -> Self {
        Self(self.0.union(&other.0).cloned().collect::<HashSet<T>>())
    }
}
