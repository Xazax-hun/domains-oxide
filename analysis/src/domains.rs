use std::cmp::Ordering;
use std::fmt::Display;

pub trait Domain: Eq + PartialOrd + Display {
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

// TODO:
// Add more general building blocks for finite domains and
// post SignDomain to those facilities.

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

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
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
