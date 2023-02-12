use crate::domains::*;

/// Lattice to represent the signs of integral values:
/// ```txt
///     Top
///   /  |  \
///   N  Z  P
///   \  |  /
///    Bottom
/// ```
/// A small lattice that lends itself to a fast, efficient analysis.
/// For more precision, consider using [`IntervalDomain`].
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

impl From<IntervalDomain> for SignDomain {
    fn from(value: IntervalDomain) -> Self {
        if value == IntervalDomain::bottom(&()) {
            return SignDomain::Bottom;
        }
        if value == IntervalDomain::from(0) {
            return SignDomain::Zero;
        }
        if value.max < 0 {
            return SignDomain::Negative;
        }
        if value.min > 0 {
            return SignDomain::Positive;
        }
        SignDomain::Top
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

mod sign_tables {
    use super::SignDomain::{self, *};
    pub fn index_of(s: SignDomain) -> usize {
        match s {
            SignDomain::Top => 0,
            SignDomain::Bottom => 1,
            SignDomain::Negative => 2,
            SignDomain::Zero => 3,
            SignDomain::Positive => 4,
        }
    }

    #[rustfmt::skip]
    pub const ADDITION : [[SignDomain; 5]; 5] =
    [
        // LHS/RHS,      Top,    Bottom, Negative, Zero,     Positive
        /* Top      */ [ Top,    Bottom, Top,      Top,      Top     ],
        /* Bottom   */ [ Bottom, Bottom, Bottom,   Bottom,   Bottom  ],
        /* Negative */ [ Top,    Bottom, Negative, Negative, Top     ],
        /* Zero     */ [ Top,    Bottom, Negative, Zero,     Positive],
        /* Positive */ [ Top,    Bottom, Top,      Positive, Positive]
    ];

    #[rustfmt::skip]
    pub const MULTIPLICATION : [[SignDomain; 5]; 5] =
    [
        // LHS/RHS,      Top,    Bottom, Negative, Zero,     Positive
        /* Top      */ [ Top,    Bottom, Top,      Zero,     Top     ],
        /* Bottom   */ [ Bottom, Bottom, Bottom,   Bottom,   Bottom  ],
        /* Negative */ [ Top,    Bottom, Positive, Zero,     Negative],
        /* Zero     */ [ Zero,   Bottom, Zero,     Zero,     Zero    ],
        /* Positive */ [ Top,    Bottom, Negative, Zero,     Positive]
    ];
}

impl Add for SignDomain {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        use sign_tables::*;
        ADDITION[index_of(self)][index_of(rhs)]
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

impl Sub for SignDomain {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        self + -rhs
    }
}

impl Mul for SignDomain {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        use sign_tables::*;
        MULTIPLICATION[index_of(self)][index_of(rhs)]
    }
}

impl Div for SignDomain {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        use sign_tables::*;
        MULTIPLICATION[index_of(self)][index_of(rhs)]
    }
}


pub const INF: i64 = i64::MAX;
pub const NEG_INF: i64 = i64::MIN;

/// [`IntervalDomain`] is often used to represent a possible range of values.
/// The lattice is ordered by inclusion and has very long ascending and
/// descending chains, thus it implements widening. It also implements
/// some basic arithmetic operations.
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct IntervalDomain {
    pub min: i64,
    pub max: i64,
}

impl From<i64> for IntervalDomain {
    fn from(val: i64) -> Self {
        Self { min: val, max: val }
    }
}

impl Debug for IntervalDomain {
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

    fn widen(&self, prev: &Self, _: usize) -> Self {
        if *prev == Self::bottom(&()) {
            return *self;
        }
        Self {
            min: if prev.min > self.min {
                NEG_INF
            } else {
                self.min
            },
            max: if prev.max < self.max { INF } else { self.max },
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
            Self::bottom(&())
        } else {
            result
        }
    }
}

impl From<SignDomain> for IntervalDomain {
    fn from(value: SignDomain) -> Self {
        match value {
            SignDomain::Top => IntervalDomain::top(&()),
            SignDomain::Bottom => IntervalDomain::bottom(&()),
            SignDomain::Zero => IntervalDomain::from(0),
            SignDomain::Positive => IntervalDomain { min: 1, max: INF },
            SignDomain::Negative => IntervalDomain {
                min: NEG_INF,
                max: -1,
            },
        }
    }
}

impl Add<IntervalDomain> for IntervalDomain {
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

impl Neg for IntervalDomain {
    type Output = Self;

    fn neg(self) -> Self {
        Self {
            min: if self.max == INF { NEG_INF } else { -self.max },
            max: if self.min == NEG_INF { INF } else { -self.min },
        }
    }
}

impl Sub for IntervalDomain {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self + -rhs
    }
}

// TODO: add other arithmetic operations.

// TODO: add congruence domain.
// TODO: add relational domains like octagons and polyhedra.
