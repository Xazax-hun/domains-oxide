use crate::domains::*;

/// A small lattice that lends itself to a fast, efficient analysis.
/// It represents the signs of integral values:
/// ```txt
///       Top
///      / | \
///     /  |  \
///  ~Pos  ~Z  ~Neg
///    | \ /\ / |
///    |  X  X  |
///    | / \/ \ |
///   Neg  Z   Pos
///     \  |  /
///      Bottom
/// ```
/// For more precision, consider using [`IntervalDomain`].
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum SignDomain {
    Top,
    Bottom,
    Negative,
    Zero,
    Positive,
    NonNeg,
    NonZero,
    NonPos,
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
        if value.max <= 0 {
            return SignDomain::NonPos;
        }
        if value.min > 0 {
            return SignDomain::Positive;
        }
        if value.min >= 0 {
            return SignDomain::NonNeg;
        }
        SignDomain::Top
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
            SignDomain::NonNeg => 5,
            SignDomain::NonZero => 6,
            SignDomain::NonPos => 7,
        }
    }

    #[rustfmt::skip]
    pub const JOIN : [[SignDomain; 8]; 8] =
    [
    // LHS/RHS,      Top, Bottom,   Negative, Zero,   Positive  ~Negative  ~Zero    ~Positive
    /* Top      */ [ Top, Top,      Top,      Top,    Top,      Top,       Top,     Top   ],
    /* Bottom   */ [ Top, Bottom,   Negative, Zero,   Positive, NonNeg,    NonZero, NonPos],
    /* Negative */ [ Top, Negative, Negative, NonPos, NonZero,  Top,       NonZero, NonPos],
    /* Zero     */ [ Top, Zero,     NonPos,   Zero,   NonNeg,   NonNeg,    Top,     NonPos],
    /* Positive */ [ Top, Positive, NonZero,  NonNeg, Positive, NonNeg,    NonZero, Top   ],
    /* ~Negative*/ [ Top, NonNeg,   Top,      NonNeg, NonNeg,   NonNeg,    Top,     Top   ],
    /* ~Zero    */ [ Top, NonZero,  NonZero,  Top,    NonZero,  Top,       NonZero, Top   ],
    /* ~Positive*/ [ Top, NonPos,   NonPos,   NonPos, Top,      Top,       Top,     NonPos],
    ];

    #[rustfmt::skip]
    pub const MEET : [[SignDomain; 8]; 8] =
    [
    // LHS/RHS,      Top,      Bottom, Negative, Zero,   Positive  ~Negative ~Zero     ~Positive
    /* Top      */ [ Top,      Bottom, Negative, Zero,   Positive, NonNeg,   NonZero,  NonPos  ],
    /* Bottom   */ [ Bottom,   Bottom, Bottom,   Bottom, Bottom,   Bottom,   Bottom,   Bottom  ],
    /* Negative */ [ Negative, Bottom, Negative, Bottom, Bottom,   Bottom,   Negative, Negative],
    /* Zero     */ [ Zero,     Bottom, Bottom,   Zero,   Bottom,   Zero,     Bottom,   Zero    ],
    /* Positive */ [ Positive, Bottom, Bottom,   Bottom, Positive, Positive, Positive, Bottom  ],
    /* ~Negative*/ [ NonNeg,   Bottom, Bottom,   Zero,   Positive, NonNeg,   Positive, Zero    ],
    /* ~Zero    */ [ NonZero,  Bottom, Negative, Bottom, Positive, Positive, NonZero,  Negative],
    /* ~Positive*/ [ NonPos,   Bottom, Negative, Zero,   Bottom,   Zero,     Negative, NonPos  ],
    ];

    #[rustfmt::skip]
    pub const ADDITION : [[SignDomain; 8]; 8] =
    [
    // LHS/RHS,      Top,    Bottom, Negative, Zero,     Positive  ~Negative  ~Zero    ~Positive
    /* Top      */ [ Top,    Bottom, Top,      Top,      Top ,     Top,       Top,     Top     ],
    /* Bottom   */ [ Bottom, Bottom, Bottom,   Bottom,   Bottom,   Bottom,    Bottom,  Bottom  ],
    /* Negative */ [ Top,    Bottom, Negative, Negative, Top,      Top,       Top,     Negative],
    /* Zero     */ [ Top,    Bottom, Negative, Zero,     Positive, NonNeg,    NonZero, NonPos  ],
    /* Positive */ [ Top,    Bottom, Top,      Positive, Positive, Positive,  Top,     Top     ],
    /* ~Negative*/ [ Top,    Bottom, Top,      NonNeg,   Positive, NonNeg,    Top,     Top     ],
    /* ~Zero    */ [ Top,    Bottom, Top,      NonZero,  Top,      Top,       Top,     Top     ],
    /* ~Positive*/ [ Top,    Bottom, Negative, NonPos,   Top,      Top,       Top,     NonPos  ]
    ];

    #[rustfmt::skip]
    pub const MULTIPLICATION : [[SignDomain; 8]; 8] =
    [
    // LHS/RHS,      Top,    Bottom, Negative, Zero,   Positive  ~Negative  ~Zero    ~Positive
    /* Top      */ [ Top,    Bottom, Top,      Zero,   Top,      Top,       Top,     Top   ],
    /* Bottom   */ [ Bottom, Bottom, Bottom,   Bottom, Bottom,   Bottom,    Bottom,  Bottom],
    /* Negative */ [ Top,    Bottom, Positive, Zero,   Negative, NonPos,    NonZero, NonNeg],
    /* Zero     */ [ Zero,   Bottom, Zero,     Zero,   Zero,     Zero,      Zero,    Zero  ],
    /* Positive */ [ Top,    Bottom, Negative, Zero,   Positive, NonNeg,    NonZero, NonPos],
    /* ~Negative*/ [ Top,    Bottom, NonPos,   Zero,   NonNeg,   NonNeg,    Top,     NonPos],
    /* ~Zero    */ [ Top,    Bottom, NonZero,  Zero,   NonZero,  Top,       NonZero, Top   ],
    /* ~Positive*/ [ Top,    Bottom, NonNeg,   Zero,   NonPos,   NonPos,    Top,     NonNeg],
    ];

    use core::cmp::Ordering::{self, *};

    #[rustfmt::skip]
    pub const COMPARISON : [[Option<Ordering>; 8]; 8] =
    [
    // LHS/RHS,      Top,         Bottom,        Negative,      Zero,          Positive       ~Negative      ~Zero          ~Positive
    /* Top      */ [ Some(Equal), Some(Greater), Some(Greater), Some(Greater), Some(Greater), Some(Greater), Some(Greater), Some(Greater)],
    /* Bottom   */ [ Some(Less),  Some(Equal),   Some(Less),    Some(Less),    Some(Less),    Some(Less),    Some(Less),    Some(Less)   ],
    /* Negative */ [ Some(Less),  Some(Greater), Some(Equal),   None,          None,          None,          Some(Less),    Some(Less)   ],
    /* Zero     */ [ Some(Less),  Some(Greater), None,          Some(Equal),   None,          Some(Less),    None,          Some(Less)   ],
    /* Positive */ [ Some(Less),  Some(Greater), None,          None,          Some(Equal),   Some(Less),    Some(Less),    None         ],
    /* ~Negative*/ [ Some(Less),  Some(Greater), None,          Some(Greater), Some(Greater), Some(Equal),   None,          None         ],
    /* ~Zero    */ [ Some(Less),  Some(Greater), Some(Greater), None,          Some(Greater), None,          Some(Equal),   None         ],
    /* ~Positive*/ [ Some(Less),  Some(Greater), Some(Greater), Some(Greater), None,          None,          None,          Some(Equal)  ],
    ];

    #[rustfmt::skip]
    pub const STRICT_CONCRETE_COMPARISON : [[Option<Ordering>; 8]; 8] =
    [
    // LHS/RHS,      Top,  Bottom, Negative,      Zero,          Positive    ~Negative   ~Zero ~Positive
    /* Top      */ [ None, None,   None,          None,          None,       None,       None, None         ],
    /* Bottom   */ [ None, None,   None,          None,          None,       None,       None, None         ],
    /* Negative */ [ None, None,   None,          Some(Less),    Some(Less), Some(Less), None, None         ],
    /* Zero     */ [ None, None,   Some(Greater), Some(Equal),   Some(Less), None,       None, None         ],
    /* Positive */ [ None, None,   Some(Greater), Some(Greater), None,       None,       None, Some(Greater)],
    /* ~Negative*/ [ None, None,   Some(Greater), None,          None,       None,       None, None         ],
    /* ~Zero    */ [ None, None,   None,          None,          None,       None,       None, None         ],
    /* ~Positive*/ [ None, None,   None,          None,          Some(Less), None,       None, None         ],
    ];

    #[rustfmt::skip]
    pub const WEAK_CONCRETE_COMPARISON : [[Option<Ordering>; 8]; 8] =
    [
    // LHS/RHS,      Top,  Bottom, Negative,      Zero,          Positive    ~Negative      ~Zero ~Positive
    /* Top      */ [ None, None,   None,          None,          None,       None,          None, None         ],
    /* Bottom   */ [ None, None,   None,          None,          None,       None,          None, None         ],
    /* Negative */ [ None, None,   None,          Some(Less),    Some(Less), Some(Less),    None, None         ],
    /* Zero     */ [ None, None,   Some(Greater), Some(Equal),   Some(Less), Some(Greater), None, Some(Less)   ],
    /* Positive */ [ None, None,   Some(Greater), Some(Greater), None,       None,          None, Some(Greater)],
    /* ~Negative*/ [ None, None,   Some(Greater), Some(Greater), None,       None,          None, Some(Greater)],
    /* ~Zero    */ [ None, None,   None,          None,          None,       None,          None, None         ],
    /* ~Positive*/ [ None, None,   None,          Some(Less),    Some(Less), Some(Less),    None, None         ],
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
            SignDomain::NonPos => SignDomain::NonNeg,
            SignDomain::NonNeg => SignDomain::NonPos,
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
        use SignDomain::*;
        #[allow(clippy::suspicious_arithmetic_impl)]
        match self * rhs {
            Positive => NonNeg,
            Negative => NonPos,
            res => res,
        }
    }
}

impl PartialOrd for SignDomain {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use sign_tables::*;
        COMPARISON[index_of(*self)][index_of(*other)]
    }
}

impl SignDomain {
    pub fn logical_eq(self, other: SignDomain, is_bool: bool) -> SignDomain {
        if (is_bool && self == SignDomain::Positive && other == SignDomain::Positive)
            || (self == SignDomain::Zero && other == SignDomain::Zero)
        {
            SignDomain::Positive
        } else if self <= other || other <= self {
            SignDomain::NonNeg
        } else {
            SignDomain::Zero
        }
    }

    pub fn logical_and(self, other: SignDomain) -> SignDomain {
        match (self, other) {
            (SignDomain::Zero, _) | (_, SignDomain::Zero) => SignDomain::Zero,
            (SignDomain::Positive, SignDomain::Positive) => SignDomain::Positive,
            _ => SignDomain::NonNeg,
        }
    }

    pub fn logical_or(self, other: SignDomain) -> SignDomain {
        match (self, other) {
            (SignDomain::Positive, _) | (_, SignDomain::Positive) => SignDomain::Positive,
            (SignDomain::Zero, SignDomain::Zero) => SignDomain::Zero,
            _ => SignDomain::NonNeg,
        }
    }

    pub fn logical_not(self) -> SignDomain {
        match self {
            SignDomain::NonNeg => SignDomain::NonNeg,
            SignDomain::Positive => SignDomain::Zero,
            SignDomain::Zero => SignDomain::Positive,
            _ => panic!("Unexpected value"),
        }
    }

    /// Compares the concrete values.
    pub fn strict_cmp(self, other: SignDomain) -> Option<Ordering> {
        use sign_tables::*;
        STRICT_CONCRETE_COMPARISON[index_of(self)][index_of(other)]
    }

    /// Compares the concrete values.
    pub fn weak_cmp(self, other: SignDomain) -> Option<Ordering> {
        use sign_tables::*;
        WEAK_CONCRETE_COMPARISON[index_of(self)][index_of(other)]
    }
}

impl JoinSemiLattice for SignDomain {
    type LatticeContext = ();

    fn bottom(_: &Self::LatticeContext) -> Self {
        SignDomain::Bottom
    }

    fn join(&self, other: &Self, _ctx: &Self::LatticeContext) -> Self {
        use sign_tables::*;
        JOIN[index_of(*self)][index_of(*other)]
    }
}

impl Lattice for SignDomain {
    fn top(_: &Self::LatticeContext) -> Self {
        SignDomain::Top
    }

    fn meet(&self, other: &Self, _ctx: &Self::LatticeContext) -> Self {
        use sign_tables::*;
        MEET[index_of(*self)][index_of(*other)]
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

pub static BOOL_RANGE: IntervalDomain = IntervalDomain { min: 0, max: 1 };
pub static TRUE_RANGE: IntervalDomain = IntervalDomain { min: 1, max: 1 };
pub static FALSE_RANGE: IntervalDomain = IntervalDomain { min: 0, max: 0 };

impl IntervalDomain {
    /// Returns [n, inf].
    pub fn greater(n: i64) -> Self {
        Self { min: n, max: INF }
    }

    /// Returns [-inf, n].
    pub fn smaller(n: i64) -> Self {
        Self {
            min: NEG_INF,
            max: n,
        }
    }

    pub fn singleton(&self) -> Option<i64> {
        if self.min == self.max {
            Some(self.min)
        } else {
            None
        }
    }

    pub fn logical_and(self, other: IntervalDomain) -> Self {
        match (self.singleton(), other.singleton()) {
            (Some(0), _) | (_, Some(0)) => FALSE_RANGE,
            (Some(1), Some(1)) => TRUE_RANGE,
            _ => BOOL_RANGE,
        }
    }

    pub fn logical_or(self, other: IntervalDomain) -> Self {
        match (self.singleton(), other.singleton()) {
            (Some(1), _) | (_, Some(1)) => TRUE_RANGE,
            (Some(0), Some(0)) => FALSE_RANGE,
            _ => BOOL_RANGE,
        }
    }

    pub fn logical_not(self) -> Self {
        match self.singleton() {
            Some(1) => FALSE_RANGE,
            Some(0) => TRUE_RANGE,
            _ => BOOL_RANGE,
        }
    }

    pub fn strict_cmp(self, other: IntervalDomain) -> Option<Ordering> {
        if self == IntervalDomain::bottom(&()) || other == IntervalDomain::bottom(&()) {
            return None;
        }

        if self.max < other.min {
            return Some(Ordering::Less);
        }

        if self.min > other.max {
            return Some(Ordering::Greater);
        }

        match (self.singleton(), other.singleton()) {
            (Some(x), Some(y)) if x == y => Some(Ordering::Equal),
            _ => None,
        }
    }

    pub fn equals(self, other: IntervalDomain) -> IntervalDomain {
        match self.strict_cmp(other) {
            Some(Ordering::Less) | Some(Ordering::Greater) => FALSE_RANGE,
            Some(Ordering::Equal) => TRUE_RANGE,
            _ => BOOL_RANGE,
        }
    }
}

impl From<i64> for IntervalDomain {
    fn from(val: i64) -> Self {
        Self { min: val, max: val }
    }
}

impl From<SignDomain> for IntervalDomain {
    fn from(value: SignDomain) -> Self {
        match value {
            SignDomain::Top | SignDomain::NonZero => IntervalDomain::top(&()),
            SignDomain::Bottom => IntervalDomain::bottom(&()),
            SignDomain::Zero => IntervalDomain::from(0),
            SignDomain::Positive => IntervalDomain { min: 1, max: INF },
            SignDomain::Negative => IntervalDomain {
                min: NEG_INF,
                max: -1,
            },
            SignDomain::NonNeg => IntervalDomain { min: 0, max: INF },
            SignDomain::NonPos => IntervalDomain {
                min: NEG_INF,
                max: 0,
            },
        }
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

    fn join(&self, other: &Self, _ctx: &Self::LatticeContext) -> Self {
        Self {
            min: self.min.min(other.min),
            max: self.max.max(other.max),
        }
    }

    /// Extrapolate unstable bounds to infinity.
    fn widen(&self, prev: &Self, ctx: &Self::LatticeContext, _: usize) -> Self {
        if *prev == Self::bottom(ctx) {
            return *self;
        }
        Self {
            min: if prev.min > self.min {
                NEG_INF
            } else {
                prev.min
            },
            max: if prev.max < self.max { INF } else { prev.max },
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

    fn meet(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
        let result = IntervalDomain {
            min: self.min.max(other.min),
            max: self.max.min(other.max),
        };

        // We only want one canonical representation for bottom.
        if result.min > result.max {
            Self::bottom(ctx)
        } else {
            result
        }
    }

    /// Improve infinite bounds.
    fn narrow(&self, prev: &Self, ctx: &Self::LatticeContext, _: usize) -> Self {
        if *prev == Self::bottom(ctx) {
            return *self;
        }
        Self {
            min: if prev.min == NEG_INF {
                self.min
            } else {
                prev.min
            },
            max: if prev.max == INF { self.max } else { prev.max },
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
                self.min.saturating_add(rhs.min)
            },
            max: if self.max == INF || rhs.max == INF {
                INF
            } else {
                self.max.saturating_add(rhs.max)
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

impl Mul for IntervalDomain {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        // Cannot do arithmetic on bottom.
        assert!(self.min != INF && rhs.min != INF);
        assert!(self.max != NEG_INF && rhs.max != NEG_INF);
        let candidates = [
            self.min.saturating_mul(rhs.min),
            self.min.saturating_mul(rhs.max),
            self.max.saturating_mul(rhs.min),
            self.max.saturating_mul(rhs.max),
        ];
        IntervalDomain {
            min: *candidates.iter().min().unwrap(),
            max: *candidates.iter().max().unwrap(),
        }
    }
}

// TODO: Optimistic division for intervals.

// TODO: add congruence domain.
// TODO: add relational domains like octagons and polyhedra.
