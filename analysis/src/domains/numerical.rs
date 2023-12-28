use number_theory::NumberTheory;

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
/// For more precision, consider using [`Interval`].
#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub enum Sign {
    Top,
    Bottom,
    Negative,
    Zero,
    Positive,
    NonNeg,
    NonZero,
    NonPos,
}

impl From<i32> for Sign {
    fn from(val: i32) -> Self {
        match val.cmp(&0) {
            Ordering::Less => Sign::Negative,
            Ordering::Equal => Sign::Zero,
            Ordering::Greater => Sign::Positive,
        }
    }
}

impl From<Interval> for Sign {
    fn from(value: Interval) -> Self {
        if value == Interval::bottom(&()) {
            return Sign::Bottom;
        }
        if value == Interval::from(0) {
            return Sign::Zero;
        }
        if value.max < 0 {
            return Sign::Negative;
        }
        if value.max <= 0 {
            return Sign::NonPos;
        }
        if value.min > 0 {
            return Sign::Positive;
        }
        if value.min >= 0 {
            return Sign::NonNeg;
        }
        Sign::Top
    }
}

mod sign_tables {
    use super::Sign::{self, *};
    pub fn index_of(s: Sign) -> usize {
        match s {
            Sign::Top => 0,
            Sign::Bottom => 1,
            Sign::Negative => 2,
            Sign::Zero => 3,
            Sign::Positive => 4,
            Sign::NonNeg => 5,
            Sign::NonZero => 6,
            Sign::NonPos => 7,
        }
    }

    #[rustfmt::skip]
    pub const JOIN : [[Sign; 8]; 8] =
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
    pub const MEET : [[Sign; 8]; 8] =
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
    pub const ADDITION : [[Sign; 8]; 8] =
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
    pub const MULTIPLICATION : [[Sign; 8]; 8] =
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

impl Add for Sign {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        use sign_tables::*;
        ADDITION[index_of(self)][index_of(rhs)]
    }
}

impl Neg for Sign {
    type Output = Self;
    fn neg(self) -> Self::Output {
        match self {
            Sign::Negative => Sign::Positive,
            Sign::Positive => Sign::Negative,
            Sign::NonPos => Sign::NonNeg,
            Sign::NonNeg => Sign::NonPos,
            _ => self,
        }
    }
}

impl Sub for Sign {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        self + -rhs
    }
}

impl Mul for Sign {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        use sign_tables::*;
        MULTIPLICATION[index_of(self)][index_of(rhs)]
    }
}

impl Div for Sign {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        #[allow(clippy::suspicious_arithmetic_impl)]
        match self * rhs {
            Sign::Positive => Sign::NonNeg,
            Sign::Negative => Sign::NonPos,
            res => res,
        }
    }
}

impl Rem for Sign {
    type Output = Self;
    fn rem(self, _rhs: Self) -> Self::Output {
        match self {
            Sign::NonNeg | Sign::Positive => Sign::NonNeg,
            Sign::NonPos | Sign::Negative => Sign::NonPos,
            Sign::Zero => Sign::Zero,
            Sign::Bottom => Sign::Bottom,
            _ => Sign::Top,
        }
    }
}

impl PartialOrd for Sign {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use sign_tables::*;
        COMPARISON[index_of(*self)][index_of(*other)]
    }
}

impl Sign {
    pub fn logical_eq(self, other: Sign, is_bool: bool) -> Sign {
        if (is_bool && self == Sign::Positive && other == Sign::Positive)
            || (self == Sign::Zero && other == Sign::Zero)
        {
            Sign::Positive
        } else if self <= other || other <= self {
            Sign::NonNeg
        } else {
            Sign::Zero
        }
    }

    pub fn logical_and(self, other: Sign) -> Sign {
        match (self, other) {
            (Sign::Zero, _) | (_, Sign::Zero) => Sign::Zero,
            (Sign::Positive, Sign::Positive) => Sign::Positive,
            _ => Sign::NonNeg,
        }
    }

    pub fn logical_or(self, other: Sign) -> Sign {
        match (self, other) {
            (Sign::Positive, _) | (_, Sign::Positive) => Sign::Positive,
            (Sign::Zero, Sign::Zero) => Sign::Zero,
            _ => Sign::NonNeg,
        }
    }

    pub fn logical_not(self) -> Sign {
        match self {
            Sign::NonNeg => Sign::NonNeg,
            Sign::Positive => Sign::Zero,
            Sign::Zero => Sign::Positive,
            _ => panic!("Unexpected value"),
        }
    }

    /// Compares the concrete values.
    pub fn strict_cmp(self, other: Sign) -> Option<Ordering> {
        use sign_tables::*;
        STRICT_CONCRETE_COMPARISON[index_of(self)][index_of(other)]
    }

    /// Compares the concrete values.
    pub fn weak_cmp(self, other: Sign) -> Option<Ordering> {
        use sign_tables::*;
        WEAK_CONCRETE_COMPARISON[index_of(self)][index_of(other)]
    }
}

impl JoinSemiLattice for Sign {
    type LatticeContext = ();

    fn bottom(&(): &Self::LatticeContext) -> Self {
        Sign::Bottom
    }

    fn join(&self, other: &Self, &(): &Self::LatticeContext) -> Self {
        use sign_tables::*;
        JOIN[index_of(*self)][index_of(*other)]
    }
}

impl Lattice for Sign {
    fn top(&(): &Self::LatticeContext) -> Self {
        Sign::Top
    }

    fn meet(&self, other: &Self, &(): &Self::LatticeContext) -> Self {
        use sign_tables::*;
        MEET[index_of(*self)][index_of(*other)]
    }
}

pub const INF: i64 = i64::MAX;
pub const NEG_INF: i64 = i64::MIN;

/// [`Interval`] is often used to represent a possible range of values.
/// The lattice is ordered by inclusion and has very long ascending and
/// descending chains, thus it implements widening. It also implements
/// some basic arithmetic operations.
#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct Interval {
    pub min: i64,
    pub max: i64,
}

pub static BOOL_RANGE: Interval = Interval { min: 0, max: 1 };
pub static TRUE_RANGE: Interval = Interval { min: 1, max: 1 };
pub static FALSE_RANGE: Interval = Interval { min: 0, max: 0 };

// TODO: figure out a way to add thresholds to widening?
//       maybe using the domain context as an object to store thresholds.

impl Interval {
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
        (self.min == self.max).then_some(self.min)
    }

    pub fn logical_and(self, other: Self) -> Self {
        match (self.singleton(), other.singleton()) {
            (Some(0), _) | (_, Some(0)) => FALSE_RANGE,
            (Some(1), Some(1)) => TRUE_RANGE,
            _ => BOOL_RANGE,
        }
    }

    pub fn logical_or(self, other: Self) -> Self {
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

    pub fn strict_cmp(self, other: Self) -> Option<Ordering> {
        if self == Self::bottom(&()) || other == Self::bottom(&()) {
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

    pub fn weak_cmp(self, other: Self) -> Option<Ordering> {
        if self == Self::bottom(&()) || other == Self::bottom(&()) {
            return None;
        }

        if self.max <= other.min {
            return Some(Ordering::Less);
        }

        if self.min >= other.max {
            return Some(Ordering::Greater);
        }

        match (self.singleton(), other.singleton()) {
            (Some(x), Some(y)) if x == y => Some(Ordering::Equal),
            _ => None,
        }
    }

    pub fn equals(self, other: Self) -> Self {
        match self.strict_cmp(other) {
            Some(Ordering::Less | Ordering::Greater) => FALSE_RANGE,
            Some(Ordering::Equal) => TRUE_RANGE,
            _ => BOOL_RANGE,
        }
    }

    /// Returns a subinterval for the inputs where the equality may hold
    /// (and cannot hold outside).
    pub fn may_equal_when(self, other: Self) -> Self {
        let bot = Self::bottom(&());
        if self == bot || other == bot {
            return bot;
        }
        Self {
            min: self.min.max(other.min),
            max: self.max.min(other.max),
        }
        .normalize()
    }

    // TODO: add function to restrict values for not equal.

    /// Returns a pair of subintervals for the inputs where the less then relation may
    /// hold (and cannot hold outside).
    pub fn maybe_less_when(self, other: Self) -> (Self, Self) {
        let bot = Self::bottom(&());
        if self == bot || other == bot || self.min > other.max {
            return (bot, bot);
        }
        (
            Self {
                min: self.min,
                max: self.max.min(other.max - 1),
            }
            .normalize(),
            Self {
                min: (self.min + 1).max(other.min),
                max: other.max,
            }
            .normalize(),
        )
    }

    /// Returns a pair of subintervals for the inputs where the less then or equal
    /// relation may hold (and cannot hold outside).
    pub fn maybe_le_when(self, other: Self) -> (Self, Self) {
        let bot = Self::bottom(&());
        if self == bot || other == bot || self.min > other.max {
            return (bot, bot);
        }
        (
            Self {
                min: self.min,
                max: self.max.min(other.max),
            }
            .normalize(),
            Self {
                min: self.min.max(other.min),
                max: other.max,
            }
            .normalize(),
        )
    }

    fn normalize(self) -> Self {
        if self.min > self.max {
            return Self::bottom(&());
        }
        self
    }
}

impl From<i64> for Interval {
    fn from(val: i64) -> Self {
        Self { min: val, max: val }
    }
}

impl From<Sign> for Interval {
    fn from(value: Sign) -> Self {
        match value {
            Sign::Top | Sign::NonZero => Self::top(&()),
            Sign::Bottom => Self::bottom(&()),
            Sign::Zero => Self::from(0),
            Sign::Positive => Self { min: 1, max: INF },
            Sign::Negative => Self {
                min: NEG_INF,
                max: -1,
            },
            Sign::NonNeg => Self { min: 0, max: INF },
            Sign::NonPos => Self {
                min: NEG_INF,
                max: 0,
            },
        }
    }
}

impl Debug for Interval {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let to_str = |x: i64| match x {
            INF => std::borrow::Cow::from("inf"),
            NEG_INF => std::borrow::Cow::from("-inf"),
            _ => std::borrow::Cow::Owned(x.to_string()),
        };
        write!(f, "[{}, {}]", to_str(self.min), to_str(self.max))
    }
}

impl PartialOrd for Interval {
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

impl JoinSemiLattice for Interval {
    type LatticeContext = ();

    fn bottom(&(): &Self::LatticeContext) -> Self {
        Self {
            min: INF,
            max: NEG_INF,
        }
    }

    fn join(&self, other: &Self, &(): &Self::LatticeContext) -> Self {
        Self {
            min: self.min.min(other.min),
            max: self.max.max(other.max),
        }
    }

    /// Extrapolate unstable bounds to infinity.
    fn widen(&self, prev: &Self, &(): &Self::LatticeContext, _: usize) -> Self {
        if *prev == Self::bottom(&()) {
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

impl Lattice for Interval {
    fn top(&(): &Self::LatticeContext) -> Self {
        Self {
            min: NEG_INF,
            max: INF,
        }
    }

    fn meet(&self, other: &Self, &(): &Self::LatticeContext) -> Self {
        let result = Self {
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

    /// Improve infinite bounds.
    fn narrow(&self, prev: &Self, &(): &Self::LatticeContext, _: usize) -> Self {
        if *prev == Self::bottom(&()) || *self == Self::bottom(&()) {
            return Self::bottom(&());
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

impl Add for Interval {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        let bot = Self::bottom(&());
        if self == bot || rhs == bot {
            return bot;
        }
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

impl Neg for Interval {
    type Output = Self;
    fn neg(self) -> Self {
        if self == Self::bottom(&()) {
            return self;
        }
        Self {
            min: if self.max == INF { NEG_INF } else { -self.max },
            max: if self.min == NEG_INF { INF } else { -self.min },
        }
    }
}

impl Sub for Interval {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        self + -rhs
    }
}

impl Mul for Interval {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        let bot = Self::bottom(&());
        if self == bot || rhs == bot {
            return bot;
        }
        let candidates = [
            self.min.saturating_mul(rhs.min),
            self.min.saturating_mul(rhs.max),
            self.max.saturating_mul(rhs.min),
            self.max.saturating_mul(rhs.max),
        ];
        Self {
            min: *candidates.iter().min().unwrap(),
            max: *candidates.iter().max().unwrap(),
        }
    }
}

impl Rem for Interval {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        let bot = Self::bottom(&());
        if self == bot || rhs == bot || rhs == Self::from(0) {
            return bot;
        }
        if let (Some(x), Some(y)) = (self.singleton(), rhs.singleton()) {
            return Self::from(x % y);
        }
        let largest_mod = rhs.min.abs().max(rhs.max.abs());
        match self.weak_cmp(Self::from(0)) {
            Some(Ordering::Greater) => Self {
                min: 0,
                max: largest_mod - 1,
            },
            Some(Ordering::Equal) => Self::from(0),
            Some(Ordering::Less) => Self {
                min: -largest_mod + 1,
                max: 0,
            },
            None => Self {
                min: -largest_mod + 1,
                max: largest_mod - 1,
            },
        }
    }
}

/// In this domain, `c mod m` represents values that has `c` as a remainder
/// modulo `m`. This can be useful to evaluate certain conditions, to
/// improve the precision of a range analysis, or to reason about the
/// alignments of memory allocations.
///
/// This domain is the generalization of the flat domain for constants
/// and parity. `5 mod 0` represents the constant `5`, `0 mod 1` represents
/// all the integers, `0 mod 2` represents even numbers, `1 mod 2` represents
/// odd numbers.
#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct Congruence {
    constant: i64,
    modulus: i64,
}

impl Congruence {
    pub fn from(constant: i64, modulus: i64) -> Self {
        debug_assert!(modulus == 0 || constant < modulus);
        Self { constant, modulus }
    }

    pub fn constant(&self) -> i64 {
        self.constant
    }

    pub fn modulus(&self) -> i64 {
        self.modulus
    }

    pub fn disjoint(self, other: Self) -> bool {
        if self.modulus == 0 && other.modulus == 0 {
            return self.constant != other.constant;
        }
        if self.modulus == 0 {
            return (self.constant % other.modulus) != other.constant;
        }
        if other.modulus == 0 {
            return (other.constant % self.modulus) != self.constant;
        }
        let modulus = self.modulus.gcd(&other.modulus);
        (self.constant - other.constant) % modulus != 0
    }

    fn normalize(self) -> Self {
        if self.modulus != 0 {
            return Self {
                constant: self.constant % self.modulus,
                modulus: self.modulus,
            };
        }
        self
    }

    pub fn singleton(self) -> Option<i64> {
        if self.modulus == 0 {
            return Some(self.constant);
        }
        None
    }

    pub fn equals(self, other: Self) -> Self {
        if self.disjoint(other) {
            return Self::from(0, 0);
        }

        match (self.singleton(), other.singleton()) {
            (Some(x), Some(y)) if y == x => Self::from(1, 0),
            _ => Self::top(&()),
        }
    }

    pub fn logical_and(self, other: Self) -> Self {
        match (self.singleton(), other.singleton()) {
            (Some(0), _) | (_, Some(0)) => Self::from(0, 0),
            (Some(1), Some(1)) => Self::from(1, 0),
            _ => Self::top(&()),
        }
    }

    pub fn logical_or(self, other: Self) -> Self {
        match (self.singleton(), other.singleton()) {
            (Some(1), _) | (_, Some(1)) => Self::from(1, 0),
            (Some(0), Some(0)) => Self::from(0, 0),
            _ => Self::top(&()),
        }
    }

    pub fn logical_not(self) -> Self {
        match self.singleton() {
            Some(1) => Self::from(0, 0),
            Some(0) => Self::from(1, 0),
            _ => Self::top(&()),
        }
    }

    pub fn strict_cmp(self, other: Self) -> Option<Ordering> {
        match (self.singleton(), other.singleton()) {
            (Some(x), Some(y)) => Some(x.cmp(&y)),
            _ => None,
        }
    }
}

impl Debug for Congruence {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{} mod {}", self.constant, self.modulus)
    }
}

impl PartialOrd for Congruence {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            return Some(Ordering::Equal);
        }

        if *self == Self::bottom(&()) {
            return Some(Ordering::Less);
        }

        if *other == Self::bottom(&()) {
            return Some(Ordering::Greater);
        }

        if other.modulus != 0
            && self.modulus % other.modulus == 0
            && (self.constant % other.modulus) == other.constant
        {
            return Some(Ordering::Less);
        }

        if self.modulus != 0
            && other.modulus % self.modulus == 0
            && (other.constant % self.modulus) == self.constant
        {
            return Some(Ordering::Greater);
        }

        None
    }
}

impl JoinSemiLattice for Congruence {
    type LatticeContext = ();

    fn bottom(&(): &Self::LatticeContext) -> Self {
        // In the representation when modulus is non-zero,
        // constant needs to be less than modulus. Using
        // an specific value where this invariant does not hold
        // as bottom. TODO: should we have an enum instead?
        Self {
            constant: 1,
            modulus: 1,
        }
    }

    fn join(&self, other: &Self, &(): &Self::LatticeContext) -> Self {
        if *self == Self::bottom(&()) {
            return *other;
        }
        if *other == Self::bottom(&()) {
            return *self;
        }
        let new_modulus = self
            .modulus
            .gcd(&other.modulus)
            .gcd(&(self.constant - other.constant).abs());
        Self {
            constant: self.constant,
            modulus: new_modulus,
        }
        .normalize()
    }

    // No need for widening, there are no infinite ascending chains.
}

impl Lattice for Congruence {
    fn top(&(): &Self::LatticeContext) -> Self {
        Self {
            constant: 0,
            modulus: 1,
        }
    }

    fn meet(&self, other: &Self, &(): &Self::LatticeContext) -> Self {
        let bot = Self::bottom(&());
        if *self == bot || *other == bot {
            return bot;
        }
        if self.modulus == 0 {
            return if self.disjoint(*other) { bot } else { *self };
        }
        if other.modulus == 0 {
            return if self.disjoint(*other) { bot } else { *other };
        }
        let modulus = self.modulus.gcd(&other.modulus);
        if (self.constant - other.constant).abs() % modulus == 0 {
            let new_modulus = self.modulus.lcm(&other.modulus);
            let new_constant = self.constant % new_modulus;
            Self {
                constant: new_constant,
                modulus: new_modulus,
            }
            .normalize()
        } else {
            bot
        }
    }

    fn narrow(&self, previous: &Self, &(): &Self::LatticeContext, _iteration: usize) -> Self {
        if *previous == Self::top(&()) {
            return *self;
        }
        *previous
    }
}

impl Neg for Congruence {
    type Output = Self;

    fn neg(self) -> Self::Output {
        if self == Self::bottom(&()) {
            return self;
        }
        Self::from(-self.constant, self.modulus)
    }
}

impl Add for Congruence {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let bot = Self::bottom(&());
        if self == bot || rhs == bot {
            return bot;
        }
        let new_modulus = self.modulus.gcd(&rhs.modulus);
        Self {
            constant: self.constant + rhs.constant,
            modulus: new_modulus,
        }
        .normalize()
    }
}

impl Sub for Congruence {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self + (-rhs)
    }
}

impl Mul for Congruence {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let bot = Self::bottom(&());
        if self == bot || rhs == bot {
            return bot;
        }
        let new_modulus = (self.modulus * rhs.modulus)
            .gcd(&(self.modulus * rhs.constant))
            .gcd(&(rhs.modulus * self.constant));
        Self {
            constant: self.constant * rhs.constant,
            modulus: new_modulus,
        }
        .normalize()
    }
}

impl Div for Congruence {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        let bot = Self::bottom(&());
        if self == bot || rhs == bot || rhs == Self::from(0, 0) {
            return bot;
        }
        if rhs.modulus == 0 && self.constant % rhs.constant == 0 && self.modulus % rhs.constant == 0
        {
            return Self::from(self.constant / rhs.constant, self.modulus / rhs.constant);
        }
        Self::top(&())
    }
}

// TODO: Optimistic division for intervals.

// TODO: add relational domains like zones, octagons and polyhedra.
