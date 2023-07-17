use core::array;
use std::collections::HashMap;

use crate::domains::*;
use paste::paste;

/////////////////////////
// Domain transformers //
/////////////////////////

// TODO:
// Add operations to build lattices
// * Reduced product
// * Immutable versions of the containers

/// A lattice with a height of 3, where all non-equal elements have top
/// and bottom as their least upper bound and greatest lower bound
/// respectively. It is often used to implement constant propagation.
///  
/// ```txt
///       Top
///     /  |  \
/// ... e1 e2 e3 ...
///     \  |  /
///      Bottom
/// ```
#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub enum Flat<T: Eq + Clone + Debug> {
    Top,
    Element(T),
    Bottom,
}

impl<T: Eq + Clone + Debug> PartialOrd for Flat<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            return Some(Ordering::Equal);
        }
        if *self == Flat::Top || *other == Flat::Bottom {
            return Some(Ordering::Greater);
        }
        if *self == Flat::Bottom || *other == Flat::Top {
            return Some(Ordering::Less);
        }
        None
    }
}

impl<T: Eq + Clone + Debug> JoinSemiLattice for Flat<T> {
    type LatticeContext = ();

    fn bottom(_ctx: &Self::LatticeContext) -> Self {
        Flat::Bottom
    }

    fn join(&self, other: &Self, _ctx: &Self::LatticeContext) -> Self {
        match (self, other) {
            (_, &Flat::Bottom) => self.clone(),
            (&Flat::Bottom, _) => other.clone(),
            (_, _) if self == other => other.clone(),
            _ => Flat::Top,
        }
    }
}

impl<T: Eq + Clone + Debug> Lattice for Flat<T> {
    fn top(_ctx: &Self::LatticeContext) -> Self {
        Flat::Top
    }

    fn meet(&self, other: &Self, _ctx: &Self::LatticeContext) -> Self {
        match (self, other) {
            (_, &Flat::Top) => self.clone(),
            (&Flat::Top, _) => other.clone(),
            (_, _) if self == other => other.clone(),
            _ => Flat::Bottom,
        }
    }
}

/// The None in Option acts as a bottom element, this
/// transformation is often called lift.
pub type Lift<T> = Option<T>;

impl<T: JoinSemiLattice> JoinSemiLattice for Option<T> {
    type LatticeContext = T::LatticeContext;

    fn bottom(_ctx: &Self::LatticeContext) -> Self {
        None
    }

    fn join(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
        match (self, other) {
            (&None, _) => other.clone(),
            (_, &None) => self.clone(),
            (Some(s), Some(o)) => Some(s.join(o, ctx)),
        }
    }

    fn widen(&self, previous: &Self, ctx: &Self::LatticeContext, iteration: usize) -> Self {
        self.as_ref().map(|inner| {
            previous
                .as_ref()
                .map_or_else(|| inner.clone(), |prev| inner.widen(prev, ctx, iteration))
        })
    }
}

impl<T: Lattice> Lattice for Option<T> {
    fn top(ctx: &Self::LatticeContext) -> Self {
        Some(T::top(ctx))
    }

    fn meet(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
        match (self, other) {
            (&None, _) | (_, &None) => None,
            (Some(s), Some(o)) => Some(s.meet(o, ctx)),
        }
    }

    fn narrow(&self, previous: &Self, ctx: &Self::LatticeContext, iteration: usize) -> Self {
        self.as_ref().map(|inner| {
            previous
                .as_ref()
                .map_or_else(|| inner.clone(), |prev| inner.narrow(prev, ctx, iteration))
        })
    }
}

/// A wrapper for a domain that will only start widening after
/// approximately N iterations where N is coming from the
/// [`JoinSemiLattice::LatticeContext`].
#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Hash)]
pub struct UnrollWiden<T>(pub T);

impl<T> AsRef<T> for UnrollWiden<T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T> Deref for UnrollWiden<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for UnrollWiden<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: JoinSemiLattice> JoinSemiLattice for UnrollWiden<T> {
    type LatticeContext = (usize, T::LatticeContext);

    fn bottom(ctx: &Self::LatticeContext) -> Self {
        UnrollWiden(T::bottom(&ctx.1))
    }

    fn join(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
        UnrollWiden(self.0.join(&other.0, &ctx.1))
    }

    fn widen(&self, previous: &Self, ctx: &Self::LatticeContext, iteration: usize) -> Self {
        if iteration < ctx.0 {
            self.clone()
        } else {
            UnrollWiden(self.0.widen(&previous.0, &ctx.1, iteration))
        }
    }
}

impl<T: Lattice> Lattice for UnrollWiden<T> {
    fn top(ctx: &Self::LatticeContext) -> Self {
        UnrollWiden(T::top(&ctx.1))
    }

    fn meet(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
        UnrollWiden(self.0.meet(&other.0, &ctx.1))
    }

    fn narrow(&self, previous: &Self, ctx: &Self::LatticeContext, iteration: usize) -> Self {
        UnrollWiden(self.0.narrow(previous, &ctx.1, iteration))
    }
}

/// A simple homogenous pair. It can be useful to represent the analysis
/// state for small vectors.
#[derive(PartialEq, Eq, Clone, Hash)]
pub struct Vec2Domain<T: JoinSemiLattice> {
    pub x: T,
    pub y: T,
}

impl<T: JoinSemiLattice> Debug for Vec2Domain<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{{ x: {:?}, y: {:?} }}", self.x, self.y)
    }
}

impl<T: JoinSemiLattice> PartialOrd for Vec2Domain<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let x_cmp = self.x.partial_cmp(&other.x);
        let y_cmp = self.y.partial_cmp(&other.y);
        match (x_cmp, y_cmp) {
            (Some(Ordering::Equal), _) => y_cmp,
            (_, Some(Ordering::Equal)) => x_cmp,
            (_, _) if x_cmp == y_cmp => x_cmp,
            _ => None,
        }
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

    fn join(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
        Self {
            x: self.x.join(&other.x, ctx),
            y: self.y.join(&other.y, ctx),
        }
    }

    fn widen(&self, previous: &Self, ctx: &Self::LatticeContext, iteration: usize) -> Self {
        Self {
            x: self.x.widen(&previous.x, ctx, iteration),
            y: self.y.widen(&previous.y, ctx, iteration),
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

    fn meet(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
        Self {
            x: self.x.meet(&other.x, ctx),
            y: self.y.meet(&other.y, ctx),
        }
    }

    fn narrow(&self, previous: &Self, ctx: &Self::LatticeContext, iteration: usize) -> Self {
        Self {
            x: self.x.narrow(&previous.x, ctx, iteration),
            y: self.y.narrow(&previous.y, ctx, iteration),
        }
    }
}

/// Flip a lattice by swapping the join and meet operations,
/// and the top and bottom elements.
#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct Flipped<T: Lattice>(pub T);

impl<T: Lattice> Deref for Flipped<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Lattice> DerefMut for Flipped<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: Lattice> PartialOrd for Flipped<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0).map(Ordering::reverse)
    }
}

impl<T: Lattice> JoinSemiLattice for Flipped<T> {
    type LatticeContext = T::LatticeContext;

    fn bottom(ctx: &Self::LatticeContext) -> Self {
        Self(T::top(ctx))
    }

    fn join(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
        Self(self.0.meet(&other.0, ctx))
    }

    fn widen(&self, previous: &Self, ctx: &Self::LatticeContext, iteration: usize) -> Self {
        Self(self.0.narrow(previous, ctx, iteration))
    }
}

impl<T: Lattice> Lattice for Flipped<T> {
    fn top(ctx: &Self::LatticeContext) -> Self {
        Self(T::bottom(ctx))
    }

    fn meet(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
        Self(self.0.join(&other.0, ctx))
    }

    fn narrow(&self, previous: &Self, ctx: &Self::LatticeContext, iteration: usize) -> Self {
        Self(self.0.widen(previous, ctx, iteration))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Array<T: JoinSemiLattice, const N: usize>(pub [T; N]);

impl<T: JoinSemiLattice, const N: usize> Deref for Array<T, N> {
    type Target = [T; N];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: JoinSemiLattice, const N: usize> DerefMut for Array<T, N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: JoinSemiLattice, const N: usize> PartialOrd for Array<T, N> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let mut candidate = None;
        for (lhs, rhs) in self.iter().zip(other.iter()) {
            if let Some(order) = lhs.partial_cmp(rhs) {
                if order == Ordering::Equal {
                    continue;
                }
                match candidate {
                    Some(candidate_order) if order == candidate_order => {}
                    Some(_) => return None,
                    None => candidate = Some(order),
                }
            } else {
                return None;
            }
        }
        candidate.or(Some(Ordering::Equal))
    }
}

impl<T: JoinSemiLattice, const N: usize> JoinSemiLattice for Array<T, N> {
    type LatticeContext = T::LatticeContext;

    fn bottom(ctx: &Self::LatticeContext) -> Self {
        Self(array::from_fn(|_| T::bottom(ctx)))
    }

    fn join(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
        let mut it = self.iter().zip(other.iter()).map(|(s, o)| s.join(o, ctx));
        Self(array::from_fn(|_| it.next().unwrap()))
    }

    fn widen(&self, previous: &Self, ctx: &Self::LatticeContext, iteration: usize) -> Self {
        let mut it = self
            .iter()
            .zip(previous.iter())
            .map(|(s, p)| s.widen(p, ctx, iteration));
        Self(array::from_fn(|_| it.next().unwrap()))
    }
}

impl<T: Lattice, const N: usize> Lattice for Array<T, N> {
    fn top(ctx: &Self::LatticeContext) -> Self {
        Self(array::from_fn(|_| T::top(ctx)))
    }

    fn meet(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
        let mut it = self.iter().zip(other.iter()).map(|(s, o)| s.meet(o, ctx));
        Self(array::from_fn(|_| it.next().unwrap()))
    }

    fn narrow(&self, previous: &Self, ctx: &Self::LatticeContext, iteration: usize) -> Self {
        let mut it = self
            .iter()
            .zip(previous.iter())
            .map(|(s, p)| s.narrow(p, ctx, iteration));
        Self(array::from_fn(|_| it.next().unwrap()))
    }
}

/// The map lattice is often used to encode information about multiple
/// elements of the program state like variables. In those cases the key
/// would be the unique resolutions of the variables (e.g., fully qualified name),
/// and the value would be the tracked state for each variable (e.g., [`IntervalDomain`]).
///
/// Warning: M1 without K compares less than M2 with K => Bottom. if this is undesired,
/// make sure all keys are populated.
#[derive(PartialEq, Eq, Clone)]
pub struct Map<K: Eq + Clone + Hash + Debug, V: JoinSemiLattice>(pub HashMap<K, V>);

/// Contains all the keys for top value, can leave it empty for
/// join semi-lattices.
pub struct MapCtx<K: Eq + Clone + Hash + Debug, V: JoinSemiLattice>(
    pub HashSet<K>,
    pub V::LatticeContext,
);

impl<K: Eq + Clone + Hash + Debug, V: JoinSemiLattice> AsRef<Map<K, V>> for Map<K, V> {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl<K: Eq + Clone + Hash + Debug, V: JoinSemiLattice> Default for Map<K, V> {
    fn default() -> Self {
        Map(HashMap::default())
    }
}

impl<K: Eq + Clone + Hash + Debug, V: JoinSemiLattice> Map<K, V> {
    pub fn changed_values(&self, previous: &Self) -> Self {
        let mut result = Map::default();
        for (k, v) in self.iter() {
            match previous.get(k) {
                Some(prev_v) if prev_v != v => result.insert(k.clone(), v.clone()),
                None => result.insert(k.clone(), v.clone()),
                _ => continue,
            };
        }
        result
    }

    pub fn get_or_bottom(&self, k: &K, ctx: &MapCtx<K, V>) -> V {
        self.get(k).unwrap_or(&V::bottom(&ctx.1)).clone()
    }
}

impl<K: Eq + Clone + Hash + Debug, V: Lattice> Map<K, V> {
    pub fn get_or_top(&self, k: &K, ctx: &MapCtx<K, V>) -> V {
        self.get(k).unwrap_or(&V::top(&ctx.1)).clone()
    }
}

impl<K: Eq + Clone + Hash + Debug, V: JoinSemiLattice> Deref for Map<K, V> {
    type Target = HashMap<K, V>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<K: Eq + Clone + Hash + Debug, V: JoinSemiLattice> DerefMut for Map<K, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<K: Eq + Clone + Hash + Debug, V: JoinSemiLattice> Debug for Map<K, V> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut elements: Vec<String> = self.iter().map(|x| format!("{x:?}")).collect();
        elements.sort_unstable();
        write!(f, "Map({})", elements.join(", "))
    }
}

impl<K: Eq + Clone + Hash + Debug, V: JoinSemiLattice> PartialOrd for Map<K, V> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            return Some(Ordering::Equal);
        }
        // Unfortunately, HashMap does not have set operations.
        let mut candidate = None;
        for (k, v) in &self.0 {
            // Could be simplified if we could create a bottom value here.
            if let Some(other_v) = other.get(k) {
                match (candidate, v.partial_cmp(other_v)) {
                    // If not comparable at a point, the maps are also not comparable.
                    (_, None) => return None,
                    // Equal elements will not influence the comparison result.
                    (_, Some(Ordering::Equal)) => continue,
                    // The ordering if this element is in agreement with the ordering of others.
                    (Some(o1), Some(o2)) if o1 == o2 => continue,
                    // Contradiction found, maps are not comparable.
                    (Some(_), Some(_)) => return None,
                    // Found the first non-equal element, set the candidate ordering.
                    (None, cmp_result) => candidate = cmp_result,
                };
            } else if candidate.is_none() {
                // Non-existent key in the other map, non-existent key compares smaller than
                // bottom.
                candidate = Some(Ordering::Greater);
            } else if candidate != Some(Ordering::Greater) {
                // Contradiction with the candidate ordering.
                return None;
            }
        }
        for k in other.keys() {
            if self.contains_key(k) {
                continue;
            }
            if candidate.is_none() || candidate == Some(Ordering::Less) {
                return Some(Ordering::Less);
            }
            return None;
        }
        candidate
    }
}

impl<K: Eq + Clone + Hash + Debug, V: JoinSemiLattice> JoinSemiLattice for Map<K, V> {
    type LatticeContext = MapCtx<K, V>;

    fn bottom(_ctx: &Self::LatticeContext) -> Self {
        Self(HashMap::new())
    }

    fn join(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
        let mut result = HashMap::new();
        for (k, v) in &self.0 {
            if let Some(other_v) = other.get(k) {
                result.insert(k.clone(), v.join(other_v, &ctx.1));
            } else {
                result.insert(k.clone(), v.clone());
            }
        }
        for (k, v) in &other.0 {
            if self.contains_key(k) {
                continue;
            }
            result.insert(k.clone(), v.clone());
        }
        Self(result)
    }

    fn widen(&self, previous: &Self, ctx: &Self::LatticeContext, iteration: usize) -> Self {
        if *previous == Self::bottom(ctx) {
            return self.clone();
        }
        let mut result = HashMap::new();
        for (k, v) in &self.0 {
            if let Some(prev_v) = previous.get(k) {
                result.insert(k.clone(), v.widen(prev_v, &ctx.1, iteration));
            }
            // Leave out new elements since the previous iteration to avoid
            // unbounded growth. Do we want to insert top instead?
        }
        Self(result)
    }
}

impl<K: Eq + Clone + Hash + Debug, V: Lattice> Lattice for Map<K, V> {
    fn top(ctx: &Self::LatticeContext) -> Self {
        let mut result = HashMap::new();
        for k in &ctx.0 {
            result.insert(k.clone(), V::top(&ctx.1));
        }
        Self(result)
    }

    fn meet(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
        let mut result = HashMap::new();
        for (k, v) in &self.0 {
            if let Some(other_v) = other.get(k) {
                result.insert(k.clone(), v.meet(other_v, &ctx.1));
            }
        }
        Self(result)
    }

    fn narrow(&self, previous: &Self, ctx: &Self::LatticeContext, iteration: usize) -> Self {
        let mut result = HashMap::new();
        for (k, v) in &self.0 {
            if let Some(prev_v) = previous.get(k) {
                result.insert(k.clone(), v.narrow(prev_v, &ctx.1, iteration));
            }
            // What to do with elements that are only in one of the maps?
        }
        Self(result)
    }
}

///////////////////////////////////////
// Product lattices up to 5 elements //
///////////////////////////////////////

macro_rules! tuple_lattice {
    ( $prod:ident $( $name:ident )+ ) => {
        paste! {
            /// Product lattice with point-wise ordering.
            #[derive(Clone, PartialEq, Eq, Debug, Hash)]
            pub struct $prod<$($name: JoinSemiLattice),+>($(pub $name,)+);

            impl<$($name: JoinSemiLattice),+> PartialOrd for $prod<$($name),+> {
                fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                    if self == other {
                        return Some(Ordering::Equal);
                    }
                    let $prod($([<$name:lower 1>],)+) = self;
                    let $prod($([<$name:lower 2>],)+) = other;
                    if $([<$name:lower 1>] <= [<$name:lower 2>] && )* true {
                        return Some(Ordering::Less);
                    }
                    if $([<$name:lower 1>] >= [<$name:lower 2>] && )* true {
                        return Some(Ordering::Greater);
                    }
                    None
                }
            }

            impl<$($name: JoinSemiLattice),+> JoinSemiLattice for $prod<$($name,)+>
            {
                type LatticeContext = ($($name::LatticeContext,)+);

                fn bottom(ctx: &Self::LatticeContext) -> Self {
                    let ($([<$name:lower 1>],)+) = ctx;
                    $prod($($name::bottom([<$name:lower 1>]),)+)
                }

                fn join(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
                    let $prod($([<$name:lower 1>],)+) = self;
                    let $prod($([<$name:lower 2>],)+) = other;
                    let ($([<$name:lower 3>],)+) = ctx;
                    $prod($([<$name:lower 1>].join([<$name:lower 2>], [<$name:lower 3>]),)*)
                }

                fn widen(&self, previous: &Self, ctx: &Self::LatticeContext, iteration: usize) -> Self {
                    let $prod($([<$name:lower 1>],)+) = self;
                    let $prod($([<$name:lower 2>],)+) = previous;
                    let ($([<$name:lower 3>],)+) = ctx;
                    $prod(
                    $([<$name:lower 1>].widen([<$name:lower 2>], [<$name:lower 3>], iteration),)*
                    )
                }
            }

            impl<$($name: Lattice),+> Lattice for $prod<$($name,)+>
            {
                fn top(ctx: &Self::LatticeContext) -> Self {
                    let ($([<$name:lower 1>],)+) = ctx;
                    $prod($($name::top([<$name:lower 1>]),)+)
                }

                fn meet(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
                    let $prod($([<$name:lower 1>],)+) = self;
                    let $prod($([<$name:lower 2>],)+) = other;
                    let ($([<$name:lower 3>],)+) = ctx;
                    $prod($([<$name:lower 1>].meet([<$name:lower 2>], [<$name:lower 3>]),)*)
                }

                fn narrow(&self, previous: &Self, ctx: &Self::LatticeContext, iteration: usize) -> Self {
                    let $prod($([<$name:lower 1>],)+) = self;
                    let $prod($([<$name:lower 2>],)+) = previous;
                    let ($([<$name:lower 3>],)+) = ctx;
                    $prod(
                    $([<$name:lower 1>].narrow([<$name:lower 2>], [<$name:lower 3>], iteration),)*
                    )
                }
            }
        }
    };
}

tuple_lattice!(Prod1 D1);
tuple_lattice!(Prod2 D1 D2);
tuple_lattice!(Prod3 D1 D2 D3);
tuple_lattice!(Prod4 D1 D2 D3 D4);
tuple_lattice!(Prod5 D1 D2 D3 D4 D5);

/////////////////////////////////////
// Stack lattices up to 5 elements //
/////////////////////////////////////

macro_rules! stack_lattice {
    ( $stack:ident $top:tt $( $name:tt )+ ) => {
        paste! {
            /// Stack lattice, the generic arguments are ordered:
            /// elements of the type to the left are smaller than the
            /// elements of the type to the right.
            #[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Hash)]
            pub enum $stack<$([<T $name>]: JoinSemiLattice),+>{
                Bottom,
                $([<S $name>]([<T $name>])),+
            }

            impl<$([<T $name>]: JoinSemiLattice),+> JoinSemiLattice
                for $stack<$([<T $name>],)+>
            {
                type LatticeContext = ($([<T $name>]::LatticeContext,)+);

                fn bottom(_ctx: &Self::LatticeContext) -> Self {
                    $stack::Bottom
                }

                fn join(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
                    let ($([<c $name:lower 3>],)+) = ctx;
                    match (self, other) {
                        $(($stack::[<S $name>]([<s $name 1>]), $stack::[<S $name>]([<s $name 2>])) =>
                            $stack::[<S $name>]([<s $name 1>].join([<s $name 2>], [<c $name:lower 3>]))),+,
                        _ if self < other => other.clone(),
                        _ => self.clone()
                    }
                }

                fn widen(&self, previous: &Self, ctx: &Self::LatticeContext, iteration: usize) -> Self {
                    let ($([<c $name:lower 3>],)+) = ctx;
                    match (self, previous) {
                        $(($stack::[<S $name>]([<s $name 1>]), $stack::[<S $name>]([<s $name 2>])) =>
                            $stack::[<S $name>]([<s $name 1>].widen([<s $name 2>], [<c $name:lower 3>], iteration))),+,
                        _ => self.clone(),
                    }
                }
            }

            impl<$([<T $name>]: Lattice),+> Lattice
                for $stack<$([<T $name>],)+>
            {
                fn top(ctx: &Self::LatticeContext) -> Self {
                    let ($([<_c $name:lower 3>],)+) = ctx;
                    $stack::[<S $top>]([<T $top>]::top([<_c $top 3>]))
                }

                fn meet(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
                    let ($([<c $name:lower 3>],)+) = ctx;
                    match (self, other) {
                        $(($stack::[<S $name>]([<s $name 1>]), $stack::[<S $name>]([<s $name 2>])) =>
                            $stack::[<S $name>]([<s $name 1>].meet([<s $name 2>], [<c $name:lower 3>]))),+,
                        _ if self > other => other.clone(),
                        _ => self.clone()
                    }
                }

                fn narrow(&self, previous: &Self, ctx: &Self::LatticeContext, iteration: usize) -> Self {
                    let ($([<c $name:lower 3>],)+) = ctx;
                    match (self, previous) {
                        $(($stack::[<S $name>]([<s $name 1>]), $stack::[<S $name>]([<s $name 2>])) =>
                            $stack::[<S $name>]([<s $name 1>].narrow([<s $name 2>], [<c $name:lower 3>], iteration))),+,
                        _ => self.clone(),
                    }
                }
            }
        }
    };
}

stack_lattice!(Stack2 2 1 2);
stack_lattice!(Stack3 3 1 2 3);
stack_lattice!(Stack4 4 1 2 3 4);
stack_lattice!(Stack5 5 1 2 3 4 5);

//////////////////////////////////////////////
// Disjoint union lattices up to 5 elements //
//////////////////////////////////////////////

macro_rules! distjoint_union_lattice {
    ( $union:ident $( $name:tt )+ ) => {
        paste! {
            /// A disjoint union lattice.
            #[derive(Clone, PartialEq, Eq, Debug, Hash)]
            pub enum $union<$([<T $name>]: JoinSemiLattice),+>{
                Bottom,
                $([<U $name>]([<T $name>])),+,
                Top
            }

            impl<$([<T $name>]: JoinSemiLattice),+> PartialOrd
                for $union<$([<T $name>],)+> {

                fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                    if self == other {
                        return Some(Ordering::Equal);
                    }
                    match (self, other) {
                        $(($union::[<U $name>]([<u $name 1>]), $union::[<U $name>]([<u $name 2>])) =>
                            [<u $name 1>].partial_cmp([<u $name 2>])),+,
                        ($union::Bottom, _) => Some(Ordering::Less),
                        ($union::Top, _) => Some(Ordering::Greater),
                        (_, $union::Bottom) => Some(Ordering::Greater),
                        (_, $union::Top) => Some(Ordering::Less),
                        _ => None,
                    }
                }
            }

            impl<$([<T $name>]: JoinSemiLattice),+> JoinSemiLattice
                for $union<$([<T $name>],)+>
            {
                type LatticeContext = ($([<T $name>]::LatticeContext,)+);

                fn bottom(_ctx: &Self::LatticeContext) -> Self {
                    $union::Bottom
                }

                fn join(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
                    let ($([<c $name:lower 3>],)+) = ctx;
                    match (self, other) {
                        ($union::Bottom, _) => other.clone(),
                        (_, $union::Bottom) => self.clone(),
                        $(($union::[<U $name>]([<u $name 1>]), $union::[<U $name>]([<u $name 2>])) =>
                            $union::[<U $name>]([<u $name 1>].join([<u $name 2>], [<c $name:lower 3>]))),+,
                        _ => $union::Top
                    }
                }

                fn widen(&self, previous: &Self, ctx: &Self::LatticeContext, iteration: usize) -> Self {
                    let ($([<c $name:lower 3>],)+) = ctx;
                    match (self, previous) {
                        $(($union::[<U $name>]([<u $name 1>]), $union::[<U $name>]([<u $name 2>])) =>
                            $union::[<U $name>]([<u $name 1>].widen([<u $name 2>], [<c $name:lower 3>], iteration))),+,
                        _ => self.clone(),
                    }
                }
            }

            impl<$([<T $name>]: Lattice),+> Lattice
                for $union<$([<T $name>],)+>
            {
                fn top(_ctx: &Self::LatticeContext) -> Self {
                    $union::Top
                }

                fn meet(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
                    let ($([<c $name:lower 3>],)+) = ctx;
                    match (self, other) {
                        ($union::Top, _) => other.clone(),
                        (_, $union::Top) => self.clone(),
                        $(($union::[<U $name>]([<u $name 1>]), $union::[<U $name>]([<u $name 2>])) =>
                            $union::[<U $name>]([<u $name 1>].meet([<u $name 2>], [<c $name:lower 3>]))),+,
                        _ => $union::Bottom,
                    }
                }

                fn narrow(&self, previous: &Self, ctx: &Self::LatticeContext, iteration: usize) -> Self {
                    let ($([<c $name:lower 3>],)+) = ctx;
                    match (self, previous) {
                        $(($union::[<U $name>]([<u $name 1>]), $union::[<U $name>]([<u $name 2>])) =>
                            $union::[<U $name>]([<u $name 1>].narrow([<u $name 2>], [<c $name:lower 3>], iteration))),+,
                        _ => self.clone(),
                    }
                }
            }
        }
    };
}

distjoint_union_lattice!(Union2 1 2);
distjoint_union_lattice!(Union3 1 2 3);
distjoint_union_lattice!(Union4 1 2 3 4);
distjoint_union_lattice!(Union5 1 2 3 4 5);
