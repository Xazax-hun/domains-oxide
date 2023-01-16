use crate::domains::*;
use paste::paste;

/////////////////////////
// Domain transformers //
/////////////////////////

// TODO:
// Add operations to built lattices
// * Reduced product
// * Disjoint union
// * Stacking
// * Lifting
// * Finite lattices

// TODO:
// Add more general building blocks for finite domains and
// port SignDomain to those facilities.

#[derive(PartialEq, Eq, Clone, Debug)]
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

    fn join(&self, other: &Self) -> Self {
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

    fn meet(&self, other: &Self) -> Self {
        match (self, other) {
            (_, &Flat::Top) => self.clone(),
            (&Flat::Top, _) => other.clone(),
            (_, _) if self == other => other.clone(),
            _ => Flat::Bottom,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Clone)]
pub struct Vec2Domain<T: JoinSemiLattice> {
    pub x: T,
    pub y: T,
}

impl<T: JoinSemiLattice> Debug for Vec2Domain<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{{ x: {:?}, y: {:?} }}", self.x, self.y)
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

///////////////////////////////////////
// Product lattices up to 5 elements //
///////////////////////////////////////

macro_rules! tuple_join_semi_lattice {
    ( $prod:ident $( $name:ident )+ ) => {
        paste! {
            #[derive(Clone, PartialEq, Eq, Debug)]
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

                fn join(&self, other: &Self) -> Self {
                    let $prod($([<$name:lower 1>],)+) = self;
                    let $prod($([<$name:lower 2>],)+) = other;
                    $prod($([<$name:lower 1>].join([<$name:lower 2>]),)*)
                }

                fn widen(&self, previous: &Self, iteration: usize) -> Self {
                    let $prod($([<$name:lower 1>],)+) = self;
                    let $prod($([<$name:lower 2>],)+) = previous;
                    $prod(
                    $([<$name:lower 1>].widen([<$name:lower 2>], iteration),)*
                    )
                }
            }

            impl<$($name: Lattice),+> Lattice for $prod<$($name,)+>
            {
                fn top(ctx: &Self::LatticeContext) -> Self {
                    let ($([<$name:lower 1>],)+) = ctx;
                    $prod($($name::top([<$name:lower 1>]),)+)
                }

                fn meet(&self, other: &Self) -> Self {
                    let $prod($([<$name:lower 1>],)+) = self;
                    let $prod($([<$name:lower 2>],)+) = other;
                    $prod($([<$name:lower 1>].meet([<$name:lower 2>]),)*)
                }
            }
        }
    };
}

tuple_join_semi_lattice!(Prod1 D1);
tuple_join_semi_lattice!(Prod2 D1 D2);
tuple_join_semi_lattice!(Prod3 D1 D2 D3);
tuple_join_semi_lattice!(Prod4 D1 D2 D3 D4);
tuple_join_semi_lattice!(Prod5 D1 D2 D3 D4 D5);
