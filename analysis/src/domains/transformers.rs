use crate::domains::*;

/////////////////////////
// Domain transformers //
/////////////////////////

// TODO:
// Add operations to built lattices
// * Product, Pair
// * Reduced product
// * Disjoint union
// * Stacking
// * Lifting
// * Finite lattices

// TODO:
// Add more general building blocks for finite domains and
// port SignDomain to those facilities.


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

