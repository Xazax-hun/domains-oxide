use core::array;
use core::marker::PhantomData;

use crate::domains::*;

use fixedbitset::FixedBitSet;

/// Hold the auxiliary information for a finite lattice encoding partially based on:
/// ```txt
///   H. Aït-Kaci, R. Boyer, P. Lincoln, R. Nasr. Efficient implementation of
///   lattice operations. In ACM Transactions on Programming Languages and
///   Systems (TOPLAS), Volume 11, Issue 1, Jan. 1989, pages 115-146.
/// ```
#[derive(Debug, Clone)]
pub struct FiniteCtx<T: Clone + Eq, const N: usize> {
    elements: [T; N],
    smaller_matrix: [FixedBitSet; N],
    greater_matrix: [FixedBitSet; N],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FiniteDomainError {
    NoLeastUpperBound(usize, usize),
    NoGreatestLowerBound(usize, usize),
    TopNotFirst,
    BottomNotLast,
    LatticeTooSmall,
    HasDuplicateElements,
    NonExistentEdge,
}

impl core::fmt::Display for FiniteDomainError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::error::Error for FiniteDomainError {}

impl<T: Clone + Debug + Eq, const N: usize> FiniteCtx<T, N> {
    /// Similar to [`FiniteCtx::new`], but edges are defined by indices into the first parameter.
    pub fn new_idx(elements: &[T; N], less: &[(usize, usize)]) -> Result<Self, FiniteDomainError> {
        // TODO: in most cases the lattice is known at compile time,
        //       look into making the generation of the context a compile-time
        //       operation.
        #[cfg(debug_assertions)]
        {
            if elements.len() < 2 {
                return Err(FiniteDomainError::LatticeTooSmall);
            }

            if (1..elements.len()).any(|i| elements[i..].contains(&elements[i - 1])) {
                return Err(FiniteDomainError::HasDuplicateElements);
            }

            for &(x, y) in less {
                if x >= elements.len() || y >= elements.len() {
                    return Err(FiniteDomainError::NonExistentEdge);
                }
            }
        }

        let mut result = Self {
            elements: elements.clone(),
            smaller_matrix: array::from_fn(|_| FixedBitSet::with_capacity(N)),
            greater_matrix: array::from_fn(|_| FixedBitSet::with_capacity(N)),
        };

        edge_list_to_matrix(&mut result.smaller_matrix, less);
        reflexive_closure(&mut result.smaller_matrix);
        transitive_closure(&mut result.smaller_matrix);
        transpose(&result.smaller_matrix, &mut result.greater_matrix);

        #[cfg(debug_assertions)]
        {
            result.is_lattice()?;
            if result.smaller_matrix[0] != Finite::top(&result).smaller_than {
                return Err(FiniteDomainError::TopNotFirst);
            }
            if *result.smaller_matrix.last().unwrap() != Finite::bottom(&result).smaller_than {
                return Err(FiniteDomainError::BottomNotLast);
            }
        }

        Ok(result)
    }

    /// Creating a lattice based on the Hasse diagram:
    ///
    /// ```txt
    ///      A
    ///     / \
    ///    B   C
    ///     \ /
    ///      D
    /// ```
    ///
    /// Would be encoded as:
    /// ```
    /// use analysis::domains::FiniteCtx;
    /// #[derive(Debug, Clone, PartialEq, Eq)]
    /// enum E { A, B, C, D }
    /// use E::*;
    /// let ctx = FiniteCtx::new(
    ///   &[A, B, C, D],
    ///   &[(B, A), (C, A), (D, B), (D, C)]).unwrap();
    /// ```
    ///
    /// Top must be the first, Bottom must be the last element.
    ///
    /// Returns None when the input diagram is not a Lattice.
    ///
    /// Use [`FiniteCtx::new_idx`] if the lattice elements are expensive
    /// to copy, or the number of edges is large.
    ///
    /// # Parameters
    ///
    /// * `elements`: List of lattice elements
    /// * `less`: pairs of elements, the first element is smaller than the second.
    pub fn new(elements: &[T; N], less: &[(T, T)]) -> Result<Self, FiniteDomainError> {
        let mut less_indices = Vec::with_capacity(less.len());

        for (x, y) in less {
            let Some(from) = elements.iter().position(|i| i == x) else {
                return Err(FiniteDomainError::NonExistentEdge);
            };
            let Some(to) = elements.iter().position(|i| i == y) else {
                return Err(FiniteDomainError::NonExistentEdge);
            };

            less_indices.push((from, to));
        }

        Self::new_idx(elements, &less_indices)
    }

    pub fn decode(&self, element: &Finite<T, N>) -> &T {
        let pos = self.get_row_index(element);
        &self.elements[pos]
    }

    pub fn encode(&self, element: &T) -> Option<Finite<T, N>> {
        let pos = self.elements.iter().position(|e| e == element)?;
        Some(Finite {
            smaller_than: self.smaller_matrix[pos].clone(),
            phantom: PhantomData,
        })
    }

    pub fn meet(&self, lhs: &Finite<T, N>, rhs: &Finite<T, N>) -> Finite<T, N> {
        let (lhs_idx, rhs_idx) = (self.get_row_index(lhs), self.get_row_index(rhs));
        let lhs_greater = &self.greater_matrix[lhs_idx];
        let rhs_greater = &self.greater_matrix[rhs_idx];
        let mut column = lhs_greater.clone();
        column.intersect_with(rhs_greater);
        let column_idx = self.get_column_index(&Finite {
            smaller_than: column,
            phantom: PhantomData,
        });
        Finite {
            smaller_than: self.smaller_matrix[column_idx].clone(),
            phantom: PhantomData,
        }
    }

    fn get_row_index(&self, element: &Finite<T, N>) -> usize {
        // TODO: make this more efficient by relabeling + leading zeros.
        self.smaller_matrix
            .iter()
            .position(|r| *r == element.smaller_than)
            .expect("Missing element!")
    }

    fn get_column_index(&self, element: &Finite<T, N>) -> usize {
        // TODO: make this more efficient by relabeling + leading zeros.
        self.greater_matrix
            .iter()
            .position(|r| *r == element.smaller_than)
            .expect("Missing element!")
    }

    fn is_lattice(&self) -> Result<(), FiniteDomainError> {
        for i in 0..N {
            for j in (i + 1)..N {
                let mut lub = self.smaller_matrix[i].clone();
                lub.intersect_with(&self.smaller_matrix[j]);
                if !self.smaller_matrix.iter().any(|r| *r == lub) {
                    return Err(FiniteDomainError::NoLeastUpperBound(i, j));
                }
                let mut glb = self.greater_matrix[i].clone();
                glb.intersect_with(&self.greater_matrix[j]);
                if !self.greater_matrix.iter().any(|r| *r == glb) {
                    return Err(FiniteDomainError::NoGreatestLowerBound(i, j));
                }
            }
        }
        Ok(())
    }
}

/// Use [`FiniteCtx`] to generate the encoded lattice elements
/// and [`FiniteCtx::decode`] to get the actual element. The
/// algorithms are running on the encoded representation, the result needs
/// to be decoded. The encoding is really efficient up to 64 elements,
/// the join operation is cheaper than meet.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Finite<T: Clone + Eq, const N: usize> {
    smaller_than: FixedBitSet,
    phantom: PhantomData<T>,
}

impl<T: Clone + Eq, const N: usize> PartialOrd for Finite<T, N> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            return Some(Ordering::Equal);
        }
        if self.smaller_than.is_subset(&other.smaller_than) {
            return Some(Ordering::Greater);
        }
        if self.smaller_than.is_superset(&other.smaller_than) {
            return Some(Ordering::Less);
        }

        None
    }
}

impl<T: Clone + Debug + Eq, const N: usize> JoinSemiLattice for Finite<T, N> {
    type LatticeContext = FiniteCtx<T, N>;

    fn bottom(_: &Self::LatticeContext) -> Self {
        let mut smaller_than = FixedBitSet::with_capacity(N);
        smaller_than.toggle_range(..);
        Self {
            smaller_than,
            phantom: PhantomData,
        }
    }

    fn join(&self, other: &Self, _: &Self::LatticeContext) -> Self {
        let mut smaller_than = self.smaller_than.clone();
        smaller_than.intersect_with(&other.smaller_than);
        Self {
            smaller_than,
            phantom: PhantomData,
        }
    }
}

impl<T: Clone + Debug + Eq, const N: usize> Lattice for Finite<T, N> {
    fn top(_: &Self::LatticeContext) -> Self {
        let mut smaller_than = FixedBitSet::with_capacity(N);
        smaller_than.toggle(0);
        Self {
            smaller_than,
            phantom: PhantomData,
        }
    }

    fn meet(&self, other: &Self, ctx: &Self::LatticeContext) -> Self {
        ctx.meet(self, other)
    }
}

fn edge_list_to_matrix<const N: usize>(empty: &mut [FixedBitSet; N], edges: &[(usize, usize)]) {
    for &(from, to) in edges {
        empty[from].insert(to);
    }
}

fn reflexive_closure<const N: usize>(bool_matrix: &mut [FixedBitSet; N]) {
    for (i, item) in bool_matrix.iter_mut().enumerate() {
        item.insert(i);
    }
}

fn transitive_closure<const N: usize>(bool_matrix: &mut [FixedBitSet; N]) {
    // TODO: there might be a more efficient way using matrix multiplication
    for k in 0..N {
        for i in 0..N {
            for j in 0..N {
                if bool_matrix[i].contains(k) && bool_matrix[k].contains(j) {
                    bool_matrix[i].insert(j);
                }
            }
        }
    }
}

fn transpose<const N: usize>(from: &[FixedBitSet; N], to: &mut [FixedBitSet; N]) {
    #[allow(clippy::needless_range_loop)]
    for i in 0..N {
        for j in 0..N {
            to[j].set(i, from[i].contains(j));
        }
    }
}
