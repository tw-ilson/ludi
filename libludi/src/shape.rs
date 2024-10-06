use smallvec::{smallvec, SmallVec};
use crate::err::Result;

pub type ShapeVec = SmallVec<[usize; 4]>; // Will stay on the stack for up to 4 dimensions.

#[derive(Clone, Hash, Debug, PartialEq, Eq)]
pub struct Shape {
    s: ShapeVec,
}

#[ambassador::delegatable_trait]
pub trait ArrayProps {
    fn shape_slice(&self) -> &[usize];
    fn shape(&self) -> Shape;
    fn rank(&self) -> usize;
    fn cardinality(&self) -> usize;
}

#[ambassador::delegatable_trait]
pub trait ShapeOps {
    fn reshape(&mut self, newshape: &[usize]) -> Result<()>;
}

impl ArrayProps for Shape {
    fn shape_slice(&self) -> &[usize] {
        &self.s
    }
    fn shape(&self) -> Shape {
        self.clone()
    }
    fn rank(&self) -> usize {
        self.s.len()
    }
    fn cardinality(&self) -> usize {
        self.s.iter().sum()
    }
}

impl ShapeOps for Shape {
    fn reshape(&mut self, newshape: &[usize]) -> Result<()> {
        *self = Shape::new(newshape);
        Ok(())
    }
}

impl Shape {
    pub fn new(s: &[usize]) -> Shape {
        Shape {
            s: ShapeVec::from_slice(s),
        }
    }
    pub fn concat(self, other: Self) -> Self {
        Self {
            s: self.s.into_iter().chain(other.s.into_iter()).collect(),
        }
    }
}

impl std::ops::Index<usize> for Shape {
    type Output = usize;
    fn index(&self, index: usize) -> &Self::Output {
        self.s.index(index)
    }
}

impl From<ShapeVec> for Shape {
    fn from(value: ShapeVec) -> Self {
        Self { s: value }
    }
}
pub struct SmallVecDeque {

}
