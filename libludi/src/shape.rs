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
    fn shape(&self) -> &Shape;
    fn rank(&self) -> usize;
    fn volume(&self) -> usize;
}

#[ambassador::delegatable_trait]
pub trait ShapeOps {
    fn reshape(&mut self, newshape: &[usize]) -> Result<()>;
}
#[ambassador::delegatable_trait]
pub trait Frame {
    // A "view" from one frame to another is the dimension of `self` in cells of shape `other`
    fn view(&self, other: &Self) -> Option<Shape>;
    fn view_unordered(&self, other: &Self) -> Option<Shape> {
        self.view(other).or(other.view(self))
    }
}

impl ArrayProps for Shape {
    fn shape_slice(&self) -> &[usize] {
        &self.s
    }
    fn shape(&self) -> &Shape {
        &self
    }
    fn rank(&self) -> usize {
        self.s.len()
    }
    fn volume(&self) -> usize {
        self.s.iter().product()
    }
}

impl ShapeOps for Shape {
    fn reshape(&mut self, newshape: &[usize]) -> Result<()> {
        *self = Shape::new(newshape);
        Ok(())
    }
}

impl Default for Shape {
    fn default() -> Self {
        Shape::new(&[])
    }
}

impl Shape {
    pub fn new(s: &[usize]) -> Shape {
        Shape {
            s: ShapeVec::from_slice(s),
        }
    }
    pub fn empty() -> Shape {
        Self::default()
    }
    pub fn is_empty(&self) -> bool {
        self.s.is_empty()
    }
    pub fn concat(self, other: Self) -> Self {
        Self {
            s: self.s.into_iter().chain(other.s.into_iter()).collect(),
        }
    }
}

impl Frame for Shape {

    // checks if the other shape is a valid subshape of this (shape agreement)
    // returns the shape of the extra dimensions (the shape of the application)
    // ex: [2 1] <= [2 2 1] so return 2, 
    // ex: [2 1] !<= [2 1 1] so return None
    fn view(&self, other: &Self) -> Option<Shape> {
            if let Some(rank_diff) = self.rank().checked_sub(other.rank()) {
                if &self.shape_slice()[rank_diff..] == other.shape_slice() {
                    return Some(Shape::new(&self.shape_slice()[..rank_diff]))
                } 
            }
            None
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

impl FromIterator<usize> for Shape {
    fn from_iter<T: IntoIterator<Item = usize>>(iter: T) -> Self {
        Shape{ 
            s: iter.into_iter().collect()
        }
    }
}
