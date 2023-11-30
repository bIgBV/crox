use std::fmt::Display;

use zerocopy::{AsBytes, FromBytes};

#[derive(FromBytes, AsBytes, Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct Offset(pub usize);

pub const OFFSET_SIZE: usize = std::mem::size_of::<Offset>();

impl Display for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Instruction for Offset {
    const SIZE: usize = std::mem::size_of::<Offset>();
}

pub trait Instruction: FromBytes {
    const SIZE: usize;
}
