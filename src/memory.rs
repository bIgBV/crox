use zerocopy::{AsBytes, FromBytes};

#[derive(FromBytes, AsBytes)]
#[repr(C)]
pub struct Offset(pub usize);

pub const OFFSET_SIZE: usize = std::mem::size_of::<Offset>();
