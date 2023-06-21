use zerocopy::{AsBytes, FromBytes};

#[derive(FromBytes, AsBytes)]
#[repr(C)]
pub struct Usize(pub usize);
