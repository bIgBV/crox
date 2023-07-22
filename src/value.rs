use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign};

use sharded_slab::Slab;
use thiserror::Error;

use crate::memory::Offset;

#[derive(Debug)]
pub struct Values {
    storage: Slab<f64>,
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Value(pub f64);

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Value(self.0 + rhs.0)
    }
}

impl AddAssign for Value {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Value(self.0 - rhs.0)
    }
}

impl SubAssign for Value {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 -= rhs.0
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Value(self.0 * rhs.0)
    }
}

impl MulAssign for Value {
    fn mul_assign(&mut self, rhs: Self) {
        self.0 *= rhs.0
    }
}

impl Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Value(self.0 / rhs.0)
    }
}

impl DivAssign for Value {
    fn div_assign(&mut self, rhs: Self) {
        self.0 /= rhs.0
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Values {
    pub fn new() -> Self {
        Self {
            storage: Slab::new(),
        }
    }

    pub fn add_constant(&self, value: Value) -> Result<Offset, ValueError> {
        self.storage
            .insert(value.0)
            .map(Offset)
            .ok_or(ValueError::OutOfMemory)
    }

    pub fn get(&self, idx: Offset) -> Result<Value, ValueError> {
        // make a copy of the value in the store. Could probably optimized
        self.storage
            .get(idx.0)
            .map(|val| Value(*val))
            .ok_or(ValueError::NotFound(idx))
    }
}

#[derive(Debug, Error)]
pub enum ValueError {
    #[error("Value storage is out of memory")]
    OutOfMemory,

    #[error("Value for Offset: {0} not found")]
    NotFound(Offset),
}
