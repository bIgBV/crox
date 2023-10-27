use std::{
    fmt::{write, Display},
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign},
};

use sharded_slab::Slab;
use thiserror::Error;

use crate::memory::Offset;

#[derive(Debug)]
pub struct Values {
    storage: Slab<ValueKind>,
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Value(pub ValueKind);

impl Value {
    pub fn is_number(&self) -> bool {
        true
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum ValueKind {
    Number(f64),
    Nil,
    Bool(bool),
}

impl Display for ValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueKind::Nil => write!(f, "Nil"),
            ValueKind::Number(n) => write!(f, "Number({})", n),
            ValueKind::Bool(b) => write!(f, "Bool({})", b),
        }
    }
}

impl Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self.0 {
            ValueKind::Number(n) => Value(ValueKind::Number(-n)),
            _ => unreachable!("Vm checks for types"),
        }
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self.0, rhs.0) {
            (ValueKind::Number(left), ValueKind::Number(right)) => {
                Value(ValueKind::Number(left + right))
            }
            _ => unreachable!("The VM ensures types match"),
        }
    }
}

impl AddAssign for Value {
    fn add_assign(&mut self, rhs: Self) {
        match (self.0, rhs.0) {
            (ValueKind::Number(left), ValueKind::Number(right)) => {
                *self = Value(ValueKind::Number(left + right));
            }
            _ => unreachable!("The VM ensures types match"),
        }
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self.0, rhs.0) {
            (ValueKind::Number(left), ValueKind::Number(right)) => {
                Value(ValueKind::Number(left - right))
            }
            _ => unreachable!("The VM ensures types match"),
        }
    }
}

impl SubAssign for Value {
    fn sub_assign(&mut self, rhs: Self) {
        match (self.0, rhs.0) {
            (ValueKind::Number(left), ValueKind::Number(right)) => {
                *self = Value(ValueKind::Number(left - right));
            }
            _ => unreachable!("The VM ensures types match"),
        }
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self.0, rhs.0) {
            (ValueKind::Number(left), ValueKind::Number(right)) => {
                Value(ValueKind::Number(left * right))
            }
            _ => unreachable!("The VM ensures types match"),
        }
    }
}

impl MulAssign for Value {
    fn mul_assign(&mut self, rhs: Self) {
        match (self.0, rhs.0) {
            (ValueKind::Number(left), ValueKind::Number(right)) => {
                *self = Value(ValueKind::Number(left * right));
            }
            _ => unreachable!("The VM ensures types match"),
        }
    }
}

impl Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self.0, rhs.0) {
            (ValueKind::Number(left), ValueKind::Number(right)) => {
                Value(ValueKind::Number(left / right))
            }
            _ => unreachable!("The VM ensures types match"),
        }
    }
}

impl DivAssign for Value {
    fn div_assign(&mut self, rhs: Self) {
        match (self.0, rhs.0) {
            (ValueKind::Number(left), ValueKind::Number(right)) => {
                *self = Value(ValueKind::Number(left / right));
            }
            _ => unreachable!("The VM ensures types match"),
        }
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
