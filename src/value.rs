use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Sub},
};

use sharded_slab::{Entry, Slab};
use thiserror::Error;
use tracing::{error, instrument};

use crate::memory::Offset;

#[derive(Debug)]
pub struct Values {
    storage: Slab<ValueKind>,
}

impl Values {
    pub fn new() -> Self {
        Self {
            storage: Slab::new(),
        }
    }

    #[instrument(skip(self))]
    pub fn add(&self, value: Value<'_>) -> Result<Offset, ValueError> {
        let Some(kind) = value.kind() else {
            error!("Cannot add an existing entry back to storage");
            return Err(ValueError::ValueExists);
        };

        self.storage
            .insert(kind)
            .map(Offset)
            .ok_or(ValueError::OutOfMemory)
    }

    /// Take ownership of the value at the given index
    pub fn take(&self, idx: Offset) -> Result<Value<'_>, ValueError> {
        self.storage
            .take(idx.0)
            .map(|val| Value::Owned(val))
            .ok_or(ValueError::NotFound(idx))
    }

    /// Get a reference to the value at the given index
    pub fn get(&self, idx: Offset) -> Result<Value<'_>, ValueError> {
        // make a copy of the value in the store. Could probably optimized
        self.storage
            .get(idx.0)
            .map(|val| Value::Entry(val))
            .ok_or(ValueError::NotFound(idx))
    }
}

/// Represents a lox value.
///
/// This can be either an owned value or a reference to an entry in the slab.
///
/// The arithmentic operators implemented on this type always return a new
/// `Owned` value as semantically it fits the model of these math operators in
/// my head
#[derive(Debug)]
pub enum Value<'a> {
    Owned(ValueKind),
    Entry(Entry<'a, ValueKind>),
}

impl Add for Value<'_> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Value::Owned(self.as_ref() + rhs.as_ref())
    }
}

impl Sub for Value<'_> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Value::Owned(self.as_ref() - rhs.as_ref())
    }
}

impl Mul for Value<'_> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Value::Owned(self.as_ref() * rhs.as_ref())
    }
}

impl Div for Value<'_> {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Value::Owned((self.as_ref() / rhs.as_ref()).clone())
    }
}

impl PartialEq for Value<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().eq(other.as_ref())
    }
}

impl PartialOrd for Value<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_ref().partial_cmp(other.as_ref())
    }
}

impl Neg for Value<'_> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Value::Owned((-self.as_ref()).clone())
    }
}

impl<'a> Value<'a> {
    pub fn as_ref(&self) -> &ValueKind {
        match self {
            Value::Owned(val) => &val,
            Value::Entry(en) => &en,
        }
    }

    pub fn new_nil() -> Self {
        Value::Owned(ValueKind::Nil)
    }

    pub fn new_bool(val: bool) -> Self {
        Value::Owned(ValueKind::Bool(val))
    }

    pub fn new_num(num: f64) -> Self {
        Value::Owned(ValueKind::Number(num))
    }

    pub fn new_string<S>(str: S) -> Self
    where
        S: Into<String>,
    {
        Value::Owned(ValueKind::Obj(Object {
            kind: ObjectKind::String(str.into()),
        }))
    }

    pub fn is_number(&self) -> bool {
        self.as_ref().is_number()
    }

    pub fn is_string(&self) -> bool {
        self.as_ref().is_string()
    }

    /// Lox follows Ruby in that nil and false are falsey and every other value
    /// behaves like true.
    pub fn is_falsey(&self) -> bool {
        self.as_ref().is_falsey()
    }

    pub fn value_type(&self) -> ValueType {
        self.as_ref().value_type()
    }

    pub fn kind(self) -> Option<ValueKind> {
        match self {
            Value::Entry(_) => None,
            Value::Owned(kind) => Some(kind),
        }
    }

    pub fn take(self) -> Option<ValueKind> {
        // We can only take an owned value
        debug_assert!(matches!(self, Value::Owned(_)));

        match self {
            Value::Owned(val) => Some(val),
            _ => None,
        }
    }
}

impl ValueKind {
    pub fn is_number(&self) -> bool {
        match self {
            ValueKind::Number(_) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            ValueKind::Obj(obj) => obj.is_string(),
            _ => false,
        }
    }

    /// Lox follows Ruby in that nil and false are falsey and every other value
    /// behaves like true.
    pub fn is_falsey(&self) -> bool {
        match self {
            ValueKind::Nil => true,
            ValueKind::Bool(b) => !b,
            _ => false,
        }
    }

    pub fn value_type(&self) -> ValueType {
        match self {
            ValueKind::Bool(_) => ValueType::Bool,
            ValueKind::Nil => ValueType::Nil,
            ValueKind::Number(_) => ValueType::Num,
            ValueKind::Obj(_) => ValueType::Obj,
        }
    }
}

/// Enumeration of possible types a Lox [`Value`] can take
#[derive(Debug)]
pub enum ValueType {
    Bool,
    Nil,
    Num,
    Obj,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::Bool => write!(f, "{}", "bool"),
            ValueType::Nil => write!(f, "{}", "nil"),
            ValueType::Num => write!(f, "{}", "num"),
            ValueType::Obj => write!(f, "{}", "obj"),
        }
    }
}

impl From<bool> for Value<'_> {
    fn from(value: bool) -> Self {
        Value::Owned(ValueKind::Bool(value))
    }
}

impl From<f64> for Value<'_> {
    fn from(value: f64) -> Self {
        Value::Owned(ValueKind::Number(value))
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum ValueKind {
    Bool(bool),
    Number(f64),
    Nil,
    Obj(Object),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Object {
    kind: ObjectKind,
}

impl Object {
    fn is_string(&self) -> bool {
        match self.kind {
            ObjectKind::String(_) => true,
            //_ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum ObjectKind {
    String(String),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for ObjectKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectKind::String(str) => write!(f, "{{String: {}}}", str),
        }
    }
}

impl Display for ValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueKind::Nil => write!(f, "Nil"),
            ValueKind::Number(n) => write!(f, "{}", n),
            ValueKind::Bool(b) => write!(f, "{}", b),
            ValueKind::Obj(obj) => write!(f, "{}", obj),
        }
    }
}

impl Neg for &ValueKind {
    type Output = ValueKind;

    fn neg(self) -> Self::Output {
        match self {
            ValueKind::Number(n) => ValueKind::Number(-n),
            _ => unreachable!("Vm checks for types"),
        }
    }
}

impl Add for &ValueKind {
    type Output = ValueKind;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ValueKind::Number(left), ValueKind::Number(right)) => ValueKind::Number(left + right),
            _ => unreachable!("The VM ensures types match"),
        }
    }
}

impl Sub for &ValueKind {
    type Output = ValueKind;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ValueKind::Number(left), ValueKind::Number(right)) => ValueKind::Number(left - right),
            _ => unreachable!("The VM ensures types match"),
        }
    }
}

impl Mul for &ValueKind {
    type Output = ValueKind;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ValueKind::Number(left), ValueKind::Number(right)) => ValueKind::Number(left * right),
            _ => unreachable!("The VM ensures types match"),
        }
    }
}

impl Div for &ValueKind {
    type Output = ValueKind;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ValueKind::Number(left), ValueKind::Number(right)) => ValueKind::Number(left / right),
            _ => unreachable!(""),
        }
    }
}

impl std::fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

#[derive(Debug, Error)]
pub enum ValueError {
    #[error("Value storage is out of memory")]
    OutOfMemory,

    #[error("Value for Offset: {0} not found")]
    NotFound(Offset),

    #[error("Value already exists in the slab")]
    ValueExists,
}
