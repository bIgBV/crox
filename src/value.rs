use sharded_slab::Slab;
use thiserror::Error;

use crate::memory::Offset;

#[derive(Debug)]
pub struct Values {
    storage: Slab<f64>,
}

#[derive(Debug)]
pub struct Value(pub f64);

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

    pub fn get(&self, idx: Offset) -> Option<Value> {
        self.storage.get(idx.0).map(|val| Value(*val))
    }
}

#[derive(Debug, Error)]
pub enum ValueError {
    #[error("Value storage is out of memory")]
    OutOfMemory,
}
