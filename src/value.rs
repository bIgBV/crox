use sharded_slab::Slab;
use thiserror::Error;

use crate::memory::Offset;

pub struct Values {
    storage: Slab<f64>,
}

pub struct Value(pub f64);

impl Values {
    pub fn new() -> Self {
        Self {
            storage: Slab::new(),
        }
    }

    pub fn add_constant(&self, value: Value) -> Result<Offset, ValueError> {
        self.storage
            .insert(value.0)
            .map(|val| Offset(val))
            .ok_or(ValueError::OutOfMemory)
    }

    pub fn get(&self, idx: Offset) -> Option<Value> {
        self.storage.get(idx.0).map(|val| {
            dbg!(&val);
            Value(*val)
        })
    }
}

#[derive(Debug, Error)]
pub enum ValueError {
    #[error("Value storage is out of memory")]
    OutOfMemory,
}
