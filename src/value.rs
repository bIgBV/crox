use sharded_slab::Slab;

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

    pub fn add_constant(&self, value: Value) -> Result<Offset, ()> {
        if let Some(offset) = self.storage.insert(value.0) {
            Ok(Offset(offset))
        } else {
            Err(())
        }
    }

    pub fn get(&self, idx: Offset) -> Option<Value> {
        self.storage.get(idx.0).map(|val| {
            dbg!(&val);
            Value(*val)
        })
    }
}
