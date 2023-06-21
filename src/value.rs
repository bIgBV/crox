use sharded_slab::Slab;

use crate::memory::Usize;

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

    pub fn add_constant(&self, value: Value) -> Result<Usize, ()> {
        if let Some(offset) = self.storage.insert(value.0) {
            Ok(Usize(offset))
        } else {
            Err(())
        }
    }

    pub fn get(&self, idx: Usize) -> Option<Value> {
        self.storage.get(idx.0).and_then(|val| {
            dbg!(&val);
            // todo: this is creating unnecessary copies?
            Some(Value((*val).clone()))
        })
    }
}
