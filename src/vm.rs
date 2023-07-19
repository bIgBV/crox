use std::fmt::{self, Debug};
use std::sync::{
    atomic::{AtomicUsize, Ordering},
    RwLock,
};

use scc::{
    ebr::{Arc, Barrier},
    LinkedEntry, Stack,
};
use thiserror::Error;
use tracing::{debug, error, instrument};

use crate::{
    chunk::{Chunk, OpCode},
    memory::{Instruction, Offset},
};

#[derive(Debug)]
pub struct Vm<'chunk> {
    chunk: RwLock<Option<&'chunk Chunk>>,
    // Good enough for now and helps with debugging. If this becomes a bottleneck
    // we can optimize it as all calls are abstracted away behind push and pop
    stack: RwLock<Vec<Offset>>,
    ip: AtomicUsize,
}

type StackEntry = Arc<LinkedEntry<Offset>>;

impl<'chunk> Vm<'chunk> {
    pub fn new() -> Self {
        Vm {
            chunk: RwLock::new(None),
            stack: RwLock::new(Vec::new()),
            ip: AtomicUsize::new(0),
        }
    }

    pub fn interpret(&self, chunk: &'chunk Chunk) -> Result<(), VmError> {
        let _ = self
            .chunk
            .write()
            .expect("Unable to get write lock")
            .insert(chunk);
        self.run()
    }

    pub fn run(&self) -> Result<(), VmError> {
        if let Some(chunk) = *self
            .chunk
            .read()
            .expect("Unable to obtain read lock on chunk")
        {
            self.run_loop(chunk)?
        }

        Ok(())
    }

    fn run_loop(&self, chunk: &Chunk) -> Result<(), VmError> {
        // TODO: Is Relaxed ordering here ok if we are AcqRel within the loop itself?
        while self.ip.load(Ordering::Relaxed) < chunk.code.len() {
            let instruction = self.read_byte(chunk);
            debug!(stack = %self.dump_stack(chunk));
            debug!(instruction = ?instruction);

            match instruction {
                OpCode::Return => {
                    if let Some(value_offset) = self.pop() {
                        let value = chunk.get_value(&value_offset).ok_or(VmError::Runtime)?;
                        debug!(%value);
                        return Ok(());
                    } else {
                        return Err(VmError::Runtime);
                    }
                }
                OpCode::Constant => {
                    let value_offset = self
                        .read_type::<Offset>(chunk)
                        .expect("No constant value found");

                    self.push(value_offset);
                }
            }
        }

        Ok(())
    }

    #[instrument]
    fn read_byte(&self, chunk: &Chunk) -> OpCode {
        chunk.code[self.ip.fetch_add(1, Ordering::AcqRel)].into()
    }

    #[instrument]
    fn read_type<T>(&self, chunk: &Chunk) -> Option<T>
    where
        T: Instruction,
    {
        let ip = self.ip.load(Ordering::Acquire);
        let instruction = ip + T::SIZE;
        let val = T::read_from(&chunk.code[ip..instruction]);
        self.ip.store(instruction, Ordering::Release);

        val
    }

    #[instrument]
    fn push(&self, value_offset: Offset) {
        (*self.stack.write().unwrap()).push(value_offset);
    }

    #[instrument]
    fn pop(&self) -> Option<Offset> {
        (*self.stack.write().unwrap()).pop()
    }

    fn dump_stack(&self, chunk: &Chunk) -> String {
        self.stack
            .read()
            .unwrap()
            .iter()
            .map(|offset| {
                if let Some(value) = chunk.get_value(offset) {
                    format!("[{:#}:{:#}]", offset, value)
                } else {
                    error!(offset = ?offset, "Unable to get value for offset");
                    panic!("DUDE I DON'T KNOW");
                }
            })
            .collect()
    }
}

// TODO we need to be using miette here
#[derive(Debug, Error)]
pub enum VmError {
    #[error("Compiler error")]
    Compile,

    #[error("Runtime error")]
    Runtime,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn read_byte_updates_ip() {
        let vm = Vm::new();
        let mut chunk = Chunk::new("test");
        chunk.write(OpCode::Return, 1);

        let result = vm.interpret(&chunk);

        assert!(result.is_ok());
        assert_eq!(vm.ip.load(Ordering::Relaxed), 1);
    }

    #[test]
    fn read_bytes_updates_ip() {
        let vm = Vm::new();
        let mut chunk = Chunk::new("test");
        chunk.write_constant(6.8, 1).unwrap();

        let result = vm.interpret(&chunk);

        assert!(result.is_ok());
        assert_eq!(
            vm.ip.load(Ordering::Relaxed),
            OpCode::Constant as usize + Offset::SIZE
        );
    }
}
