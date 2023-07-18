use std::{
    ops::Deref,
    sync::{
        atomic::{AtomicUsize, Ordering},
        RwLock,
    },
};

use scc::Stack;
use thiserror::Error;
use tracing::info;
use zerocopy::FromBytes;

use crate::{
    chunk::{Chunk, OpCode},
    memory::{Instruction, Offset},
};

#[derive(Debug)]
pub struct Vm<'chunk> {
    chunk: RwLock<Option<&'chunk Chunk>>,
    stack: Stack<Offset>,
    ip: AtomicUsize,
}

impl<'chunk> Vm<'chunk> {
    pub fn new() -> Self {
        Vm {
            chunk: RwLock::new(None),
            stack: Stack::default(),
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
            match self.read_byte(chunk) {
                OpCode::Return => return Ok(()),
                OpCode::Constant => {
                    let constant = self
                        .read_type::<Offset>(chunk)
                        .and_then(|offset| chunk.get_value(offset))
                        .expect("No constant value found");

                    info!(%constant);
                }
            }
        }

        Ok(())
    }

    fn read_byte(&self, chunk: &Chunk) -> OpCode {
        chunk.code[self.ip.fetch_add(1, Ordering::AcqRel)].into()
    }

    fn read_type<T>(&self, chunk: &Chunk) -> Option<T>
    where
        T: Instruction + FromBytes,
    {
        let ip = self.ip.load(Ordering::Acquire);
        let instruction = ip + T::SIZE;
        let val = T::read_from(&chunk.code[ip..instruction]);
        self.ip.store(instruction, Ordering::Release);

        val
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
