use std::fmt::Debug;
use std::sync::{
    atomic::{AtomicUsize, Ordering},
    RwLock,
};

use thiserror::Error;
use tracing::{debug, error, instrument};

use crate::chunk::ChunkError;
use crate::compiler::{Compiler, CompilerError};
use crate::value::Value;
use crate::{
    chunk::{Chunk, OpCode},
    memory::{Instruction, Offset},
};

#[derive(Debug)]
pub struct Vm {
    // Good enough for now and helps with debugging. If this becomes a bottleneck
    // we can optimize it as all calls are abstracted away behind push and pop
    stack: RwLock<Vec<Offset>>,
    ip: AtomicUsize,
}

impl Vm {
    pub fn new() -> Self {
        Vm {
            stack: RwLock::new(Vec::new()),
            ip: AtomicUsize::new(0),
        }
    }

    pub fn interpret(&self, line: String) -> Result<(), VmError> {
        let compiler = Compiler::new();

        let chunk = compiler.complie(&line)?;
        self.run_loop(&chunk)?;

        Ok(())
    }

    fn run_loop(&self, chunk: &Chunk) -> Result<(), VmError> {
        // TODO: Is Relaxed ordering here ok if we are AcqRel within the loop itself?
        while self.ip.load(Ordering::Relaxed) < chunk.code.len() {
            let instruction = self.read_byte(chunk);
            debug!(instruction = ?instruction, stack = %self.dump_stack(chunk));

            match instruction {
                OpCode::Return => {
                    let value = chunk.get_value(&self.pop())?;
                    debug!(%value);
                    return Ok(());
                }
                OpCode::Constant => {
                    let value_offset = self
                        .read_type::<Offset>(chunk)
                        .expect("No constant value found");

                    self.push(value_offset);
                }
                OpCode::Negate => {
                    let value = chunk.get_value(&self.pop())?;
                    self.push(chunk.add_value(Value(-value.0)).unwrap());
                }
                OpCode::Add => self.binary_op(OpCode::Add, chunk)?,
                OpCode::Subtract => self.binary_op(OpCode::Subtract, chunk)?,
                OpCode::Multiply => self.binary_op(OpCode::Multiply, chunk)?,
                OpCode::Divide => self.binary_op(OpCode::Divide, chunk)?,
            }
        }

        Ok(())
    }

    #[instrument]
    fn read_byte(&self, chunk: &Chunk) -> OpCode {
        // TODO: This might fail as we load the IP using Ordering::Relaxed, and another thread might
        // have gotten updated the IP to the end of the chunk before us.
        // How do you handle this?
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

    fn binary_op(&self, op: OpCode, chunk: &Chunk) -> Result<(), VmError> {
        let b = chunk.get_value(&self.pop())?;
        let a = chunk.get_value(&self.pop())?;

        match op {
            OpCode::Add => self.push(chunk.add_value(b + a)?),
            OpCode::Subtract => self.push(chunk.add_value(b - a)?),
            OpCode::Multiply => self.push(chunk.add_value(b * a)?),
            OpCode::Divide => self.push(chunk.add_value(b / a)?),
            _ => panic!("This should never happen"),
        };

        Ok(())
    }

    #[instrument]
    fn push(&self, value_offset: Offset) {
        (*self.stack.write().unwrap()).push(value_offset);
    }

    #[instrument]
    fn pop(&self) -> Offset {
        debug_assert!(
            self.stack.read().unwrap().len() >= 1,
            "Stack popped with no items"
        );

        (*self.stack.write().unwrap()).pop().unwrap()
    }

    fn dump_stack(&self, chunk: &Chunk) -> String {
        self.stack
            .read()
            .unwrap()
            .iter()
            .map(|offset| {
                let value = chunk.get_value(offset).expect("Error getting value");
                format!("[{:#}:{:#}]", offset, value)
            })
            .collect()
    }
}

// TODO we need to be using miette here
#[derive(Debug, Error)]
pub enum VmError {
    #[error("Compiler error: {0}")]
    Compile(#[from] CompilerError),

    #[error("Runtime error")]
    Runtime,

    #[error("Error with value store: {0}")]
    ChunkError(#[from] ChunkError),
}

#[cfg(test)]
mod test {
    use super::*;

    // #[test]
    // fn read_byte_updates_ip() {
    //     let vm = Vm::new();
    //     let mut chunk = Chunk::new("test");
    //     chunk.write(OpCode::Return, 1);

    //     let result = vm.interpret(&chunk);

    //     assert!(result.is_ok());
    //     assert_eq!(vm.ip.load(Ordering::Relaxed), 1);
    // }

    // #[test]
    // fn read_bytes_updates_ip() {
    //     let vm = Vm::new();
    //     let mut chunk = Chunk::new("test");
    //     chunk.write_constant(6.8, 1).unwrap();

    //     let result = vm.interpret(&chunk);

    //     assert!(result.is_ok());
    //     assert_eq!(
    //         vm.ip.load(Ordering::Relaxed),
    //         OpCode::Constant as usize + Offset::SIZE
    //     );
    // }
}
