use std::sync::atomic::{AtomicUsize, Ordering};

use thiserror::Error;
use tracing::info;
use zerocopy::FromBytes;

use crate::{
    chunk::{Chunk, OpCode},
    memory::{Instruction, Offset},
};

#[derive(Debug)]
pub struct Vm<'chunk> {
    chunk: Option<&'chunk mut Chunk>,
    ip: AtomicUsize,
}

impl<'chunk> Vm<'chunk> {
    pub fn new() -> Self {
        Vm {
            chunk: None,
            ip: AtomicUsize::new(0),
        }
    }

    pub fn interpret(&mut self, chunk: &'chunk mut Chunk) -> Result<(), VmError> {
        self.chunk = Some(chunk);
        self.run()
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        loop {
            match self.read_byte().expect("Unable to read instruction") {
                OpCode::Return => return Ok(()),
                OpCode::Constant => {
                    let constant = self
                        .read_type::<Offset>()
                        .and_then(|offset| {
                            self.chunk
                                .as_ref()
                                .and_then(|chunk| chunk.get_value(offset))
                        })
                        .expect("No constant value found");

                    info!(%constant);
                }
            }
        }
    }

    fn read_byte(&self) -> Option<OpCode> {
        self.chunk
            .as_ref()
            .map(|chunk| chunk.code[self.ip.fetch_add(1, Ordering::AcqRel)])
            .map(|byte| byte.into())
    }

    fn read_type<T>(&mut self) -> Option<T>
    where
        T: Instruction + FromBytes,
    {
        self.chunk.as_mut().and_then(|chunk| {
            let ip = self.ip.load(Ordering::Acquire);
            let instruction = ip + T::SIZE;
            let val = T::read_from(&mut chunk.code[ip..instruction]);
            self.ip.store(instruction, Ordering::Release);

            val
        })
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
