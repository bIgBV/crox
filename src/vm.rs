use std::fmt::Debug;
use std::sync::{
    atomic::{AtomicUsize, Ordering},
    RwLock,
};

use thiserror::Error;
use tracing::{debug, error, instrument, trace};

use crate::chunk::ChunkError;
use crate::compiler::{compile, CompilerError};
use crate::value::{Value, ValueError, ValueKind, ValueType};
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

    #[instrument(skip(self))]
    pub fn interpret(&self, line: String) -> Result<(), VmError> {
        let chunk = compile(&line)?;
        self.run_loop(&chunk)?;

        Ok(())
    }

    #[instrument(skip_all)]
    fn run_loop(&self, chunk: &Chunk) -> Result<(), VmError> {
        // TODO: Is Relaxed ordering here ok if we are AcqRel within the loop itself?
        debug!(%chunk, "interpreting chunk");

        debug_assert!(self.stack.read().unwrap().len() == 0, "Stack isn't clear yet");
        debug_assert!(self.ip.load(Ordering::Acquire) == 0, "IP isn't set to 0");

        while self.ip.load(Ordering::Acquire) < chunk.code.read().unwrap().len() {
            let instruction = self.read_byte(chunk);
            debug!(instruction = ?instruction, stack = %self.dump_stack(chunk));

            match instruction {
                OpCode::Return => {
                    let value = chunk.take_value(self.pop())?;
                    debug!(%value);
                    println!("{:#}", value);
                    return Ok(());
                }
                OpCode::Constant => {
                    let value_offset = self
                        .read_type::<Offset>(chunk)
                        .expect("No constant value found");

                    self.push(value_offset);
                }
                OpCode::Negate => self.op_neg(chunk, instruction)?,
                OpCode::Add => self.op_add(chunk)?,
                OpCode::Subtract => self.binary_op(OpCode::Subtract, chunk)?,
                OpCode::Multiply => self.binary_op(OpCode::Multiply, chunk)?,
                OpCode::Divide => self.binary_op(OpCode::Divide, chunk)?,
                OpCode::Not => {
                    // Take ownership of the value
                    let updated_value = chunk.take_value(self.pop())?.is_falsey().into();
                    self.push(chunk.add_value(updated_value)?)
                }
                OpCode::Nil => self.push(chunk.add_value(Value::new_nil())?),
                OpCode::False => self.push(chunk.add_value(Value::new_bool(false))?),
                OpCode::True => self.push(chunk.add_value(Value::new_bool(true))?),
                OpCode::Equal => {
                    let a = chunk.get_value(&self.pop())?;
                    let b = chunk.get_value(&self.pop())?;

                    self.push(chunk.add_value((a == b).into())?)
                }
                OpCode::Greater => self.binary_op(OpCode::Greater, chunk)?,
                OpCode::Less => self.binary_op(OpCode::Less, chunk)?,
            }
        }

        self.stack.write().unwrap().clear();
        self.ip.store(0, Ordering::Release);

        Ok(())
    }

    #[instrument(skip(self))]
    fn read_byte(&self, chunk: &Chunk) -> OpCode {
        chunk.code.read().unwrap()[self.ip.fetch_add(1, Ordering::AcqRel)].into()
    }

    #[instrument(skip(self))]
    fn read_type<T>(&self, chunk: &Chunk) -> Option<T>
    where
        T: Instruction,
    {
        let ip = self.ip.load(Ordering::Acquire);
        let instruction = ip + T::SIZE;
        let val = T::read_from(&chunk.code.read().unwrap()[ip..instruction]);
        self.ip.store(instruction, Ordering::Release);

        val
    }

    #[instrument(skip_all)]
    fn op_add(&self, chunk: &Chunk) -> Result<(), VmError> {
        let value_type = self.peek_with_bin(0, 1, |first, second| {
            let first = chunk.get_value(first)?;
            let second = chunk.get_value(second)?;

            if first.is_string() && second.is_string() {
                Ok(ValueType::Obj)
            } else if first.is_number() && second.is_number() {
                Ok(ValueType::Num)
            } else {
                Err(operator_error(
                    &format!("{}", OpCode::Add),
                    first.value_type(),
                    second.value_type(),
                ))
            }
        })?;

        match value_type {
            ValueType::Obj => todo!(),
            _ => self.binary_op(OpCode::Add, chunk)?,
        };

        Ok(())
    }

    #[instrument(skip_all)]
    fn concatenate(&self, chunk: &Chunk) -> Result<(), VmError> {
        let left = chunk
            .take_value(self.pop())?
            .take()
            .ok_or_else(|| VmError::Value)?;
        let right = chunk
            .take_value(self.pop())?
            .take()
            .ok_or_else(|| VmError::Value)?;

        Ok(())
    }

    #[instrument(skip_all)]
    fn op_neg(&self, chunk: &Chunk, opcode: OpCode) -> Result<(), VmError> {
        trace!("Executing OpCode::Neg");
        self.peek_with(0, |offset| {
            let value = chunk.get_value(offset)?;
            if !value.is_number() {
                return Err(VmError::UnaryOperatorMismatch {
                    op: format!("{}", opcode),
                    right_ty: value.value_type().to_string(),
                });
            }
            Ok(())
        })?;
        let value = chunk.take_value(self.pop())?;
        self.push(chunk.add_value(-value).unwrap());

        Ok(())
    }

    #[instrument(skip(self, chunk))]
    fn binary_op(&self, op: OpCode, chunk: &Chunk) -> Result<(), VmError> {
        self.peek_with_bin(0, 1, |first, second| {
            let first = chunk.get_value(first)?;
            let second = chunk.get_value(second)?;

            if !first.is_number() || !second.is_number() {
                return Err(operator_error(
                    &format!("{}", op),
                    // We need to flip the order since we are working with values from a stack.
                    second.value_type(),
                    first.value_type(),
                ));
            }

            Ok(ValueType::Num)
        })?;

        // Ensure we take ownership of the value here.
        let b = chunk.take_value(self.pop())?;
        let a = chunk.take_value(self.pop())?;

        let result = match op {
            OpCode::Add => b + a,
            OpCode::Subtract => b - a,
            OpCode::Multiply => b * a,
            OpCode::Divide => b / a,
            OpCode::Greater => (a > b).into(),
            OpCode::Less => (a < b).into(),
            _ => panic!("This should never happen"),
        };

        self.push(chunk.add_value(result)?);

        Ok(())
    }

    #[instrument]
    fn push(&self, value_offset: Offset) {
        (*self.stack.write().unwrap()).push(value_offset);
    }

    fn pop(&self) -> Offset {
        debug_assert!(
            self.stack.read().unwrap().len() >= 1,
            "Stack popped with no items"
        );

        (*self.stack.write().unwrap()).pop().unwrap()
    }

    /// Peeks into the stack and validates whether it with the predicate while
    /// holding the read lock on the stack
    fn peek_with<F>(&self, distance: usize, check: F) -> Result<(), VmError>
    where
        F: FnOnce(&Offset) -> Result<(), VmError>,
    {
        debug_assert!(
            self.stack.read().unwrap().len() >= 1,
            "Peeking into the stack without any items"
        );

        let lock_guard = self.stack.read().unwrap();
        let offset = lock_guard
            .iter()
            .rev()
            .skip(distance)
            .next()
            .expect("we confirmed that items were present");

        check(offset)
    }

    fn peek_with_bin<F>(&self, first: usize, second: usize, check: F) -> Result<ValueType, VmError>
    where
        F: FnOnce(&Offset, &Offset) -> Result<ValueType, VmError>,
    {
        debug_assert!(
            self.stack.read().unwrap().len() >= 1,
            "Peeking into the stack without any items"
        );

        let lock_guard = self.stack.read().unwrap();
        let first = lock_guard
            .iter()
            .rev()
            .skip(first)
            .next()
            .expect("we confirmed that items were present");
        let second = lock_guard
            .iter()
            .rev()
            .skip(second)
            .next()
            .expect("There should be more items present");

        check(first, second)
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

    #[error("Type mismatch during operation: {op} Left: {left_ty} Right: {right_ty}")]
    OperatorMismatch {
        op: String,
        left_ty: ValueType,
        right_ty: ValueType,
    },

    #[error("Type mismatch during operation: {op} Right: {right_ty}")]
    UnaryOperatorMismatch { op: String, right_ty: String },

    #[error("Error with value store: {0}")]
    ChunkError(#[from] ChunkError),

    #[error("Value error")]
    Value,
}

fn operator_error(op: &str, left_ty: ValueType, right_ty: ValueType) -> VmError {
    VmError::OperatorMismatch {
        op: op.to_string(),
        left_ty,
        right_ty,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn read_byte_updates_ip() {
        let vm = Vm::new();
        let line = "true";
        let result = vm.interpret(line.to_string());

        assert!(result.is_ok());
        assert_eq!(vm.ip.load(Ordering::Relaxed), 2);
    }

    #[test]
    fn read_bytes_updates_ip() {
        let vm = Vm::new();
        let line = "8.5".to_string();

        let result = vm.interpret(line);

        assert!(result.is_ok());
        assert_eq!(
            vm.ip.load(Ordering::Relaxed),
            OpCode::Constant as usize + Offset::SIZE + 1
        );
    }

    #[test]
    fn repeat_executions() {
        let vm = Vm::new();
        let line = "(6 <= (7 == 5))".to_string();

        let result = vm.interpret(line.clone());
        let Err(VmError::OperatorMismatch {
            op: _,
            left_ty: _,
            right_ty: _,
        }) = result
        else {
            panic!("Unexpected error");
        };

        let result = vm.interpret(line.clone());
        let Err(VmError::OperatorMismatch {
            op: _,
            left_ty: _,
            right_ty: _,
        }) = result
        else {
            panic!("Unexpected error");
        };

        let result = vm.interpret(line);
        let Err(VmError::OperatorMismatch {
            op: _,
            left_ty: _,
            right_ty: _,
        }) = result
        else {
            panic!("Unexpected error");
        };
    }
}
