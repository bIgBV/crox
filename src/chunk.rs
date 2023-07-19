use std::{
    fmt::Display,
    sync::atomic::{AtomicUsize, Ordering},
};

use anyhow::Context;
use thiserror::Error;
use zerocopy::{AsBytes, FromBytes};

use crate::{
    line_store::LineStore,
    memory::{Instruction, Offset, OFFSET_SIZE},
    value::{Value, ValueError, Values},
};

/// A chunk of bytecode.
#[derive(Debug)]
pub struct Chunk {
    name: &'static str,
    values: Values,
    pub code: Vec<u8>,
    lines: LineStore,
}

/// An enumeration of all operations supported by our instruction set.
#[repr(u8)]
#[derive(Debug)]
pub enum OpCode {
    /// Return from a function.
    Return = 0,

    /// Represents a constant in the instruction stream. Constants are not
    /// stored in the stream itself, but in a global value store (see
    /// [`Values`]) and the [`Offset`] into the store is stored in the
    /// bytestream, as a suffix to the opcode.
    ///
    /// An [`Offset`] is internally represented as a usize (usually 8 bytes),
    /// therefore, we can store up to usize::MAX constants. But we will probably
    /// run out of memory long before then
    Constant = 1,
}

impl From<u8> for OpCode {
    fn from(value: u8) -> Self {
        match value {
            0 => OpCode::Return,
            1 => OpCode::Constant,
            _ => unimplemented!("We need more op codes"),
        }
    }
}

impl Chunk {
    pub fn new(name: &'static str) -> Self {
        Chunk {
            name,
            values: Values::new(),
            code: Vec::new(),
            lines: LineStore::new(),
        }
    }

    /// Write a single opcode into the bytestream and associate it with a line
    /// from the source code.
    pub fn write(&mut self, byte: OpCode, line: usize) {
        self.code.push(byte as u8);
        self.lines.add_byte(line);
    }

    /// Write a constant to the bytestream.
    pub fn write_constant(&mut self, constant: f64, line: usize) -> Result<(), ChunkError> {
        let offset = self.values.add_constant(Value(constant))?;
        self.write(OpCode::Constant, line);
        self.write_bytes(offset.as_bytes(), line);
        Ok(())
    }

    pub fn get_value(&self, offset: &Offset) -> Option<Value> {
        self.values.get(*offset)
    }

    fn write_bytes(&mut self, bytes: &[u8], line: usize) {
        self.code.extend_from_slice(bytes);
        self.lines.add_bytes(line, bytes.len());
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer = String::new();
        buffer.push_str(&format!("\n==== {} ====\n", self.name));

        let formatter = ChunkFormatter::from_chunk(self);
        if let Ok(()) = formatter.format(&mut buffer) {
            f.write_str(&buffer)
        } else {
            write!(f, "Unable to format chunk: {}", self.name)
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ChunkError {
    #[error("Constant store: {0}")]
    ConstantStore(#[from] ValueError),
}

/// Formats a given chunk
pub struct ChunkFormatter<'chunk> {
    chunk: &'chunk Chunk,
    current_line: AtomicUsize,
}

impl<'chunk> ChunkFormatter<'chunk> {
    pub fn from_chunk(chunk: &'chunk Chunk) -> Self {
        ChunkFormatter {
            chunk,
            current_line: AtomicUsize::new(0),
        }
    }

    pub fn format(&self, buffer: &mut String) -> anyhow::Result<()> {
        let mut offset = 0;

        while offset < self.chunk.code.len() {
            let byte_offset = Offset(offset);
            let src_line = self
                .chunk
                .lines
                .get_line(byte_offset)
                .ok_or(FormatterError::NoLine(byte_offset))?;

            self.current_line.store(src_line, Ordering::Release);

            offset = match self.chunk.code[offset] {
                0 => self
                    .simple_instruction(buffer, "OP_RETURN", offset, src_line)
                    .with_context(|| {
                        format!(
                            "Unable to parse simple instruction in chunk: {}",
                            self.chunk.name
                        )
                    })?,
                1 => self
                    .constant_instruction(buffer, "OP_CONSTANT", offset, src_line)
                    .with_context(|| {
                        format!(
                            "Unable to parse simple instruction in chunk: {}",
                            self.chunk.name
                        )
                    })?,

                _ => todo!(),
            };
        }

        Ok(())
    }

    fn simple_instruction(
        &self,
        buffer: &mut String,
        op_name: &'static str,
        offset: usize,
        line: usize,
    ) -> Result<usize, FormatterError> {
        buffer.push_str(&format!("{:>4}", offset));
        self.insert_line(offset, buffer, line);
        buffer.push_str(&format!(" {}\n", op_name));

        Ok(offset + 1)
    }

    fn constant_instruction(
        &self,
        buffer: &mut String,
        op_name: &'static str,
        offset: usize,
        line: usize,
    ) -> Result<usize, FormatterError> {
        buffer.push_str(&format!("{:>4}", offset));
        self.insert_line(offset, buffer, line);
        buffer.push_str(&format!(" {}", op_name));

        let idx_offset = offset + 1;
        let parsed_offset =
            Offset::read_from(&self.chunk.code[idx_offset..idx_offset + Offset::SIZE]);

        if let Some(idx) = parsed_offset {
            if let Some(val) = self.chunk.values.get(idx) {
                buffer.push_str(&format!(" {}\n", val.0));
            } else {
                return Err(FormatterError::Value(idx));
            }
        } else {
            return Err(FormatterError::OffsetParse(offset));
        }

        Ok(offset + 1 + OFFSET_SIZE)
    }

    fn insert_line(&self, offset: usize, buffer: &mut String, line: usize) {
        if offset > 0 && line == self.current_line.load(Ordering::Acquire) {
            buffer.push_str("   |");
        } else {
            buffer.push_str(&format!("{:>4}", line))
        }
    }
}

/// Possible errors encountered during formatting a chunk
#[derive(Error, Debug)]
enum FormatterError {
    #[error("Unable to parse Value offset at {0}")]
    OffsetParse(usize),

    #[error("Unable to get value with idx {0} from value store")]
    Value(Offset),

    #[error("Unable to find associated line for byte offset {0}")]
    NoLine(Offset),
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn format_op_return() {
        let mut chunk = Chunk::new("test");
        chunk.write(OpCode::Return, 1);

        let formatter = ChunkFormatter::from_chunk(&chunk);

        let mut buffer = String::new();
        let result = formatter.format(&mut buffer);

        assert!(result.is_ok());
        assert_eq!(buffer, "   0   1 OP_RETURN\n");
    }

    #[test]
    fn format_op_constant() {
        let mut chunk = Chunk::new("test");
        chunk.write(OpCode::Return, 1);
        chunk.write_constant(5.0, 1).unwrap();

        let formatter = ChunkFormatter::from_chunk(&chunk);

        let mut buffer = String::new();
        let result = formatter.format(&mut buffer);

        assert!(result.is_ok());
        assert_eq!(buffer, "   0   1 OP_RETURN\n   1   | OP_CONSTANT 5\n");
    }
}
