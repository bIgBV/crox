use std::fmt::Display;

use anyhow::Context;
use thiserror::Error;
use zerocopy::{AsBytes, FromBytes};

use crate::{
    memory::{Offset, OFFSET_SIZE},
    value::{Value, Values},
};

pub struct Chunk {
    name: &'static str,
    values: Values,
    code: Vec<u8>,
}

#[repr(u8)]
pub enum OpCode {
    Return = 0,
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
        }
    }

    pub fn write(&mut self, byte: OpCode) {
        self.code.push(byte as u8)
    }

    pub fn write_bytes(&mut self, bytes: &[u8]) {
        self.code.extend_from_slice(bytes);
    }

    pub fn write_constant(&mut self, constant: f64) -> Result<(), ()> {
        let offset = self.values.add_constant(Value(constant))?;
        self.write(OpCode::Constant);
        self.write_bytes(offset.as_bytes());
        Ok(())
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

/// Formats a given chunk
struct ChunkFormatter<'chunk> {
    chunk: &'chunk Chunk,
}

impl<'chunk> ChunkFormatter<'chunk> {
    pub fn from_chunk(chunk: &'chunk Chunk) -> Self {
        ChunkFormatter { chunk }
    }

    pub fn format(&self, buffer: &mut String) -> anyhow::Result<()> {
        let mut offset = 0;

        while offset < self.chunk.code.len() {
            offset = match self.chunk.code[offset] {
                0 => Self::simple_instruction(buffer, "OP_RETURN", offset).with_context(|| {
                    format!(
                        "Unable to parse simple instruction in chunk: {}",
                        self.chunk.name
                    )
                })?,
                1 => self
                    .constant_instruction(buffer, "OP_CONSTANT", offset)
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
        buffer: &mut String,
        op_name: &'static str,
        offset: usize,
    ) -> Result<usize, FormatterError> {
        buffer.push_str(&format!("{:>4}", offset));
        buffer.push_str(&format!(" {}\n", op_name));

        Ok(offset + 1)
    }

    fn constant_instruction(
        &self,
        buffer: &mut String,
        op_name: &'static str,
        offset: usize,
    ) -> Result<usize, FormatterError> {
        buffer.push_str(&format!("{:>4}", offset));
        buffer.push_str(&format!(" {}", op_name));

        let idx_offset = offset + 1;
        let parsed_offset =
            Offset::read_from(&self.chunk.code[idx_offset..idx_offset + OFFSET_SIZE]);

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
}

/// Possible errors encountered during formatting a chunk
#[derive(Error, Debug)]
enum FormatterError {
    #[error("Unable to parse Value offset at {0}")]
    OffsetParse(usize),

    #[error("Unable to get value with idx {0} from value store")]
    Value(Offset),
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn format_op_return() {
        let mut chunk = Chunk::new("test");
        chunk.write(OpCode::Return);

        let formatter = ChunkFormatter::from_chunk(&chunk);

        let mut buffer = String::new();
        let result = formatter.format(&mut buffer);

        assert!(result.is_ok());
        assert_eq!(buffer, "   0 OP_RETURN\n");
    }

    #[test]
    fn format_op_constant() {
        let mut chunk = Chunk::new("test");
        chunk.write(OpCode::Return);
        chunk.write_constant(5.0).unwrap();

        let formatter = ChunkFormatter::from_chunk(&chunk);

        let mut buffer = String::new();
        let result = formatter.format(&mut buffer);

        assert!(result.is_ok());
        assert_eq!(buffer, "   0 OP_RETURN\n   1 OP_CONSTANT 5\n");
    }
}
