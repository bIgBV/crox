use std::fmt::Display;

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
        return Ok(());
    }

    fn constant_instruction(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        op_name: &'static str,
        offset: usize,
    ) -> usize {
        write!(f, "{:>4}", offset).expect("Formatting failed");
        write!(f, " {}", op_name).expect("Formatting failed");

        let idx_offset = offset + 1;
        if let Some(idx) = Offset::read_from(&self.code[idx_offset..idx_offset + OFFSET_SIZE]) {
            let val = self
                .values
                .get(idx)
                .expect("Unable to get value from const array");
            write!(f, " {}", val.0).expect("Formatting failed");
        } else {
            todo!("THE FUCK");
        }

        offset + 1 + OFFSET_SIZE
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "==== {} ====", self.name).expect("Formatting failed");

        let mut offset = 0;

        while offset < self.code.len() {
            offset = match self.code[offset] {
                0 => simple_instruction(f, "OP_RETURN", offset),
                1 => self.constant_instruction(f, "OP_CONSTANT", offset),
                _ => todo!(),
            };
        }

        fn simple_instruction(
            f: &mut std::fmt::Formatter<'_>,
            op_name: &'static str,
            offset: usize,
        ) -> usize {
            write!(f, "{:>4}", offset).expect("Formatting failed");
            writeln!(f, " {}", op_name).expect("Formatting failed");
            return offset + 1;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn display_op_return() {
        let mut chunk = Chunk::new("test");
        chunk.write(OpCode::Return);

        let output = format!("{:#}", chunk);

        assert_eq!(
            output,
            "==== test ====
   0 OP_RETURN
"
        );
    }
}
