use anyhow::Result;
use tracing::Level;

use tracing_subscriber::FmtSubscriber;

use crate::{
    chunk::{Chunk, OpCode},
    vm::Vm,
};

mod chunk;
mod line_store;
mod memory;
mod value;
mod vm;

fn main() -> Result<()> {
    let subscriber = FmtSubscriber::builder()
        // all spans/events with a level higher than TRACE (e.g, debug, info, warn, etc.)
        // will be written to stdout.
        .with_max_level(Level::TRACE)
        // completes the builder.
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    let mut chunk = Chunk::new("test");
    chunk.write_constant(4.5, 1)?;

    chunk.write_constant(6.9, 1)?;
    chunk.write(OpCode::Add, 2);

    chunk.write_constant(8.6, 2)?;

    chunk.write(OpCode::Divide, 2);
    chunk.write(OpCode::Negate, 2);
    chunk.write(OpCode::Return, 3);
    let vm = Vm::new();
    vm.interpret(&chunk)?;

    Ok(())
}
