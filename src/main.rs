use anyhow::Result;
use tracing::{info, Level};
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
    chunk.write(OpCode::Return, 1);
    info!(%chunk);

    let mut vm = Vm::new();
    vm.interpret(&mut chunk)?;

    Ok(())
}
