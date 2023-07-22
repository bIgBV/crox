use anyhow::Result;
use tracing::Level;

use tracing_subscriber::FmtSubscriber;

use crate::{
    chunk::{Chunk, OpCode},
    vm::Vm,
};

mod chunk;
mod compiler;
mod line_store;
mod memory;
mod repl;
mod scanner;
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

    Ok(())
}
