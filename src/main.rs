use anyhow::Result;
use repl::Repl;
use tracing::Level;

use tracing_subscriber::FmtSubscriber;

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

    let _ = Repl::start()?;

    Ok(())
}
