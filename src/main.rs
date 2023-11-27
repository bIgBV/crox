use anyhow::Result;
use argh::FromArgs;
use repl::Repl;
use tracing::Level;

use tracing_subscriber::FmtSubscriber;
use vm::Vm;

mod chunk;
mod compiler;
mod line_store;
mod memory;
mod repl;
mod scanner;
mod value;
mod vm;

#[derive(FromArgs)]
/// Run a clox interpreter
struct Options {
    /// enable verbose logging
    #[argh(switch, short = 'v')]
    verbose: bool,

    /// compile and execute a string
    #[argh(option, short = 's')]
    stream: Option<String>,
}

fn main() -> Result<()> {
    let options: Options = argh::from_env();

    if options.verbose {
        let subscriber = FmtSubscriber::builder()
            .pretty()
            // all spans/events with a level higher than TRACE (e.g, debug, info, warn, etc.)
            // will be written to stdout.
            .with_max_level(Level::TRACE)
            // completes the builder.
            .finish();

        tracing::subscriber::set_global_default(subscriber)
            .expect("setting default subscriber failed");
    }

    if let Some(stream) = options.stream {
        let vm = Vm::new();
        vm.interpret(stream)?;
    } else {
        let _ = Repl::start()?;
    }

    Ok(())
}
