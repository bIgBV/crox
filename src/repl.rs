use anyhow::Result;
use miette::{Diagnostic, EyreContext, MietteHandlerOpts};
use rustyline::{
    completion::FilenameCompleter,
    highlight::{Highlighter, MatchingBracketHighlighter},
    hint::HistoryHinter,
    validate::MatchingBracketValidator,
    Completer, CompletionType, Config, EditMode, Editor, Helper, Hinter, Validator,
};
use thiserror::Error;

use crate::vm::Vm;
use std::borrow::Cow::{self, Borrowed, Owned};

pub struct Repl {}

impl Repl {
    pub fn start() -> Result<()> {
        let config = Config::builder()
            .history_ignore_space(true)
            .completion_type(CompletionType::List)
            .edit_mode(EditMode::Vi)
            .build();
        let h = MyHelper {
            completer: FilenameCompleter::new(),
            highlighter: MatchingBracketHighlighter::new(),
            hinter: HistoryHinter {},
            colored_prompt: "".to_owned(),
            validator: MatchingBracketValidator::new(),
        };
        let mut rl = Editor::with_config(config)?;
        rl.set_helper(Some(h));

        let vm = Vm::new();

        loop {
            let p = format!(">> ");
            rl.helper_mut().expect("No helper").colored_prompt = format!("\x1b[1;32m{p}\x1b[0m");
            let readline = rl.readline(&p);
            if let Ok(line) = readline {
                if line.len() == 0 {
                    continue;
                }
                rl.add_history_entry(line.as_str())?;
                match vm.interpret(line) {
                    Ok(_) => (),
                    Err(e) => println!("{}", e),
                }
            } else {
                println!("Interrupted");
                break;
            }
        }
        rl.append_history("history.txt")?;

        Ok(())
    }
}

#[derive(Helper, Completer, Hinter, Validator)]
struct MyHelper {
    #[rustyline(Completer)]
    completer: FilenameCompleter,
    highlighter: MatchingBracketHighlighter,
    #[rustyline(Validator)]
    validator: MatchingBracketValidator,
    #[rustyline(Hinter)]
    hinter: HistoryHinter,
    colored_prompt: String,
}

impl Highlighter for MyHelper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        if default {
            Borrowed(&self.colored_prompt)
        } else {
            Borrowed(prompt)
        }
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        self.highlighter.highlight_char(line, pos)
    }
}

pub fn report_error(source: &str, error: &(dyn miette::Diagnostic + Send + Sync + 'static)) {
    eprintln!("{:?}", ErrorReporter(error, source));
}

#[derive(Error)]
#[error("{0}")]
pub struct ErrorReporter<'source>(
    pub &'source (dyn miette::Diagnostic + Send + Sync + 'static),
    pub &'source str,
);

impl<'error> std::fmt::Debug for ErrorReporter<'error> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let miette_handler = MietteHandlerOpts::new().color(true).unicode(true).build();

        // Ignore error to prevent format! panics. This can happen if the source code is pointing to the wrong location
        let _ = miette_handler.debug(self, f);

        Ok(())
    }
}

impl<'error> Diagnostic for ErrorReporter<'error> {
    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.0.code()
    }

    fn severity(&self) -> Option<miette::Severity> {
        self.0.severity()
    }

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.0.help()
    }

    fn url<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.0.url()
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        // This is where we provide the source code if it isn't already available
        if let Some(source_code) = self.0.source_code() {
            Some(source_code)
        } else {
            Some(&self.1)
        }
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        None
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        None
    }

    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        None
    }
}
