use anyhow::Result;
use rustyline::{
    completion::FilenameCompleter,
    highlight::{Highlighter, MatchingBracketHighlighter},
    hint::HistoryHinter,
    validate::MatchingBracketValidator,
    Completer, CompletionType, Config, EditMode, Editor, Helper, Hinter, Validator,
};

use crate::{chunk::Chunk, vm::Vm};
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
                rl.add_history_entry(line.as_str())?;
                vm.interpret(line)?;
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
