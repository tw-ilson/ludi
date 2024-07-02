use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};
use crate::interpret::Interpret;
use crate::run::run;
use libludi::env::{Env, EnvRef};

pub fn repl() -> Result<()> {
    let mut rl = DefaultEditor::new()?;
    let mut e: EnvRef = Env::default().into();
    #[cfg(feature = "with-file-history")]
    if rl.load_history("$LUDIPATH/history.txt").is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                run(&line, e.clone());
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    #[cfg(feature = "with-file-history")]
    rl.save_history("history.txt");
    Ok(())
}
