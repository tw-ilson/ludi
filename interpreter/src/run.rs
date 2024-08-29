use std::env;
use std::fs;
use std::io;

use crate::interpret::Interpret;
use libludi::ast::ParseTree;
use libludi::env::{Env, EnvRef};
use libludi::err::{LangError, Result};
use libludi::parser::Parser;
use libludi::lex::lex;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

#[repr(u32)]
pub enum LudiExit {
    Success = 0,
    UncaughtErr = 1,
}

pub fn repl() -> Result<()> {
    let mut rl = DefaultEditor::new().expect("readline failure?");
    let e: EnvRef = Env::default().into();
    #[cfg(feature = "with-file-history")]
    if rl.load_history("$LUDIPATH/history.txt").is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                run(&line, e.clone())?;
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    #[cfg(feature = "with-file-history")]
    rl.save_history("history.txt");
    Ok(())
}

pub fn run(source: &str, e: EnvRef) -> Result<LudiExit> {
    let dump_ast: bool = env::var("DUMP_AST").is_ok();
    let dump_tokens: bool = env::var("DUMP_TOKENS").is_ok();
    let mut tokens = lex(source);
    if dump_tokens {
        tokens.clone().for_each(|t| {
            println!("{:?}", t.token);
        })
    };
    let p: ParseTree = tokens.parse().unwrap();
    for stmt in p {
        if dump_ast {
            println!("{:#?}", &stmt);
        }
        if let Err(e) = stmt.interpret(e.clone()) {
            println!("{}", e);
        }
    }
    Ok(LudiExit::Success)
}

fn run_file(filename: String) -> Result<LudiExit> {
    let e = Env::new(None);
    let mut source = fs::read_to_string(filename).expect("Failed to read file");
    run(&mut source, e.into())?;
    Ok(LudiExit::Success)
}
