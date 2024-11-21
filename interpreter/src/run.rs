use std::env;
use std::fs;
use std::io;

use crate::interpret::DynamicEnv;
use crate::interpret::Interpret;
use libludi::ast::Expr;
use libludi::ast::ParseTree;
use libludi::ast::Stmt;
use libludi::env::{Env, EnvRef};
use libludi::err::{Error, Result};
use libludi::lex::lex;
use libludi::parser::Parser;
use libludi::parser::{expression, statement};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

#[repr(u32)]
pub enum LudiExit {
    Success = 0,
    UncaughtErr = 1,
}

pub fn repl() -> Result<()> {
    let mut rl = DefaultEditor::new().expect("readline failure?");
    let mut e: DynamicEnv = Env::default().into();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                run(&line, &mut e)?;
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
    Ok(())
}

pub fn run(source: &str, e: &mut DynamicEnv) -> Result<LudiExit> {
    let dump_ast: bool = env::var("DUMP_AST").is_ok();
    let dump_tokens: bool = env::var("DUMP_TOKENS").is_ok();
    let mut tokens = lex(source);
    if dump_tokens {
        tokens.clone().for_each(|t| {
            println!("{:?}", t.token);
        })
    };
    match statement(&mut tokens) {
        Ok(p) => {
            if dump_ast {
                println!("{:#?}", &p);
            }
            if let Err(e) = p.interpret(e) {
                println!("{}", e);
            }
        }
        Err(e) => println!("{}", e),
    }
    Ok(LudiExit::Success)
}

fn run_file(filename: String) -> Result<LudiExit> {
    let mut e = DynamicEnv::new(None);
    let mut source = fs::read_to_string(filename).expect("Failed to read file");
    run(&mut source, &mut e)?;
    Ok(LudiExit::Success)
}
