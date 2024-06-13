use std::env;
use std::fs;
use std::io;

use crate::interpret::Interpret;
// use interpreter::interpret::*;
use crate::ast::Program;
use crate::env::{Env, EnvRef};
use crate::parser::Parser;
// use interpreter::primitive::*;
use crate::scanner::scanner;
// use std::iter::Peekable;

pub fn run(source: &mut String, e: EnvRef) {
    let dump_ast: bool = env::var("DUMP_AST").is_ok();
    let dump_tokens: bool = env::var("DUMP_TOK").is_ok();
    let mut tokens = scanner(source);
    if dump_tokens {
        tokens.clone().for_each(|t| {
            println!("{:?}", t.token);
        })
    };
    let p: Program = tokens.parse().unwrap();
    for stmt in p {
        if dump_ast {
            println!("{:#?}", &stmt);
        }
        let _ = stmt.interpret(e.clone());
    }
    // tokens.compile().unwrap();
}

fn run_prompt() {
    let e: EnvRef = Env::new(None).into();
    io::stdin()
        .lines()
        .for_each(|line| run(&mut line.unwrap(), e.clone()))
}

fn run_file(filename: String) {
    let e = Env::new(None);
    let mut source = fs::read_to_string(filename).expect("Failed to read file");
    run(&mut source, e.into());
}

pub fn entry() {
    let mut args = env::args().skip(1);
    if let Some(filename) = args.next() {
        run_file(filename);
    } else {
        run_prompt();
    }
}
