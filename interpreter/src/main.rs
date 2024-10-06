#![allow(unused_imports)]
#![allow(dead_code)]
mod array;
mod datatypes;
mod ops;
mod interpret;
mod run;

fn main() -> () {
    let _ = run::repl();
}
