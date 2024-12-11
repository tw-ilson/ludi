use crate::pipeline::{BasicCompiler, Pipeline};

pub fn cli() {
    let args: Vec<String> = std::env::args().collect();
    assert!(args.len() == 2);
    let file = std::fs::read_to_string(args[1].clone()).expect("no such file");
    let output = BasicCompiler::apply(file).expect("failed");
    println!("{}", output);
}
