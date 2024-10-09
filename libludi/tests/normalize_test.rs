use libludi::lex::lex;
use libludi::parser::Parser;
use pretty_assertions::assert_eq;

// use libludi::normalize::*;

// #[test]
// fn normalize_fncall_basic() -> anyhow::Result<()> {
//     let ast: NormalizedAST = lex("f(x+1)").parse()?.normalize(&mut 0)?;
//     let expected: NormalizedAST = lex("let v0 = x+1 in f(v0)").parse()?.normalize(&mut 0)?;
//     // println!("{:#?}", ast);
//     // println!("{:?}", expected);
//     assert_eq!(ast[0], expected[0]);
//     Ok(())
// }
//
//
// #[test]
// fn normalize_add_two_fns() -> anyhow::Result<()> {
//     let ast: NormalizedAST = lex("f(a+2) - g(b+3)").parse()?.normalize(&mut 0)?;
//     let expected: NormalizedAST = lex("let v0 = a+2 in let v1 = b+3 in f(v0) - g(v1)").parse()?.normalize(&mut 0)?;
//     // println!("{:#?}", ast);
//     // println!("{:?}", expected);
//     assert_eq!(ast[0], expected[0]);
//     Ok(())
// }
//
// #[test]
// fn normalize_fncall_nested() -> anyhow::Result<()> {
//     let ast: NormalizedAST = lex("f(g(h(x)))").parse()?.normalize(&mut 0)?;
//     let expected: NormalizedAST = lex("let v1 = h(x) in let v0 = g(v1) in f(v0)")
//         .parse()?
//         .normalize(&mut 0)?;
//     assert_eq!(ast[0], expected[0]);
//     Ok(())
// }
