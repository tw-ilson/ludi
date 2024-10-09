#![allow(unused_imports)]
#![allow(dead_code)]
mod array;
mod datatypes;
mod interpret;
mod ops;
mod run;

// cli entrypoint
fn main() -> Result<()> {
    let _ = run::repl()?;
    Ok(())
}

// ****************************
//      Interpreter Tests
// ****************************

use std::result;

use array::{Array, Iota};
use datatypes::{ArrayType, AtomicType, DataType};
use libludi::err::Result;
use libludi::shape::{ArrayProps, Shape, ShapeOps};
use libludi::lex::lex;
use libludi::parser::expression;
use ops::*;

use crate::interpret::{DynamicEnv, Interpret};

#[test]
fn test_indexing() {
    let mut a: Array<u32> = Iota::iota(24);
    a.reshape(&[4, 3, 2]).unwrap();
    println!("{}", a);
    assert_eq!(Some(&8), a.get(&[2, 1, 0]))
}
#[test]
fn test_add() -> Result<()>{
    let mut a: Array<isize> = Iota::iota(9);
    let mut b: Array<isize> = Iota::iota(9);
    a.reshape(&[3, 3])?;
    b.reshape(&[3, 3])?;
    let _c = Add::add(
        DataType::Array(ArrayType::Int(a)),
        DataType::Array(ArrayType::Int(b)),
    )
    .unwrap();
    Ok(())
}


#[test]
fn test_frame() -> Result<()>{
    let mut a = ArrayType::Int(Iota::iota(100));
    a.reshape(&[10, 10])?;
    let mut b = ArrayType::Int(Iota::iota(100));
    b.reshape(&[10, 10])?;

    let c = Result::<ArrayType>::from_iter(vec![a, b]).unwrap();
    assert_eq!(&[2, 10, 10], c.shape_slice());
    println!("{}", c);
    Ok(())
}

#[test]
fn basic_unary() -> Result<()> {
    // assert_eq!(8, std::mem::size_of::<Atom>());
    let mut e: DynamicEnv = DynamicEnv::new(None);
    let expr = expression(&mut lex("let a = 2 in -a"))?;
    // dbg!(&expr);
    let r = expr.interpret(&mut e)?;
    assert_eq!(format!("{}",r), "-2");
    Ok(())
}

#[test]
fn basic_arithmetic1() -> Result<()> {
    let mut e: DynamicEnv = DynamicEnv::new(None);
    let r = expression(&mut lex("2+2"))?.interpret(&mut e)?;
    assert_eq!(format!("{}",r), "4");
    Ok(())
}

#[test]
fn basic_arithmetic2() -> Result<()> {
    let mut e: DynamicEnv = DynamicEnv::new(None);
    let r = expression(&mut lex("1./2."))?.interpret(&mut e)?;
    assert_eq!(format!("{}",r), "0.5");
    Ok(())
}
#[test]
fn basic_arithmetic3() -> Result<()> {
    let mut e: DynamicEnv = DynamicEnv::new(None);
    let r = expression(&mut lex("1.0/(2.0+3.0)"))?.interpret(&mut e)?;
    assert_eq!(format!("{}",r), "0.2");
    Ok(())
}
#[test]
fn basic_arithmetic4() ->Result<()> {
    let mut e: DynamicEnv = DynamicEnv::new(None);
    let r = expression(&mut lex("5.0 * 1.0/(2.0+3.0)"))?.interpret(&mut e)?;
    assert_eq!(format!("{}",r), "1");
    Ok(())
}
#[test]
fn basic_arithmetic5() -> Result<()> {
    let mut e: DynamicEnv = DynamicEnv::new(None);
    let r = expression(&mut lex("2.0 + 0.3"))?.interpret(&mut e)?;
    assert_eq!(format!("{}",r), "2.3");
    Ok(())
}
#[test]
fn basic_arithmetic6() -> Result<()> {
    let mut e: DynamicEnv = DynamicEnv::new(None);
    let r = expression(&mut lex("10.0 + 17.0 - 12.2/4.0"))?.interpret(&mut e)?;
    assert_eq!(format!("{}",r), "23.95");
    Ok(())
}
#[test]
fn assignment1() -> Result<()> {
    let mut e: DynamicEnv = DynamicEnv::new(None);
    let _ = expression(&mut lex("let a = 2.0+0.3;"))?.interpret(&mut e)?;
    let r = expression(&mut lex("a"))?.interpret(&mut e)?;
    assert_eq!(format!("{}",r), "2.3");
    let r = expression(&mut lex("let a = a + a; a"))?.interpret(&mut e)?;
    assert_eq!(format!("{}",r), "4.6");
    Ok(())
}
#[test]
fn assignment_err1() -> Result<()> {
    let mut e: DynamicEnv = DynamicEnv::new(None);
    let _ = expression(&mut lex("notathing"))?.interpret(&mut e).expect_err("failed to detect unbound symbol!");
    Ok(())
}

#[test] 
fn automap() -> Result<()> {
    let mut e: DynamicEnv = DynamicEnv::new(None);
    let r = expression(&mut lex("reshape(iota(8), [4 2]) * [0 2]"))?.interpret(&mut e)?;
    match r {
        DataType::Array(ArrayType::Int(int_array)) => {
            assert_eq!(int_array.shape_slice(), &[4, 2]);
            assert_eq!(int_array.data(), &[0, 2, 0, 6, 0, 10, 0, 14]);
            Ok(())
        },
        _ => Err(anyhow::anyhow!("this result should be an array of int"))
    }
}
