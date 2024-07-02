use libludi::env::{Env, EnvRef};
use libludi::err::Result;
use libludi::atomic::*;
use interpreter::parser::Parser;
use interpreter::interpret::*;

#[test]
fn basic_unary() -> Result<()> {
    // assert_eq!(8, std::mem::size_of::<Atom>());
    let e: EnvRef = Env::new(None).into();
    let r = "-2".to_string().parse()?[0].interpret(e)?;
    assert_eq!(format!("{}",r), "-2");
    Ok(())
}

#[test]
fn basic_arithmetic1() -> Result<()> {
    assert_eq!(8, std::mem::size_of::<Atom>());
    let e: EnvRef = Env::new(None).into();
    let r = "2+2".to_string().parse()?[0].interpret(e)?;
    assert_eq!(format!("{}",r), "4");
    Ok(())
}

#[test]
fn basic_arithmetic2() -> Result<()> {
    let e: EnvRef = Env::new(None).into();
    let r = "1/2".to_string().parse()?[0].interpret(e)?;
    assert_eq!(format!("{}",r), "0.5");
    Ok(())
}
#[test]
fn basic_arithmetic3() -> Result<()> {
    let e: EnvRef = Env::new(None).into();
    let r = "1/(2+3)".to_string().parse()?[0].interpret(e)?;
    assert_eq!(format!("{}",r), "0.2");
    Ok(())
}
#[test]
fn basic_arithmetic4() ->Result<()> {
    let e: EnvRef = Env::new(None).into();
    let r = "5 * 1/(2+3)".to_string().parse()?[0].interpret(e)?;
    assert_eq!(format!("{}",r), "1");
    Ok(())
}
#[test]
fn basic_arithmetic5() -> Result<()> {
    let e: EnvRef = Env::new(None).into();
    let r = "2.0 + 0.3".to_string().parse()?[0].interpret(e)?;
    assert_eq!(format!("{}",r), "2.3");
    Ok(())
}
#[test]
fn basic_arithmetic6() -> Result<()> {
    let e: EnvRef = Env::new(None).into();
    let r = "10 + 17 - 12.2/4".to_string().parse()?[0].interpret(e)?;
    assert_eq!(format!("{}",r), "23.95");
    Ok(())
}
#[test]
fn assignment1() -> Result<()> {
    let e: EnvRef = Env::new(None).into();
    let _ = "a = 2.0+0.3".to_string().parse()?[0].interpret(e.clone())?;
    let r = "a".to_string().parse()?[0].interpret(e.clone())?;
    assert_eq!(format!("{}",r), "2.3");
    let r = "a = a + a".to_string().parse()?[0].interpret(e)?;
    assert_eq!(format!("{}",r), "4.6");
    Ok(())
}

// #[test]
// fn array1() -> Result<()> {
//     let e: EnvRef = Env::new(None).into();
//     let _ = "a = (1 1 1)"
//         .to_string().parse()?[0].interpret(e.clone())?;
//     let r = "a".to_string().parse()?[0].interpret(e.clone())?;
//     assert_eq!(format!("{}",r), "2.3");
//     let r = "a = a + a".to_string().parse()?[0].interpret(e)?;
//     assert_eq!(format!("{}",r), "4.6");
//     Ok(())
// }
