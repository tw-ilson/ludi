use libludi::{lex::Lex, parser::expression, shape::Shape, types::*};
use pretty_assertions::assert_eq;
use typecheck::TypeCheck;

#[test]
fn basic_types() -> anyhow::Result<()> {
    let prg1 = "1";
    let prg2 = "true";
    let prg3 = "1.0";
    let mut table = TypeEnv::new();
    let t_expr1 = expression(&mut prg1.lex())?
        .type_check(&mut table)?;
    let t_expr2 = expression(&mut prg2.lex())?
        .type_check(&mut table)?;
    let t_expr3 = expression(&mut prg3.lex())?
        .type_check(&mut table)?;
    assert_eq!(&Type::Atom(Atom::Literal(AtomicDataType::Int64)), t_expr1.get_type());
    assert_eq!(&Type::Atom(Atom::Literal(AtomicDataType::Boolean)), t_expr2.get_type());
    assert_eq!(&Type::Atom(Atom::Literal(AtomicDataType::Float64)), t_expr3.get_type());
    Ok(())
}

#[test]
fn arithmetic_basic() -> anyhow::Result<()> {
    let prg1 = "1 + 2";
    let prg2 = "[1 1] + [[2 2] [3 3]]";
    let mut table = TypeEnv::new();
    let t_expr1 = expression(&mut prg1.lex())?.type_check(&mut table)?;
    let t_expr2 = expression(&mut prg2.lex())?.type_check(&mut table)?;
    assert_eq!(&Type::Atom(Atom::Literal(AtomicDataType::Int64)), t_expr1.get_type());
    assert_eq!(&Type::Array(Array::Arr(Arr{ element: Atom::Literal(AtomicDataType::Int64), shape: Shape::new(&[2,2])})), t_expr2.get_type());
    Ok(())
}

#[test]
fn letexpr_basic() -> anyhow::Result<()> {
    let prg = "let a = true in a";
    let mut table = TypeEnv::new();
    // let expr = expression(&mut prg))?;
    let t_expr = expression(&mut prg.lex())?
        .type_check(&mut table)?;
    assert_eq!(&Type::Atom(Atom::Literal(AtomicDataType::Boolean)), t_expr.get_type());
    // println!("{:#?}", expr);
    Ok(())
}

#[test]
fn frame_basic() -> anyhow::Result<()> {
    let expr1 = expression(&mut "[[1 2] [3 4]]".lex())?;
    let expr2 = expression(&mut "
        let a = true in 
            let b = false in
            [[a b]
             [b a]]
    ".lex())?;
    let ty1 = expr1.type_check(&mut TypeEnv::new())?;
    let ty2 = expr2.type_check(&mut TypeEnv::new())?;
    assert_eq!(
        ty1.get_type(),
        &Type::Array(Array::Arr(Arr {
            element: Atom::Literal(AtomicDataType::Int64),
            shape: Shape::new(&[2, 2]),
        }))
    );
    assert_eq!(
        ty2.get_type(),
        &Type::Array(Array::Arr(Arr {
            element: Atom::Literal(AtomicDataType::Boolean), // should this be AtomRef?
            shape: Shape::new(&[2,2]) ,
        }))
    );
    Ok(())
}

#[test]
fn fn_def_basic() -> anyhow::Result<()> {
    let expr = expression(&mut "
        fn diff_square(x[u32], y[u32]) -> [u32] {
            x*x - y*y
        }
    ".lex())?;
    let ty = expr.type_check(&mut TypeEnv::new())?.get_type();
    // assert_eq!(
    //     ty,
    //     Type::Atom(Atom::Func()))
    Ok(())
}
