use libludi::{lex::lex, parser::expression, shape::Shape, types::*};
use pretty_assertions::assert_eq;
use typecheck::TypeCheck;

#[test]
fn basic_types() -> anyhow::Result<()> {
    let prg1 = "1";
    let prg2 = "true";
    let prg3 = "1.0";
    let mut table = TypeEnv::new();
    let t_expr1 = expression(&mut lex(prg1))?
        .type_check(&mut table)?
        .get_type();
    let t_expr2 = expression(&mut lex(prg2))?
        .type_check(&mut table)?
        .get_type();
    let t_expr3 = expression(&mut lex(prg3))?
        .type_check(&mut table)?
        .get_type();
    assert_eq!(Type::Atom(Atom::Literal(AtomicDataType::Int64)), t_expr1);
    assert_eq!(Type::Atom(Atom::Literal(AtomicDataType::Boolean)), t_expr2);
    assert_eq!(Type::Atom(Atom::Literal(AtomicDataType::Float64)), t_expr3);
    Ok(())
}

#[test]
fn letexpr_types() -> anyhow::Result<()> {
    let prg = "let a = true in a";
    let mut table = TypeEnv::new();
    // let expr = expression(&mut lex(prg))?;
    let t_expr = expression(&mut lex(prg))?
        .type_check(&mut table)?
        .get_type();
    assert_eq!(Type::Atom(Atom::Literal(AtomicDataType::Boolean)), t_expr);
    // println!("{:#?}", expr);
    Ok(())
}

#[test]
fn frame_type() -> anyhow::Result<()> {
    let expr1 = expression(&mut lex("[[1 2] [3 4]]"))?;
    let expr2 = expression(&mut lex("
        let a = true in 
            let b = false in
            [[a b]
             [b a]]
    "))?;
    let ty1 = expr1.type_check(&mut TypeEnv::new())?.get_type();
    let ty2 = expr2.type_check(&mut TypeEnv::new())?.get_type();
    assert_eq!(
        ty1,
        Type::Array(Array::Arr(Arr {
            element: Atom::Literal(AtomicDataType::Int64),
            shape: Shape::new(&[2, 2]),
        }))
    );
    assert_eq!(
        ty2,
        Type::Array(Array::Arr(Arr {
            element: Atom::Literal(AtomicDataType::Boolean), // should this be AtomRef?
            shape: Shape::new(&[2,2]) ,
        }))
    );
    Ok(())
}

#[test]
fn fn_def_type() -> anyhow::Result<()> {
    let expr = expression(&mut lex("
        fn diff_square(x[u32], y[u32]) -> [u32] {
            x*x - y*y
        }
    "))?;
    let ty = expr.type_check(&mut TypeEnv::new())?.get_type();
    // assert_eq!(
    //     ty,
    //     Type::Atom(Atom::Func()))
    Ok(())
}
