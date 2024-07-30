use libludi::ast::*;
use libludi::atomic::AtomicType;
use libludi::env::Name;
use libludi::parser::*;
use libludi::lex::lex;
use libludi::tokens::*;
use std::vec;

#[test]
fn scan_arithmetic_tokens() {
    let src = "+ - * /";
    let src2 = "+-*/";
    let s = lex(src);
    use libludi::tokens::Token::*;
    assert_eq!(
        s.map(|tok| tok.token).collect::<Vec<Token>>(),
        vec![PLUS, MINUS, STAR, SLASH]
    );
    let s = lex(src2);
    assert_eq!(
        s.map(|tok| tok.token).collect::<Vec<Token>>(),
        vec![PLUS, MINUS, STAR, SLASH]
    );
}

#[test]
fn scan_number_literal() {
    let src = "12 - 7".to_string();
    let src2 = "4712.08".to_string();
    let src3 = "2.0 + 0.3".to_string();
    use libludi::tokens::Token::*;
    assert_eq!(
        lex(&src).next().unwrap(),
        TokenData {
            token: INTEGER_LITERAL("12".into()),
            line: 1
        }
    );
    assert_eq!(
        lex(&src2).next().unwrap(),
        TokenData {
            token: FLOAT_LITERAL("4712.08".into()),
            line: 1
        }
    );
    assert_eq!(
        lex(&src3).map(|tok| tok.token).collect::<Vec<Token>>(),
        vec![
            FLOAT_LITERAL("2.0".into()),
            PLUS,
            FLOAT_LITERAL("0.3".into())
        ]
    );
}

#[test]
fn scan_string_literal() {
    let src = "\"Hello,\" + \" world!\"";
    use libludi::tokens::Token::*;
    let toks: Vec<Token> = lex(src).map(|d| d.token).collect::<Vec<Token>>();
    assert_eq!(
        toks.as_slice(),
        &[
            STRING_LITERAL("Hello,".to_string()),
            PLUS,
            STRING_LITERAL(" world!".to_string())
        ]
    );
}

#[test]
fn scan_ident() {
    let src = "a = 10".to_string();
    use libludi::tokens::Token::*;
    assert_eq!(
        lex(&src).next().unwrap(),
        TokenData {
            token: IDENTIFIER("a".to_string()),
            line: 1
        }
    );
    assert_eq!(
        lex("a = 10").parse().expect("failed to parse")[0],
        Stmt::AssignStmt(
            AssignStmtNode {
                name: libludi::env::Name {
                    name: "a".into(),
                    line: 1
                },
                initializer: Expr::Literal(
                    LiteralNode {
                        value: AtomicType::Int64(10)
                    }
                    .into()
                )
            }
            .into()
        )
    );
}

#[test]
fn scan_stmts() {
    let src = "print 2+2;";
    use libludi::tokens::Token::*;
    let toks: Vec<Token> = lex(src).map(|d| d.token).collect::<Vec<Token>>();
    assert_eq!(
        toks.as_slice(),
        &[
            PRINT,
            INTEGER_LITERAL("2".into()),
            PLUS,
            INTEGER_LITERAL("2".into()),
            SEMICOLON
        ]
    );
}

// #[test]
// fn binary_expr1() {
//     use Expr::*;
//     use Token::*;
//     let mut src = "5<3".;
//     let s: &Stmt = &src.parse().expect("failed to parse")[0];
//
//     let s_test = &Stmt::ExprStmt(
//         ExprStmtNode {
//             expression: Binary(
//                 BinaryNode {
//                     left: AtomicCast(
//                         AtomicCastNode {
//                             value: LiteralNode {
//                                 value: AtomicData::from(TokenData {
//                                     token: INTEGER_LITERAL("5".into()),
//                                     line: 1,
//                                 }),
//                             },
//                         }
//                         .into(),
//                     ),
//                     operator: TokenData {
//                         token: LESS,
//                         line: 1,
//                     },
//                     right: AtomicCast(
//                         AtomicCastNode {
//                             value: LiteralNode {
//                                 value: AtomicData::from(TokenData {
//                                     token: INTEGER_LITERAL("3".into()),
//                                     line: 1,
//                                 }),
//                             },
//                         }
//                         .into(),
//                     ),
//                 }
//                 .into(),
//             ),
//         }
//         .into(),
//     );
//
//     assert_eq!(s, s_test)
// }

#[test]
fn binary_expr2() -> anyhow::Result<()> {
    use Expr::*;
    use Token::*;
    let s = lex("75.4 + 1.006").parse()?;

    let s_test = Stmt::ExprStmt(
        ExprStmtNode {
            expression: BinaryOperation(
                BinaryNode {
                    left: Literal(
                        LiteralNode {
                            value: AtomicType::from(TokenData {
                                token: FLOAT_LITERAL("75.4".into()),
                                line: 1,
                            }),
                        }
                        .into(),
                    ),
                    operator: TokenData {
                        token: PLUS,
                        line: 1,
                    },
                    right: Literal(
                        LiteralNode {
                            value: AtomicType::from(TokenData {
                                token: FLOAT_LITERAL("1.006".into()),
                                line: 1,
                            }),
                        }
                        .into(),
                    ),
                }
                .into(),
            ),
        }
        .into(),
    );
    assert_eq!(s[0], s_test);
    Ok(())
}

#[test]
fn test_fndef() -> anyhow::Result<()> {
    use std::str::FromStr;
    let prg = expression(&mut lex("fn (a[3] b[3] -> [3]) : a + b"))?;
    dbg!(&prg);
    assert_eq!(
        prg,
        Expr::FnDef(
            FnDefNode {
                args: vec![
                    (Name::from_str("a")?, smallvec::smallvec![3]),
                    (Name::from_str("b")?, smallvec::smallvec![3]),
                ],
                ret: Some(smallvec::smallvec![3]),
                body: Expr::BinaryOperation(
                    BinaryNode {
                        operator: TokenData {
                            token: Token::PLUS,
                            line: 1
                        },
                        left: Expr::Assignment(
                            AssignmentNode {
                                name: Name::from_str("a")?
                            }
                            .into()
                        ),
                        right: Expr::Assignment(
                            AssignmentNode {
                                name: Name::from_str("b")?
                            }
                            .into()
                        )
                    }
                    .into()
                )
            }
            .into()
        )
    );
    Ok(())
}
