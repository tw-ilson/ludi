use libludi::ast::*;
use libludi::atomic::AtomicType;
use libludi::data::DataType;
use libludi::env::Name;
use libludi::lex::lex;
use libludi::parser::*;
use libludi::tokens::*;
use pretty_assertions::{assert_eq, assert_ne};
use std::str::FromStr;
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
fn parse_let() -> anyhow::Result<()> {
    let expr = lex("let a = 10").parse()?;
    assert_eq!(
        expr[0],
        let_node(
            Name::from_str("a")?,
            literal_node(DataType::Atomic(AtomicType::Int64(10))),
            None
        )
    );
    Ok(())
}

#[test]
fn parse_let_with_body() -> anyhow::Result<()> {
    let expr = lex("let a = 2 in a+2").parse()?;
    assert_eq!(
        expr[0],
        Expr::Let(
            LetNode {
                name: libludi::env::Name {
                    name: "a".into(),
                    line: 1
                },
                initializer: Expr::Literal(
                    LiteralNode {
                        value: DataType::Atomic(AtomicType::Int64(2))
                    }
                    .into()
                ),
                region: Some(Expr::BinaryOperation(
                    BinaryOperationNode {
                        left: Expr::Term(
                            TermNode {
                                name: libludi::env::Name {
                                    name: "a".into(),
                                    line: 1
                                }
                            }
                            .into()
                        ),
                        right: Expr::Literal(
                            LiteralNode {
                                value: DataType::Atomic(AtomicType::Int64(2))
                            }
                            .into()
                        ),
                        operator: BinaryOpType::ADD,
                    }
                    .into()
                ))
            }
            .into()
        )
    );
    Ok(())
}
#[test]
fn let_with_body_complex() -> anyhow::Result<()> {
    let expr = lex("let a = 2 in let b = 4 in let c = a + b in foo(a, b, c)").parse()?;
    assert_eq!(
        expr[0],
        Expr::Let(
            LetNode {
                name: libludi::env::Name {
                    name: "a".into(),
                    line: 1
                },
                initializer: Expr::Literal(
                    LiteralNode {
                        value: DataType::Atomic(AtomicType::Int64(2))
                    }
                    .into()
                ),
                region: Some(Expr::Let(
                    LetNode {
                        name: libludi::env::Name {
                            name: "b".into(),
                            line: 1
                        },
                        initializer: Expr::Literal(
                            LiteralNode {
                                value: DataType::Atomic(AtomicType::Int64(4))
                            }
                            .into()
                        ),
                        region: Some(Expr::Let(
                            LetNode {
                                name: libludi::env::Name {
                                    name: "c".into(),
                                    line: 1
                                },
                                initializer: Expr::BinaryOperation(
                                    BinaryOperationNode {
                                        left: Expr::Term(
                                            TermNode {
                                                name: libludi::env::Name {
                                                    name: "a".into(),
                                                    line: 1
                                                }
                                            }
                                            .into()
                                        ),
                                        right: Expr::Term(
                                            TermNode {
                                                name: libludi::env::Name {
                                                    name: "b".into(),
                                                    line: 1
                                                }
                                            }
                                            .into()
                                        ),
                                        operator: BinaryOpType::ADD,
                                    }
                                    .into()
                                ),
                                region: Some(Expr::FnCall(
                                    FnCallNode {
                                        callee: Expr::Term(
                                            TermNode {
                                                name: libludi::env::Name {
                                                    name: "foo".into(),
                                                    line: 1
                                                }
                                            }
                                            .into()
                                        ),
                                        args: vec![
                                            Expr::Term(
                                                TermNode {
                                                    name: libludi::env::Name {
                                                        name: "a".into(),
                                                        line: 1
                                                    }
                                                }
                                                .into()
                                            ),
                                            Expr::Term(
                                                TermNode {
                                                    name: libludi::env::Name {
                                                        name: "b".into(),
                                                        line: 1
                                                    }
                                                }
                                                .into()
                                            ),
                                            Expr::Term(
                                                TermNode {
                                                    name: libludi::env::Name {
                                                        name: "c".into(),
                                                        line: 1
                                                    }
                                                }
                                                .into()
                                            )
                                        ]
                                    }
                                    .into()
                                ))
                            }
                            .into()
                        )),
                    }
                    .into()
                ))
            }
            .into()
        )
    );
    Ok(())
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
//                 BinaryOperationNode {
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

    let s_test = BinaryOperation(
        BinaryOperationNode {
            left: Literal(
                LiteralNode {
                    value: DataType::Atomic(AtomicType::from(TokenData {
                        token: FLOAT_LITERAL("75.4".into()),
                        line: 1,
                    })),
                }
                .into(),
            ),
            operator: BinaryOpType::ADD,
            right: Literal(
                LiteralNode {
                    value: DataType::Atomic(AtomicType::from(TokenData {
                        token: FLOAT_LITERAL("1.006".into()),
                        line: 1,
                    })),
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
fn test_binary_operation() -> anyhow::Result<()> {
    use std::str::FromStr;
    let prg = expression(&mut lex("a + b * c - d"))?;
    assert_eq!(
        prg,
        Expr::BinaryOperation(
            BinaryOperationNode {
                operator: BinaryOpType::SUB,
                left: Expr::BinaryOperation(
                    BinaryOperationNode {
                        operator: BinaryOpType::ADD,
                        left: Expr::Term(
                            TermNode {
                                name: Name::from_str("a")?
                            }
                            .into()
                        ),
                        right: Expr::BinaryOperation(
                            BinaryOperationNode {
                                operator: BinaryOpType::MUL,
                                left: Expr::Term(
                                    TermNode {
                                        name: Name::from_str("b")?
                                    }
                                    .into()
                                ),
                                right: Expr::Term(
                                    TermNode {
                                        name: Name::from_str("c")?
                                    }
                                    .into()
                                )
                            }
                            .into()
                        )
                    }
                    .into()
                ),
                right: Expr::Term(
                    TermNode {
                        name: Name::from_str("d")?
                    }
                    .into()
                )
            }
            .into()
        )
    );
    Ok(())
}

#[test]
fn test_fndef() -> anyhow::Result<()> {
    use std::str::FromStr;
    let prg = expression(&mut lex("fn (a[3] b[3] -> [3]) : a + b"))?;
    assert_eq!(
        prg,
        Expr::FnDef(
            FnDefNode {
                signature: CallSignature {
                    args: vec![
                        (Name::from_str("a")?, smallvec::smallvec![3]),
                        (Name::from_str("b")?, smallvec::smallvec![3]),
                    ],
                    ret: Some(smallvec::smallvec![3]),
                },
                body: Expr::BinaryOperation(
                    BinaryOperationNode {
                        operator: BinaryOpType::ADD,
                        left: Expr::Term(
                            TermNode {
                                name: Name::from_str("a")?
                            }
                            .into()
                        ),
                        right: Expr::Term(
                            TermNode {
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
#[test]
fn test_fndef_complex_body() -> anyhow::Result<()> {
    use std::str::FromStr;
    let prg = expression(&mut lex("fn (x[5] y[5] -> [5]) : (x + y) * 2"))?;
    assert_eq!(
        prg,
        Expr::FnDef(
            FnDefNode {
                signature: CallSignature {
                    args: vec![
                        (Name::from_str("x")?, smallvec::smallvec![5]),
                        (Name::from_str("y")?, smallvec::smallvec![5]),
                    ],
                    ret: Some(smallvec::smallvec![5]),
                },
                body: Expr::BinaryOperation(
                    BinaryOperationNode {
                        operator: BinaryOpType::MUL,
                        left: Expr::Grouping(
                            GroupingNode {
                                expression: Expr::BinaryOperation(
                                    BinaryOperationNode {
                                        operator: BinaryOpType::ADD,
                                        left: Expr::Term(
                                            TermNode {
                                                name: Name::from_str("x")?
                                            }
                                            .into()
                                        ),
                                        right: Expr::Term(
                                            TermNode {
                                                name: Name::from_str("y")?
                                            }
                                            .into()
                                        )
                                    }
                                    .into()
                                )
                            }
                            .into()
                        ),
                        right: Expr::Literal(
                            LiteralNode {
                                value: DataType::Atomic(AtomicType::Int64(2))
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

#[test]
fn test_fncall() -> anyhow::Result<()> {
    use std::str::FromStr;
    let prg = expression(&mut lex("foo(1, 2)"))?;
    assert_eq!(
        prg,
        Expr::FnCall(
            FnCallNode {
                callee: Expr::Term(
                    TermNode {
                        name: Name::from_str("foo")?
                    }
                    .into()
                ),
                args: vec![
                    Expr::Literal(
                        LiteralNode {
                            value: DataType::Atomic(AtomicType::Int64(1))
                        }
                        .into()
                    ),
                    Expr::Literal(
                        LiteralNode {
                            value: DataType::Atomic(AtomicType::Int64(2))
                        }
                        .into()
                    ),
                ]
            }
            .into()
        )
    );
    Ok(())
}
#[test]
fn test_nested_fncall() -> anyhow::Result<()> {
    use std::str::FromStr;
    let prg = expression(&mut lex("foo(bar(1), baz(2))"))?;
    assert_eq!(
        prg,
        Expr::FnCall(
            FnCallNode {
                callee: Expr::Term(
                    TermNode {
                        name: Name::from_str("foo")?
                    }
                    .into()
                ),
                args: vec![
                    Expr::FnCall(
                        FnCallNode {
                            callee: Expr::Term(
                                TermNode {
                                    name: Name::from_str("bar")?
                                }
                                .into()
                            ),
                            args: vec![Expr::Literal(
                                LiteralNode {
                                    value: DataType::Atomic(AtomicType::Int64(1))
                                }
                                .into()
                            )]
                        }
                        .into()
                    ),
                    Expr::FnCall(
                        FnCallNode {
                            callee: Expr::Term(
                                TermNode {
                                    name: Name::from_str("baz")?
                                }
                                .into()
                            ),
                            args: vec![Expr::Literal(
                                LiteralNode {
                                    value: DataType::Atomic(AtomicType::Int64(2))
                                }
                                .into()
                            )]
                        }
                        .into()
                    )
                ]
            }
            .into()
        )
    );
    Ok(())
}
