use libludi::ast::OptionalTypeSignature;
use libludi::ast::*;
use libludi::atomic::Literal;
use libludi::env::Name;
use libludi::err::ErrorKind;
use libludi::err::ParseError;
use libludi::lex::lex;
use libludi::parser::*;
use libludi::shape::Shape;
use libludi::token::*;
use libludi::types::PrimitiveFuncType;
use pretty_assertions::assert_eq;
use smallvec::smallvec;
use std::str::FromStr;
use std::vec;

fn atom_int(li: &str) -> Expr {
    atom_literal_node(Literal::Int {
        loc: Location { line: 1 },
        atom: String::from(li),
    })
}
fn atom_float(lf: &str) -> Expr {
    atom_literal_node(Literal::Float {
        loc: Location { line: 1 },
        atom: String::from(lf),
    })
}

#[test]
fn scan_arithmetic_tokens() {
    let src = "+ - * /";
    let src2 = "+-*/";
    let s = lex(src);
    use libludi::token::Token::*;
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
    let src = "12 - 7";
    let src2 = "4712.08";
    let src3 = "2.0 + 0.3";
    use libludi::token::Token::*;
    assert_eq!(
        lex(&src).next().unwrap(),
        TokenData {
            token: INTEGER_LITERAL("12".into()),
            loc: Location { line: 1 }
        }
    );
    assert_eq!(
        lex(&src2).next().unwrap(),
        TokenData {
            token: FLOAT_LITERAL("4712.08".into()),
            loc: Location { line: 1 }
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
    use libludi::token::Token::*;
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
fn let_basic() -> anyhow::Result<()> {
    let expr = expression(&mut lex("let a = 10;"))?;
    assert_eq!(
        expr,
        let_node(
            Name::from_str("a")?,
            atom_literal_node(Literal::Int {
                loc: Location { line: 1 },
                atom: String::from("10")
            }),
            None
        )
    );
    Ok(())
}

#[test]
fn let_with_body() -> anyhow::Result<()> {
    let expr = expression(&mut lex("let a = 2 in a+2"))?;
    assert_eq!(
        expr,
        Expr::Let(
            LetNode {
                name: libludi::env::Name {
                    name: "a".into(),
                    loc: Location { line: 1 }
                },
                initializer: atom_int("2"),
                region: Some(Expr::FnCall(
                    FnCallNode {
                        callee: Callee::Primitive(PrimitiveFuncType::Add),
                        args: vec![
                            Expr::Term(
                                TermNode {
                                    name: libludi::env::Name {
                                        name: "a".into(),
                                        loc: Location { line: 1 }
                                    }
                                }
                                .into()
                            ),
                            atom_int("2")
                        ]
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
    let expr = expression(&mut lex(
        "let a = 2 in let b = 4 in let c = a + b in foo(a, b, c)",
    ))?;
    assert_eq!(
        expr,
        Expr::Let(
            LetNode {
                name: libludi::env::Name {
                    name: "a".into(),
                    loc: Location { line: 1 }
                },
                initializer: atom_int("2"),
                region: Some(Expr::Let(
                    LetNode {
                        name: libludi::env::Name {
                            name: "b".into(),
                            loc: Location { line: 1 }
                        },
                        initializer: atom_int("4"),
                        region: Some(Expr::Let(
                            LetNode {
                                name: libludi::env::Name {
                                    name: "c".into(),
                                    loc: Location { line: 1 }
                                },
                                initializer: Expr::FnCall(
                                    FnCallNode {
                                        callee: Callee::Primitive(PrimitiveFuncType::Add),
                                        args: vec![
                                            Expr::Term(
                                                TermNode {
                                                    name: libludi::env::Name {
                                                        name: "a".into(),
                                                        loc: Location { line: 1 }
                                                    }
                                                }
                                                .into()
                                            ),
                                            Expr::Term(
                                                TermNode {
                                                    name: libludi::env::Name {
                                                        name: "b".into(),
                                                        loc: Location { line: 1 }
                                                    }
                                                }
                                                .into()
                                            ),
                                        ]
                                    }
                                    .into()
                                ),
                                region: Some(Expr::FnCall(
                                    FnCallNode {
                                        callee: Callee::Expression(Expr::Term(
                                            TermNode {
                                                name: libludi::env::Name {
                                                    name: "foo".into(),
                                                    loc: Location { line: 1 }
                                                }
                                            }
                                            .into()
                                        )),
                                        args: vec![
                                            Expr::Term(
                                                TermNode {
                                                    name: libludi::env::Name {
                                                        name: "a".into(),
                                                        loc: Location { line: 1 }
                                                    }
                                                }
                                                .into()
                                            ),
                                            Expr::Term(
                                                TermNode {
                                                    name: libludi::env::Name {
                                                        name: "b".into(),
                                                        loc: Location { line: 1 }
                                                    }
                                                }
                                                .into()
                                            ),
                                            Expr::Term(
                                                TermNode {
                                                    name: libludi::env::Name {
                                                        name: "c".into(),
                                                        loc: Location { line: 1 }
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
    use libludi::token::Token::*;
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

#[test]
fn binary_expr1() -> anyhow::Result<()> {
    use itertools::Itertools;
    let src = "5 < 3";
    dbg!(lex(src).into_iter().collect_vec());
    let s: Expr = expression(&mut lex(src))?;
    let s_test = Expr::FnCall(
        FnCallNode {
            callee: Callee::Primitive(PrimitiveFuncType::Lt),
            args: vec![atom_int("5"), atom_int("3")],
        }
        .into(),
    );

    assert_eq!(s, s_test);
    Ok(())
}

#[test]
fn binary_expr2() -> anyhow::Result<()> {
    use Expr::*;
    let s = expression(&mut lex("75.4 + 1.006"))?;

    let s_test = FnCall(
        FnCallNode {
            callee: Callee::Primitive(PrimitiveFuncType::Add),
            args: vec![atom_float("75.4"), atom_float("1.006")],
        }
        .into(),
    );
    assert_eq!(s, s_test);
    Ok(())
}

#[test]
fn test_binary_operation() -> anyhow::Result<()> {
    use std::str::FromStr;
    let prg = expression(&mut lex("a + b * c - d"))?;
    assert_eq!(
        prg,
        Expr::FnCall(
            FnCallNode {
                callee: Callee::Primitive(PrimitiveFuncType::Sub),
                args: vec![
                    Expr::FnCall(
                        FnCallNode {
                            callee: Callee::Primitive(PrimitiveFuncType::Add),
                            args: vec![
                                Expr::Term(
                                    TermNode {
                                        name: Name::from_str("a")?
                                    }
                                    .into()
                                ),
                                Expr::FnCall(
                                    FnCallNode {
                                        callee: Callee::Primitive(PrimitiveFuncType::Mul),
                                        args: vec![
                                            Expr::Term(
                                                TermNode {
                                                    name: Name::from_str("b")?
                                                }
                                                .into()
                                            ),
                                            Expr::Term(
                                                TermNode {
                                                    name: Name::from_str("c")?
                                                }
                                                .into()
                                            )
                                        ]
                                    }
                                    .into()
                                )
                            ]
                        }
                        .into()
                    ),
                    Expr::Term(
                        TermNode {
                            name: Name::from_str("d")?
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

#[test]
fn lambda_expr() -> anyhow::Result<()> {
    use std::str::FromStr;
    let prg = expression(&mut lex("|a[3] b[3]|  -> [3] { a + b }"))?;
    assert_eq!(
        prg,
        Expr::FnDef(
            FnDefNode {
                signature: CallSignature {
                    args: vec![
                        (
                            Name::from_str("a")?,
                            OptionalTypeSignature(None, smallvec::smallvec![3].into())
                        ),
                        (
                            Name::from_str("b")?,
                            OptionalTypeSignature(None, smallvec::smallvec![3].into())
                        ),
                    ],
                    ret: OptionalTypeSignature(None, smallvec::smallvec![3].into()),
                },
                body: Expr::FnCall(
                    FnCallNode {
                        callee: Callee::Primitive(PrimitiveFuncType::Add),
                        args: vec![
                            Expr::Term(
                                TermNode {
                                    name: Name::from_str("a")?
                                }
                                .into()
                            ),
                            Expr::Term(
                                TermNode {
                                    name: Name::from_str("b")?
                                }
                                .into()
                            )
                        ]
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
fn fndef_complex_body() -> anyhow::Result<()> {
    use std::str::FromStr;
    let prg = expression(&mut lex("fn foo(x[5] y[5]) -> [5] { (x + y) * 2 }"))?;
    assert_eq!(
        prg,
        Expr::Let(
            LetNode {
                name: Name::from("foo"),
                initializer: Expr::FnDef(
                    FnDefNode {
                        signature: CallSignature {
                            args: vec![
                                (
                                    Name::from_str("x")?,
                                    OptionalTypeSignature(None, smallvec::smallvec![5].into())
                                ),
                                (
                                    Name::from_str("y")?,
                                    OptionalTypeSignature(None, smallvec::smallvec![5].into())
                                ),
                            ],
                            ret: OptionalTypeSignature(None, smallvec::smallvec![5].into()),
                        },
                        body: Expr::FnCall(
                            FnCallNode {
                                callee: Callee::Primitive(PrimitiveFuncType::Mul),
                                args: vec![
                                    Expr::FnCall(
                                        FnCallNode {
                                            callee: Callee::Primitive(PrimitiveFuncType::Add),
                                            args: vec![
                                                Expr::Term(
                                                    TermNode {
                                                        name: Name::from_str("x")?
                                                    }
                                                    .into()
                                                ),
                                                Expr::Term(
                                                    TermNode {
                                                        name: Name::from_str("y")?
                                                    }
                                                    .into()
                                                )
                                            ]
                                        }
                                        .into()
                                    ),
                                    Expr::AtomLiteral(
                                        AtomLiteralNode {
                                            value: Literal::Int {
                                                loc: Location { line: 1 },
                                                atom: String::from("2")
                                            }
                                        }
                                        .into()
                                    )
                                ]
                            }
                            .into()
                        )
                    }
                    .into()
                ),
                region: None
            }
            .into()
        )
    );
    Ok(())
}

#[test]
fn fncall() -> anyhow::Result<()> {
    use std::str::FromStr;
    let prg = expression(&mut lex("foo(1, 2)"))?;
    assert_eq!(
        prg,
        Expr::FnCall(
            FnCallNode {
                callee: Callee::Expression(Expr::Term(
                    TermNode {
                        name: Name::from_str("foo")?
                    }
                    .into()
                )),
                args: vec![
                    Expr::AtomLiteral(
                        AtomLiteralNode {
                            value: Literal::Int {
                                loc: Location { line: 1 },
                                atom: String::from("1")
                            }
                        }
                        .into()
                    ),
                    Expr::AtomLiteral(
                        AtomLiteralNode {
                            value: Literal::Int {
                                loc: Location { line: 1 },
                                atom: String::from("2")
                            }
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
fn nested_fncall() -> anyhow::Result<()> {
    use std::str::FromStr;
    let prg = expression(&mut lex("foo(bar(1), baz(2))"))?;
    assert_eq!(
        prg,
        Expr::FnCall(
            FnCallNode {
                callee: Callee::Expression(Expr::Term(
                    TermNode {
                        name: Name::from_str("foo")?
                    }
                    .into()
                )),
                args: vec![
                    Expr::FnCall(
                        FnCallNode {
                            callee: Callee::Expression(Expr::Term(
                                TermNode {
                                    name: Name::from_str("bar")?
                                }
                                .into()
                            )),
                            args: vec![Expr::AtomLiteral(
                                AtomLiteralNode {
                                    value: Literal::Int {
                                        loc: Location { line: 1 },
                                        atom: String::from("1")
                                    }
                                }
                                .into()
                            )]
                        }
                        .into()
                    ),
                    Expr::FnCall(
                        FnCallNode {
                            callee: Callee::Expression(Expr::Term(
                                TermNode {
                                    name: Name::from_str("baz")?
                                }
                                .into()
                            )),
                            args: vec![Expr::AtomLiteral(
                                AtomLiteralNode {
                                    value: Literal::Int {
                                        loc: Location { line: 1 },
                                        atom: String::from("2")
                                    }
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

#[test]
fn parse_array1() -> anyhow::Result<()> {
    let mut tokens = lex("[ 1 2 3 2 1 ]");
    let expr = expression(&mut tokens)?;

    assert_eq!(
        expr,
        frame_node(vec![
            atom_int("1"),
            atom_int("2"),
            atom_int("3"),
            atom_int("2"),
            atom_int("1")
        ])
    );
    Ok(())
}

#[test]
fn frame1() -> anyhow::Result<()> {
    let expr = expression(&mut lex("let a = 1; let b = 2; [ a b ]"))?;
    assert_eq!(
        expr,
        let_node(
            Name::from_str("a")?,
            atom_int("1"),
            Some(let_node(
                Name::from_str("b")?,
                atom_int("2"),
                Some(frame_node(vec![
                    term_node(Name::from_str("a")?),
                    term_node(Name::from_str("b")?)
                ]))
            ))
        )
    );
    Ok(())
}

// #[test]
// fn test_frame_illegal() -> anyhow::Result<()> {
//     let expr = expression(&mut lex("[[7 1 2]
//               [9 5]
//               [2 0 5]]"));
//     assert!(expr.is_err());
//     Ok(())
// }

#[test]
fn diff_square() -> anyhow::Result<()> {
    let expr = expression(&mut lex("
        fn diff_square(x[0] y[0]) -> [0] {
            x*x - y*y
        }
    "))?;
    assert_eq!(
        expr,
        Expr::Let(
            LetNode {
                name: Name {
                    name: "diff_square".to_string(),
                    loc: Location { line: 2 },
                },
                initializer: Expr::FnDef(
                    FnDefNode {
                        signature: CallSignature {
                            args: vec![
                                (
                                    Name {
                                        name: "x".to_string(),
                                        loc: Location { line: 2 },
                                    },
                                    OptionalTypeSignature(None, Shape::new(&[0],),),
                                ),
                                (
                                    Name {
                                        name: "y".to_string(),
                                        loc: Location { line: 2 },
                                    },
                                    OptionalTypeSignature(None, Shape::new(&[0]),),
                                ),
                            ],
                            ret: OptionalTypeSignature(None, Shape::new(&[0])),
                        },
                        body: Expr::FnCall(
                            FnCallNode {
                                callee: Callee::Primitive(PrimitiveFuncType::Sub,),
                                args: vec![
                                    Expr::FnCall(
                                        FnCallNode {
                                            callee: Callee::Primitive(PrimitiveFuncType::Mul,),
                                            args: vec![
                                                Expr::Term(
                                                    TermNode {
                                                        name: Name {
                                                            name: "x".to_string(),
                                                            loc: Location { line: 3 },
                                                        },
                                                    }
                                                    .into(),
                                                ),
                                                Expr::Term(
                                                    TermNode {
                                                        name: Name {
                                                            name: "x".to_string(),
                                                            loc: Location { line: 3 },
                                                        },
                                                    }
                                                    .into(),
                                                ),
                                            ],
                                        }
                                        .into(),
                                    ),
                                    Expr::FnCall(
                                        FnCallNode {
                                            callee: Callee::Primitive(PrimitiveFuncType::Mul,),
                                            args: vec![
                                                Expr::Term(
                                                    TermNode {
                                                        name: Name {
                                                            name: "y".to_string(),
                                                            loc: Location { line: 3 },
                                                        },
                                                    }
                                                    .into(),
                                                ),
                                                Expr::Term(
                                                    TermNode {
                                                        name: Name {
                                                            name: "y".to_string(),
                                                            loc: Location { line: 3 },
                                                        },
                                                    }
                                                    .into(),
                                                ),
                                            ],
                                        }
                                        .into(),
                                    ),
                                ],
                            }
                            .into(),
                        ),
                    }
                    .into(),
                ),
                region: None,
            }
            .into(),
        )
    );
    Ok(())
}

#[test]
fn condition1() -> anyhow::Result<()> {
    let expr = expression(&mut lex("if a>b { a } else { b }"))?;
    assert_eq!(
        expr,
        fn_call_node(
            Callee::Primitive(PrimitiveFuncType::If),
            vec![
                fn_call_node(
                    Callee::Primitive(PrimitiveFuncType::Gt),
                    vec![term_node(Name::from("a")), term_node(Name::from("b"))]
                ),
                term_node(Name::from("a")),
                term_node(Name::from("b"))
            ]
        )
    );
    Ok(())
}

#[test]
fn condition2() -> anyhow::Result<()> {
    let expr = expression(&mut lex("if a>b {a} else { if b>c {b} else {c} }"))?;
    assert_eq!(
        expr,
        fn_call_node(
            Callee::Primitive(PrimitiveFuncType::If),
            vec![
                fn_call_node(
                    Callee::Primitive(PrimitiveFuncType::Gt),
                    vec![term_node(Name::from("a")), term_node(Name::from("b"))]
                ),
                term_node(Name::from("a")),
                fn_call_node(
                    Callee::Primitive(PrimitiveFuncType::If),
                    vec![
                        fn_call_node(
                            Callee::Primitive(PrimitiveFuncType::Gt),
                            vec![term_node(Name::from("b")), term_node(Name::from("c"))]
                        ),
                        term_node(Name::from("b")),
                        term_node(Name::from("c"))
                    ]
                )
            ]
        )
    );
    Ok(())
}

// #[test]
// fn test_parse_array2() -> anyhow::Result<()> {
//     let mut tokens = lex("1_2_3_2_1");
//     let expr = expression(&mut tokens)?;
//
//     assert_eq!(
//         expr,
//         array_literal_node(vec![atom_int("1"), atom_int("2"), atom_int("3"), atom_int("2"), atom_int("1")])
//     );
//     Ok(())
// }

// #[test]
// fn trailing_junk_tokens() -> anyhow::Result<()> {
//     expression(&mut lex("1+1 junk+_/=")).expect_err("Uncaught junk at end of file");
//     Ok(())
// }
