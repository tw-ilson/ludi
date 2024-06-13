use interpreter::ast::*;
// use interpreter::ast_printer::PrintAST;
use interpreter::atomic::Atom;
use interpreter::atomic::AtomicData;
use interpreter::atomic::AtomicType::*;
use interpreter::parser::*;
use interpreter::scanner::*;
// use interpreter::tokens::*;
use std::vec;

#[test]
fn scan_arithmetic_tokens() {
    let src = "+ - * /";
    let src2 = "+-*/";
    let s = scanner(src);
    use interpreter::tokens::Token::*;
    assert_eq!(
        s.map(|tok| tok.token).collect::<Vec<Token>>(),
        vec![PLUS, MINUS, STAR, SLASH]
    );
    let s = scanner(src2);
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
    use interpreter::tokens::Token::*;
    assert_eq!(
        scanner(&src).next().unwrap(),
        TokenData {
            token: NUMBER_LITERAL("12".into()),
            line: 1
        }
    );
    assert_eq!(
        scanner(&src2).next().unwrap(),
        TokenData {
            token: NUMBER_LITERAL("4712.08".into()),
            line: 1
        }
    );
    assert_eq!(
        scanner(&src3)
            .map(|tok| tok.token)
            .collect::<Vec<Token>>(),
        vec![
            NUMBER_LITERAL("2.0".into()),
            PLUS,
            NUMBER_LITERAL("0.3".into())
        ]
    );
}

#[test]
fn scan_string_literal() {
    let src = "\"Hello,\" + \" world!\"";
    use interpreter::tokens::Token::*;
    let toks: Vec<Token> = scanner(src).map(|d| d.token).collect::<Vec<Token>>();
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
    use interpreter::tokens::Token::*;
    assert_eq!(
        scanner(&src).next().unwrap(),
        TokenData {
            token: IDENTIFIER("a".to_string()),
            line: 1
        }
    );
    assert_eq!(
        "a = 10".to_string().parse().expect("failed to parse")[0],
        Stmt::AssignStmt(
            AssignStmtNode {
                name: TokenData {
                    token: IDENTIFIER("a".into()),
                    line: 1
                },
                initializer: Expr::AtomicCast(
                    AtomicCastNode {
                        value: LiteralNode {
                            value: AtomicData {
                                ty: Int64Type,
                                data: Atom { int64: 10 }
                            }
                        }
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
    use interpreter::tokens::Token::*;
    let toks: Vec<Token> = scanner(src).map(|d| d.token).collect::<Vec<Token>>();
    assert_eq!(
        toks.as_slice(),
        &[
            PRINT,
            NUMBER_LITERAL("2".into()),
            PLUS,
            NUMBER_LITERAL("2".into()),
            SEMICOLON
        ]
    );
}

#[test]
fn binary_expr1() {
    use Expr::*;
    use Token::*;
    let mut src = "5<3".to_string();
    let s: &Stmt = &src.parse().expect("failed to parse")[0];

    let s_test = &Stmt::ExprStmt(
        ExprStmtNode {
            expression: Binary(
                BinaryNode {
                    left: AtomicCast(
                        AtomicCastNode {
                            value: LiteralNode {
                                value: AtomicData::from(TokenData {
                                    token: NUMBER_LITERAL("5".into()),
                                    line: 1,
                                }),
                            },
                        }
                        .into(),
                    ),
                    operator: TokenData {
                        token: LESS,
                        line: 1,
                    },
                    right: AtomicCast(
                        AtomicCastNode {
                            value: LiteralNode {
                                value: AtomicData::from(TokenData {
                                    token: NUMBER_LITERAL("3".into()),
                                    line: 1,
                                }),
                            },
                        }
                        .into(),
                    ),
                }
                .into(),
            ),
        }
        .into(),
    );

    assert_eq!(s, s_test)
}

#[test]
fn binary_expr2() {
    use Expr::*;
    use Token::*;
    let mut src = "75.4 + 1.006".to_string();
    let s: &Stmt = &src.parse().expect("failed to parse")[0];

    let s_test = &Stmt::ExprStmt(
        ExprStmtNode {
            expression: Binary(
                BinaryNode {
                    left: AtomicCast(
                        AtomicCastNode {
                            value: LiteralNode {
                                value: AtomicData::from(TokenData {
                                    token: NUMBER_LITERAL("75.4".into()),
                                    line: 1,
                                }),
                            },
                        }
                        .into(),
                    ),
                    operator: TokenData {
                        token: PLUS,
                        line: 1,
                    },
                    right: AtomicCast(
                        AtomicCastNode {
                            value: LiteralNode {
                                value: AtomicData::from(TokenData {
                                    token: NUMBER_LITERAL("1.006".into()),
                                    line: 1,
                                }),
                            },
                        }
                        .into(),
                    ),
                }
                .into(),
            ),
        }
        .into(),
    );
    assert_eq!(s, s_test)
}
