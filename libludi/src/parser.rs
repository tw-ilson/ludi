use std::str::FromStr;

use crate::{
    ast::*,
    atomic::Literal,
    env::Name,
    err::{Error, LudiError,  Result},
    lex::{lex, Lexer},
    shape::{Shape, ShapeVec},
    token::{Location, Token, TokenData},
    types::{PrimitiveFuncType, Type},
};

use anyhow::Context;
use itertools::Itertools;
use Token::*;

pub trait Parser {
    fn parse(&mut self) -> Result<Expr>;
}
impl Parser for Lexer<'_> {
    fn parse(&mut self) -> Result<Expr> {
        // (0..).map_while(|_| expression(self)).collect()
        expression(self)
    }
}

macro_rules! parse_failure {
    ($tokens:ident, $msg:expr) => {
        if let Some(bad_tok) = $tokens.peek() {
            Error::at_token(bad_tok.clone(), $msg)
        } else {
            Error::at_token(
                TokenData {
                    token: EOF,
                    loc: Location { line: 0 },
                },
                "reached end of file unexpectedly!"
            )
        }
    };
}

macro_rules! match_next {
    ($tokens:ident, $types:pat) => {
        if let Some(TokenData { token: $types, .. }) = $tokens.peek() {
            $tokens.next()
        } else {
            None
        }
    };
}
macro_rules! expect_next {
    ($tokens:ident, $types:pat) => {
        match_next!($tokens, $types).ok_or(parse_failure!(
            $tokens,
            &format!("expected {}", stringify!($types))
        ))
    };
}

pub fn statement(tokens: &mut Lexer) -> Result<Stmt> {
    if match_next!(tokens, PRINT).is_some() {
        Ok(Stmt::PrintStmt(
            PrintStmtNode {
                expression: expression(tokens)?,
            }
            .into(),
        ))
    // } else if match_next!(tokens, OPEN_BRACE).is_some() {
    //     block(tokens)
    } else {
        Ok(Stmt::ExprStmt(
            ExprStmtNode {
                expression: expression(tokens)?,
            }
            .into(),
        ))
    }
    // match_next!(tokens, SEMICOLON).expect("expected semicolon")
}

pub fn expression(tokens: &mut Lexer) -> Result<Expr> {
    fndef(tokens)
}

fn letexpr(tokens: &mut Lexer) -> Result<Expr> {
    if match_next!(tokens, LET).is_some() {
        let name = expect_next!(tokens, IDENTIFIER(..))?;
        expect_next!(tokens, EQUAL)?;
        Ok(Expr::Let(
            LetNode {
                name: name.try_into()?,
                initializer: value(tokens)?,
                region: {
                    let TokenData { token, loc: _ } = expect_next!(tokens, IN | SEMICOLON)?;
                    match token {
                        IN => Some(expression(tokens)?),
                        SEMICOLON => expression(tokens).ok(), // instead of doing this, we want to
                        // recover from an immediate EOF, for
                        // the sake of interpreter & tests
                        _ => unreachable!(),
                    }
                },
            }
            .into(),
        ))
    } else {
        value(tokens)
    }
}

fn value(tokens: &mut Lexer) -> Result<Expr> {
    lambda(tokens)
}

fn lambda(tokens: &mut Lexer) -> Result<Expr> {
    if match_next!(tokens, VBAR).is_some() {
        let args: Vec<(Name, OptionalTypeSignature)> = (0..)
            .map_while(|_| {
                if let Some(name) = match_next!(tokens, IDENTIFIER(_)) {
                    Some((|| -> Result<(Name, OptionalTypeSignature)> {
                        Ok((name.try_into()?, typesignature(tokens)?))
                    })())
                } else {
                    None
                }
            })
            .collect::<Result<Vec<(Name, OptionalTypeSignature)>>>()?;
        expect_next!(tokens, VBAR)?;
        expect_next!(tokens, ARROW)?;
        let ret = typesignature(tokens)?;
        expect_next!(tokens, OPEN_BRACE)?;
        let body = expression(tokens)?;
        expect_next!(tokens, CLOSE_BRACE)?;
        Ok(Expr::FnDef(
            FnDefNode {
                signature: CallSignature { args, ret },
                body,
            }
            .into(),
        ))
    } else {
        equality(tokens) 
    }
}

fn fndef(tokens: &mut Lexer) -> Result<Expr> {
    if match_next!(tokens, FN).is_some() {
        let name: Name = expect_next!(tokens, IDENTIFIER(_))?.try_into()?;
        expect_next!(tokens, OPEN_PAREN)?;
        let args: Vec<(Name, OptionalTypeSignature)> = (0..)
            .map_while(|_| {
                if let Some(name) = match_next!(tokens, IDENTIFIER(_)) {
                    Some((|| -> Result<(Name, OptionalTypeSignature)> {
                        Ok((name.try_into()?, typesignature(tokens)?))
                    })())
                } else {
                    None
                }
            })
            .collect::<Result<Vec<(Name, OptionalTypeSignature)>>>()?;
        expect_next!(tokens, CLOSE_PAREN)?;
        expect_next!(tokens, ARROW)?;
        let ret = typesignature(tokens)?;
        expect_next!(tokens, OPEN_BRACE)?;
        let body = expression(tokens)?;
        expect_next!(tokens, CLOSE_BRACE)?;

        Ok(let_node(
            name,
            fn_def_node(CallSignature { args, ret }, body),
            expression(tokens).ok(),
        ))
    } else {
        letexpr(tokens)
    }
}

fn equality(tokens: &mut Lexer) -> Result<Expr> {
    match comparison(tokens) {
        Ok(mut expr) => {
            while let Some(operator) = match_next!(tokens, EQUAL_EQUAL | BANG_EQUAL) {
                match comparison(tokens) {
                    Ok(right) => {
                        let left = expr;
                        expr = Expr::FnCall(
                            FnCallNode {
                                callee: Callee::Primitive(match operator.token {
                                    EQUAL_EQUAL => PrimitiveFuncType::Equal,
                                    BANG_EQUAL => PrimitiveFuncType::Ne,
                                    _ => unreachable!(),
                                }),
                                args: vec![left, right],
                            }
                            .into(),
                        )
                    }
                    Err(e) => return Err(e),
                }
            }
            Ok(expr)
        }
        Err(e) => Err(e),
    }
}
fn comparison(tokens: &mut Lexer) -> Result<Expr> {
    match term(tokens) {
        Ok(mut expr) => {
            while let Some(operator) =
                match_next!(tokens, GREATER | GREATER_EQUAL | LESS | LESS_EQUAL)
            {
                match term(tokens) {
                    Ok(right) => {
                        let left = expr;
                        expr = Expr::FnCall(
                            FnCallNode {
                                callee: Callee::Primitive(match operator.token {
                                    GREATER => PrimitiveFuncType::Gt,
                                    GREATER_EQUAL => PrimitiveFuncType::GtEq,
                                    LESS => PrimitiveFuncType::Gt,
                                    LESS_EQUAL => PrimitiveFuncType::LtEq,
                                    _ => unreachable!(),
                                }),
                                args: vec![left, right],
                            }
                            .into(),
                        )
                    }
                    Err(e) => return Err(e),
                }
            }
            Ok(expr)
        }
        Err(e) => return Err(e),
    }
}
fn term(tokens: &mut Lexer) -> Result<Expr> {
    match factor(tokens) {
        Ok(mut expr) => {
            while let Some(operator) = match_next!(tokens, PLUS | MINUS) {
                match factor(tokens) {
                    Ok(right) => {
                        let left = expr;
                        expr = Expr::FnCall(
                            FnCallNode {
                                callee: Callee::Primitive(match operator.token {
                                    PLUS => PrimitiveFuncType::Add,
                                    MINUS => PrimitiveFuncType::Sub,
                                    _ => unreachable!(),
                                }),
                                args: vec![left, right],
                            }
                            .into(),
                        )
                    }
                    Err(e) => return Err(e),
                }
            }
            Ok(expr)
        }
        Err(e) => Err(e),
    }
}
fn factor(tokens: &mut Lexer) -> Result<Expr> {
    match unary(tokens) {
        Ok(mut expr) => {
            while let Some(operator) = match_next!(tokens, STAR | SLASH) {
                match unary(tokens) {
                    Ok(right) => {
                        let left = expr;
                        expr = Expr::FnCall(
                            FnCallNode {
                                callee: Callee::Primitive(match operator.token {
                                    STAR => PrimitiveFuncType::Mul,
                                    SLASH => PrimitiveFuncType::Div,
                                    _ => unreachable!(),
                                }),
                                args: vec![left, right],
                            }
                            .into(),
                        )
                    }
                    Err(e) => return Err(e),
                }
            }
            Ok(expr)
        }
        Err(e) => Err(e),
    }
}

fn unary(tokens: &mut Lexer) -> Result<Expr> {
    enum ans {
        YES,
        NO,
    }
    if let Some(operator) = match_next!(tokens, BANG | MINUS) {
        Ok(Expr::FnCall(
            FnCallNode {
                callee: Callee::Primitive(match operator.token {
                    BANG => PrimitiveFuncType::Not,
                    MINUS => PrimitiveFuncType::Neg,
                    _ => unreachable!(),
                }),
                args: vec![unary(tokens)?],
            }
            .into(),
        ))
    } else {
        fncall(tokens)
    }
}

fn fncall(tokens: &mut Lexer) -> Result<Expr> {
    let callee = primary(tokens)?;
    if match_next!(tokens, OPEN_PAREN).is_some() {
        let mut args = Vec::<Expr>::new();
        loop {
            args.push(expression(tokens)?);
            if match_next!(tokens, COMMA).is_none() {
                break;
            }
        }
        expect_next!(tokens, CLOSE_PAREN)?;
        Ok(Expr::FnCall(
            FnCallNode {
                callee: Callee::Expression(callee),
                args,
            }
            .into(),
        ))
    } else {
        Ok(callee)
    }
}

fn typesignature(tokens: &mut Lexer) -> Result<OptionalTypeSignature> {
    if match_next!(tokens, OPEN_BRACKET).is_some() {
        let t_decl: Option<Type> = if let Some(TokenData {
            token: IDENTIFIER(t_id),
            ..
        }) = match_next!(tokens, IDENTIFIER(_))
        {
            // Some(Type::from_str(&t_id)?)
            //
            None // ignore this for now while we work out types
        } else {
            None
        };
        let shape = Shape::from(
            (0..)
                .map_while(|_| match match_next!(tokens, INTEGER_LITERAL(_)) {
                    Some(TokenData {
                        token: INTEGER_LITERAL(n_str),
                        ..
                    }) => Some(
                        n_str
                            .parse::<usize>()
                            .or(Err(Error::parse_err("shape expects an unsigned int"))),
                    ),
                    _ => None,
                })
                .collect::<Result<ShapeVec>>()?,
        );
        expect_next!(tokens, CLOSE_BRACKET)?;
        Ok(OptionalTypeSignature(t_decl, shape))
    } else {
        Ok(OptionalTypeSignature(None, Shape::new(&[])))
    }
}

fn primary(tokens: &mut Lexer) -> Result<Expr> {
    if let Some(name) = match_next!(tokens, IDENTIFIER(_)) {
        Ok(term_node(name.try_into()?))
    } else if match_next!(tokens, OPEN_PAREN).is_some() {
        match expression(tokens) {
            Ok(expr) => {
                let _ = match_next!(tokens, CLOSE_PAREN)
                    .ok_or(parse_failure!(tokens, "expected closing paren"))?;
                Ok(expr)
            }
            Err(e) => Err(e),
        }
    } else if let Some(value) =
        match_next!(tokens, FALSE | TRUE | INTEGER_LITERAL(_) | FLOAT_LITERAL(_))
    {
        Ok(atom_literal_node(value.try_into()?))
    } else {
        array_frame(tokens)
    }
}

fn array_frame(tokens: &mut Lexer) -> Result<Expr> {
    if match_next!(tokens, OPEN_BRACKET).is_some() {
        Ok(Expr::Frame(
            FrameNode {
                expression_list: (0..)
                    .map_while(|_| {
                        if match_next!(tokens, CLOSE_BRACKET).is_some() {
                            None
                        } else {
                            Some(expression(tokens))
                        }
                    })
                    .collect::<Result<Vec<Expr>>>()?,
            }
            .into(),
        ))
    } else {
        sequence(tokens)
    }
}

fn sequence(tokens: &mut Lexer) -> Result<Expr> {
    let seq = atomic_sequence(tokens)?;
    if seq.len() > 1 {
        Ok(array_literal_node(seq))
    } else {
        Ok(atom_literal_node(seq[0].clone()))
    }
}

fn atomic_sequence(tokens: &mut Lexer) -> Result<Vec<Literal>> {
    (0..)
        .map_while(|i| {
            if i == 0 {
                Some(
                    match_next!(tokens, INTEGER_LITERAL(_) | FLOAT_LITERAL(_) | TRUE | FALSE)
                        .ok_or(parse_failure!(tokens, "expected literal expression")),
                )
            } else if match_next!(tokens, COMMA | UNDERSCORE).is_some() {
                Some(
                    match_next!(tokens, INTEGER_LITERAL(_) | FLOAT_LITERAL(_) | TRUE | FALSE)
                        .ok_or(parse_failure!(tokens, "expected literal expression")),
                )
            } else {
                None
            }
        })
        .process_results(|iter| iter.map(Literal::try_from).collect())?
}
