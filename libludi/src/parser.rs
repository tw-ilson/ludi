use std::str::FromStr;

use crate::{
    ast::*,
    atomic::Literal,
    env::Name,
    err::{Error, LudiError, Result},
    lex::{lex, Lexer},
    shape::{ArrayProps, Shape, ShapeVec},
    token::{Location, Token, TokenData},
    types::{Arr, Array, Atom, PrimitiveFuncType, Type},
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
                $msg,
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
    if tokens.peek().is_some() {
        if match_next!(tokens, PRINT).is_some() {
            Ok(Stmt::Print({
                let expression = expression(tokens)?;
                expect_next!(tokens, SEMICOLON)?;
                PrintNode { expression }.into()
            }))
        // } else if match_next!(tokens, OPEN_BRACE).is_some() {
        //     block(tokens)
        } else {
            Ok(Stmt::Expr(
                ExprNode {
                    expression: expression(tokens)?,
                }
                .into(),
            ))
        }
    } else {
        Ok(unit_node())
    }
}

pub fn expression(tokens: &mut Lexer) -> Result<Expr> {
    fndef(tokens)
}

fn fndef(tokens: &mut Lexer) -> Result<Expr> {
    if match_next!(tokens, FN).is_some() {
        let name: Name = expect_next!(tokens, IDENTIFIER(_))?.try_into()?;
        expect_next!(tokens, OPEN_PAREN)?;
        let mut args: Vec<Arg> = Vec::new();
        while let Some(name) = match_next!(tokens, IDENTIFIER(_)) {
            args.push(Arg(name.try_into()?, typesignature(tokens)?));
            if !match_next!(tokens, COMMA).is_some() {
                break;
            }
        }
        expect_next!(tokens, CLOSE_PAREN)?;
        let ret = if match_next!(tokens, ARROW).is_some() {
            vec![typesignature(tokens)?] // TODO: support multiple return args
        } else {
            vec![Type::Atom(Atom::Unit)]
        };
        expect_next!(tokens, OPEN_BRACE)?;
        let body = expression(tokens)?;
        expect_next!(tokens, CLOSE_BRACE)?;

        Ok(let_node(
            name,
            fn_def_node(FuncSignature { args, ret }, body),
            expression(tokens).ok(),
        ))
    } else {
        letexpr(tokens)
    }
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
                        SEMICOLON => expression(tokens).ok(),
                        // TODO: instead of doing this, we want to
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
        let mut args: Vec<Arg> = Vec::new();
        while let Some(name) = match_next!(tokens, IDENTIFIER(_)) {
            args.push(Arg(name.try_into()?, typesignature(tokens)?));
            if !match_next!(tokens, COMMA).is_some() {
                break;
            }
        }
        expect_next!(tokens, VBAR)?;
        let ret = if match_next!(tokens, ARROW).is_some() {
            vec![typesignature(tokens)?] // TODO: support multiple return args
        } else {
            vec![Type::Atom(Atom::Unit)]
        };
        expect_next!(tokens, OPEN_BRACE)?;
        let body = expression(tokens)?;
        expect_next!(tokens, CLOSE_BRACE)?;
        Ok(Expr::FnDef(
            FnDefNode {
                signature: FuncSignature { args, ret },
                body,
            }
            .into(),
        ))
    } else {
        conditional(tokens)
    }
}

fn typesignature(tokens: &mut Lexer) -> Result<Type> {
    if match_next!(tokens, OPEN_BRACKET).is_some() {
        let element: Atom = match match_next!(tokens, IDENTIFIER(_)) {
            Some(TokenData {
                token: IDENTIFIER(t_id),
                ..
            }) => Atom::try_from(&*t_id)?,
            _ => Atom::Unit,
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
                .collect::<Result<Shape>>()?,
        );
        let ty = if shape.rank() > 0 {
            Type::Array(Array::Arr(Arr { element, shape }))
        } else {
            Type::Atom(element)
        };
        expect_next!(tokens, CLOSE_BRACKET)?;
        Ok(ty)
    } else {
        Ok(Type::Atom(Atom::Unit))
    }
}

fn conditional(tokens: &mut Lexer) -> Result<Expr> {
    if match_next!(tokens, IF).is_some() {
        let condexpr = expression(tokens)?;
        expect_next!(tokens, OPEN_BRACE)?;
        let thenexpr = expression(tokens)?;
        expect_next!(tokens, CLOSE_BRACE)?;
        expect_next!(tokens, ELSE)?;
        expect_next!(tokens, OPEN_BRACE)?;
        let elexpr = expression(tokens)?;
        expect_next!(tokens, CLOSE_BRACE)?;
        Ok(fn_call_node(
            Callee::Primitive(PrimitiveFuncType::If),
            vec![condexpr, thenexpr, elexpr],
        ))
    } else {
        equality(tokens)
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
                                    EQUAL_EQUAL => PrimitiveFuncType::Eq,
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
                                    LESS => PrimitiveFuncType::Lt,
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
    let mut expr = primary(tokens)?;
    loop {
        if match_next!(tokens, OPEN_PAREN).is_some() {
            expr = {
                let mut args = Vec::<Expr>::new();
                if match_next!(tokens, CLOSE_PAREN).is_none() {
                    loop {
                        if args.len() > 255 {
                            return Err(Error::at_token(tokens.next().unwrap(), "Functions with >255 arguments are not allowed"))
                        }
                        let a = expression(tokens)?;
                        args.push(a);
                        if match_next!(tokens, COMMA).is_none() {
                            break;
                        }
                    }
                    expect_next!(tokens, CLOSE_PAREN)?;
                }
                Expr::FnCall(
                    FnCallNode {
                        callee: match &expr {
                            Expr::Term(node) => match (&*node.name.name).try_into() {
                                Ok(prim_ty) => Callee::Primitive(prim_ty),
                                Err(_) => Callee::Expression(expr),
                            },
                            _ => Callee::Expression(expr),
                        },
                        args,
                    }
                    .into(),
                )
            };
        } else {
            break;
        }
    }
    Ok(expr)
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
