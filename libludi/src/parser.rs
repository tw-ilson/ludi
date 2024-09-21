use std::str::FromStr;

use crate::{
    array::{Array, Shape, ShapeVec},
    ast::*,
    data::{ArrayType, AtomicType, DataType, DataTypeTag, OptionalTypeSignature},
    env::Name,
    err::{err_at_tok, LangError, Result},
    lex::{lex, Lexer},
    parse_err,
    tokens::{Token, TokenData},
};

use anyhow::Context;
use itertools::Itertools;
use Token::*;

pub trait Parser {
    fn parse(&mut self) -> Result<Expr>;
}
impl<'a> Parser for Lexer<'a> {
    fn parse(&mut self) -> Result<Expr> {
        // (0..).map_while(|_| expression(self)).collect()
        expression(self)
    }
}

macro_rules! parse_failure {
    ($tokens:ident, $msg:expr) => {
        if let Some(bad_tok) = $tokens.peek() {
            LangError::ParseErr(err_at_tok!(bad_tok.clone(), $msg.to_string()))
        } else {
            LangError::ParseErr(err_at_tok!(
                TokenData {
                    token: EOF,
                    line: 0,
                },
                "reached end of file unexpectedly!".to_string()
            ))
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
            format!("expected {}", stringify!($types))
        ))
    };
}

// pub fn declaration(tokens: &mut Lexer) -> Result<Stmt> {
//     let mut tokens2 = tokens.clone();
//     if let Some(name) = match_next!(tokens, IDENTIFIER(..)) {
//         if match_next!(tokens, EQUAL).is_some() {
//             Ok(Expr::Let(
//                 LetNode {
//                     name: name.try_into()?,
//                     initializer: expression(tokens)?,
//                     region: None,
//                 }
//                 .into(),
//             ))
//         } else {
//             statement(&mut tokens2)
//         }
//     } else {
//         statement(tokens)
//     }
// }

// pub fn statement(tokens: &mut Lexer) -> Result<Stmt> {
//     if match_next!(tokens, PRINT).is_some() {
//         Ok(Stmt::PrintStmt(
//             PrintStmtNode {
//                 expression: expression(tokens)?,
//             }
//             .into(),
//         ))
//     // } else if match_next!(tokens, OPEN_BRACE).is_some() {
//     //     block(tokens)
//     } else {
//         Ok(Stmt::ExprStmt(
//             ExprStmtNode {
//                 expression: expression(tokens)?,
//             }
//             .into(),
//         ))
//     }
//     // match_next!(tokens, SEMICOLON).expect("expected semicolon")
// }

pub fn expression(tokens: &mut Lexer) -> Result<Expr> {
    letexpr(tokens)
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
                    expect_next!(tokens, IN | SEMICOLON)?;
                    Some(expression(tokens)?)
                },
            }
            .into(),
        ))
    } else {
        value(tokens)
    }
}

fn value(tokens: &mut Lexer) -> Result<Expr> {
    fndef(tokens)
}

fn fndef(tokens: &mut Lexer) -> Result<Expr> {
    if match_next!(tokens, FN).is_some() {
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
        expect_next!(tokens, ARROW)?;
        let ret = typesignature(tokens)?;
        expect_next!(tokens, CLOSE_PAREN)?;
        expect_next!(tokens, COLON)?;
        let body = expression(tokens)?;
        Ok(Expr::FnDef(
            FnDefNode {
                signature: CallSignature { args, ret },
                body,
            }
            .into(),
        ))
    } else {
        equality(tokens) // NOTE: is this correct?
    }
}

fn equality(tokens: &mut Lexer) -> Result<Expr> {
    match comparison(tokens) {
        Ok(mut expr) => {
            while let Some(operator) = match_next!(tokens, EQUAL_EQUAL | BANG_EQUAL) {
                match comparison(tokens) {
                    Ok(right) => {
                        expr = Expr::BinaryOperation(
                            BinaryOperationNode {
                                left: expr,
                                operator: operator.token.try_into()?,
                                right,
                            }
                            .into(),
                        );
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
                        expr = Expr::BinaryOperation(
                            BinaryOperationNode {
                                left: expr,
                                operator: operator.token.try_into()?,
                                right,
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
fn term(tokens: &mut Lexer) -> Result<Expr> {
    match factor(tokens) {
        Ok(mut expr) => {
            while let Some(operator) = match_next!(tokens, PLUS | MINUS) {
                match factor(tokens) {
                    Ok(right) => {
                        expr = Expr::BinaryOperation(
                            BinaryOperationNode {
                                left: expr,
                                operator: operator.token.try_into()?,
                                right,
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
                        expr = Expr::BinaryOperation(
                            BinaryOperationNode {
                                left: expr,
                                operator: operator.token.try_into()?,
                                right,
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
    if let Some(operator) = match_next!(tokens, BANG | MINUS) {
        Ok(Expr::UnaryOperation(
            UnaryOperationNode {
                operator: operator.token.try_into()?,
                right: unary(tokens)?,
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
        Ok(Expr::FnCall(FnCallNode { callee, args }.into()))
    } else {
        Ok(callee)
    }
}

fn typesignature(tokens: &mut Lexer) -> Result<OptionalTypeSignature> {
    if match_next!(tokens, OPEN_BRACKET).is_some() {
        let t_decl: Option<DataTypeTag> = if let Some(TokenData {
            token: IDENTIFIER(t_id),
            ..
        }) = match_next!(tokens, IDENTIFIER(_))
        {
            Some(DataTypeTag::from_str(&t_id)?)
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
                            .or(parse_err!("shape expects an unsigned int")),
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

fn closure(tokens: &mut Lexer) -> Result<Expr> {
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
        let body = expression(tokens)?;
        Ok(Expr::FnDef(
            FnDefNode {
                signature: CallSignature { args, ret },
                body,
            }
            .into(),
        ))
    } else {
        expression(tokens) // NOTE: is this correct?
    }
}
fn primary(tokens: &mut Lexer) -> Result<Expr> {
    if let Some(name) = match_next!(tokens, IDENTIFIER(_)) {
        Ok(Expr::Term(
            TermNode {
                name: name.try_into()?,
            }
            .into(),
        ))
    } else if match_next!(tokens, OPEN_PAREN).is_some() {
        match expression(tokens) {
            Ok(expression) => {
                let _ = match_next!(tokens, CLOSE_PAREN)
                    .ok_or(parse_failure!(tokens, "expected closing paren"))?;
                Ok(Expr::Grouping(GroupingNode { expression }.into()))
            }
            Err(e) => Err(e),
        }
    } else if let Some(value) =
        match_next!(tokens, FALSE | TRUE | INTEGER_LITERAL(_) | FLOAT_LITERAL(_))
    {
        Ok(Expr::Literal(
            LiteralNode {
                value: DataType::Atomic(value.into()),
            }
            .into(),
        ))
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
        Ok(Expr::Literal(
            LiteralNode {
                value: DataType::Array(ArrayType::parse_seq(seq)?),
            }
            .into(),
        ))
    } else {
        Ok(Expr::Literal(
            LiteralNode {
                value: DataType::Atomic(seq[0].clone()),
            }
            .into(),
        ))
    }
}

fn atomic_sequence(tokens: &mut Lexer) -> Result<Vec<AtomicType>> {
    Ok((0..)
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
        .process_results(|iter| iter.map(|t| t.into()).collect()))?
}

// HELPERS

trait ParseSequence<I> {
    fn parse_seq(seq: I) -> Result<Self>
    where
        Self: Sized;
}

macro_rules! parse_sequence_number_impl {
    ($($variant_name:ident($inner_type:ty),)*) => {
    impl<I: IntoIterator<Item=AtomicType>> ParseSequence<I> for ArrayType {
        fn parse_seq(seq: I) -> Result<Self> {
            let mut seq = seq.into_iter().peekable();
            match seq.peek() {
                None => Err(LangError::ParseErr("tried to parse empty sequence".to_string())),
                $(Some(AtomicType::$variant_name(_)) => Ok(ArrayType::$variant_name(Array::from_iter(
                    seq
                    .map(|i| {
                        if let AtomicType::$variant_name(n) = i {
                            Ok(n)
                        } else {
                            Err(LangError::CompileErr("expected uniform types".to_owned()))
                        }
                    })
                    .collect::<Result<Vec<$inner_type>>>()?,
                ))),
                )*
                _ => todo!()
            }
        }
    }
    };
}
parse_sequence_number_impl!(
    UInt8(u8),
    Int8(i8),
    UInt16(u16),
    Int16(i16),
    UInt32(u32),
    Int32(i32),
    Int64(i64),
    UInt64(u64),
    BFloat16(half::bf16),
    Float16(half::f16),
    Float32(f32),
    Float64(f64),
    Complex(num::Complex<f32>),
);
