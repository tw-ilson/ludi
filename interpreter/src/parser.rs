use libludi::{
    array::ArrayType,
    ast::*,
    atomic::NumberType,
    err::{err_at_tok, LangError, Result},
    scanner::{scanner, Scanner},
    tokens::{Token, TokenData},
};

use std::iter::Peekable;
use Token::*;

pub trait Parser {
    fn parse(&mut self) -> Result<Vec<Stmt>>;
}

impl Parser for String {
    fn parse(&mut self) -> Result<Vec<Stmt>> {
        scanner(self).parse()
    }
}

impl<'a> Parser for Scanner<'a> {
    fn parse(&mut self) -> Result<Vec<Stmt>> {
        Ok((0..).map_while(|_| declaration(self).ok()).collect())
    }
}

macro_rules! parse_err {
    ($tokens:ident, $msg:literal) => {
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
        if let Some(TokenData { token, .. }) = $tokens.peek() {
            if matches!(token, $types) {
                $tokens.next()
            } else {
                None
            }
        } else {
            None
        }
    };
}

pub fn declaration(tokens: &mut Scanner) -> Result<Stmt> {
    let mut tokens2 = tokens.clone();
    if let Some(name) = match_next!(tokens, IDENTIFIER(..)) {
        if let Some(_) = match_next!(tokens, EQUAL) {
            Ok(Stmt::AssignStmt(
                AssignStmtNode {
                    name,
                    initializer: expression(tokens)?,
                }
                .into(),
            ))
        } else {
            statement(&mut tokens2)
        }
    } else {
        statement(tokens)
    }
}

pub fn statement(tokens: &mut Scanner) -> Result<Stmt> {
    if match_next!(tokens, PRINT).is_some() {
        Ok(Stmt::PrintStmt(
            PrintStmtNode {
                expression: expression(tokens)?,
            }
            .into(),
        ))
    } else if match_next!(tokens, OPEN_BRACE).is_some() {
        block(tokens)
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

fn block(tokens: &mut Scanner) -> Result<Stmt> {
    let mut statements = Vec::new();
    while match_next!(tokens, CLOSE_BRACE).is_none() {
        statements.push(declaration(tokens)?)
    }
    Ok(Stmt::BlockStmt(BlockStmtNode { statements }.into()))
}

pub fn expression(tokens: &mut Scanner) -> Result<Expr> {
    if match_next!(tokens, IF).is_some() {
        Ok(Expr::Conditional(
            ConditionalNode {
                cond: expression(tokens)?,
                body: statement(tokens)?,
                elsebody: {
                    if match_next!(tokens, ELSE).is_some() {
                        statement(tokens)?.into()
                    } else {
                        None
                    }
                },
            }
            .into(),
        ))
    } else {
        equality(tokens)
    }
}
fn logical_or(tokens: &mut Scanner) -> Result<Expr> {
    let mut expr = logical_and(tokens);
    while let Some(operator) = match_next!(tokens, OR) {
        expr = Ok(Expr::Logical(
            LogicalNode {
                left: expr?,
                operator,
                right: logical_and(tokens)?,
            }
            .into(),
        ))
    }
    expr
}
fn logical_and(tokens: &mut Scanner) -> Result<Expr> {
    let mut expr = equality(tokens);
    while let Some(operator) = match_next!(tokens, OR) {
        expr = Ok(Expr::Logical(
            LogicalNode {
                left: expr?,
                operator,
                right: equality(tokens)?,
            }
            .into(),
        ))
    }
    expr
}
fn equality(tokens: &mut Scanner) -> Result<Expr> {
    match comparison(tokens) {
        Ok(mut expr) => {
            while let Some(operator) = match_next!(tokens, EQUAL_EQUAL | BANG_EQUAL) {
                match comparison(tokens) {
                    Ok(right) => {
                        expr = Expr::Binary(
                            BinaryNode {
                                left: expr,
                                operator,
                                right,
                            }
                            .into(),
                        );
                    }
                    Err(e) => return Err(e),
                }
            }
            return Ok(expr);
        }
        Err(e) => return Err(e),
    }
}
fn comparison(tokens: &mut Scanner) -> Result<Expr> {
    match term(tokens) {
        Ok(mut expr) => {
            while let Some(operator) =
                match_next!(tokens, GREATER | GREATER_EQUAL | LESS | LESS_EQUAL)
            {
                match term(tokens) {
                    Ok(right) => {
                        expr = Expr::Binary(
                            BinaryNode {
                                left: expr,
                                operator,
                                right,
                            }
                            .into(),
                        )
                    }
                    Err(e) => return Err(e),
                }
            }
            return Ok(expr);
        }
        Err(e) => return Err(e),
    }
}
fn term(tokens: &mut Scanner) -> Result<Expr> {
    match factor(tokens) {
        Ok(mut expr) => {
            while let Some(operator) = match_next!(tokens, PLUS | MINUS) {
                match factor(tokens) {
                    Ok(right) => {
                        expr = Expr::Binary(
                            BinaryNode {
                                left: expr,
                                operator,
                                right,
                            }
                            .into(),
                        )
                    }
                    Err(e) => return Err(e),
                }
            }
            return Ok(expr);
        }
        Err(e) => return Err(e),
    }
}
fn factor(tokens: &mut Scanner) -> Result<Expr> {
    match unary(tokens) {
        Ok(mut expr) => {
            while let Some(operator) = match_next!(tokens, STAR | SLASH) {
                match unary(tokens) {
                    Ok(right) => {
                        expr = Expr::Binary(
                            BinaryNode {
                                left: expr,
                                operator,
                                right,
                            }
                            .into(),
                        )
                    }
                    Err(e) => return Err(e),
                }
            }
            return Ok(expr);
        }
        Err(e) => return Err(e),
    }
}
//TODO:
fn fncall(tokens: &mut Scanner) -> Result<Expr> {
    let callee = primary(tokens);
    if match_next!(tokens, OPEN_PAREN).is_some() {}
    callee
}
fn end_call(tokens: &mut Scanner, callee: Expr) -> Result<Expr> {
    let args = if match_next!(tokens, CLOSE_PAREN).is_some() {
        Vec::new()
    } else {
        (0..)
            .map_while(|i| {
                if i == 0 {
                    Some(expression(tokens))
                } else {
                    if match_next!(tokens, COMMA).is_some() {
                        Some(expression(tokens))
                    } else {
                        // maybe add more error checking
                        None
                    }
                }
            })
            .collect::<Result<Vec<Expr>>>()?
    };
    Ok(Expr::FnCall(FnCallNode { callee, args }.into()))
}
fn unary(tokens: &mut Scanner) -> Result<Expr> {
    if let Some(operator) = match_next!(tokens, BANG | MINUS) {
        Ok(Expr::Unary(
            UnaryNode {
                operator,
                right: unary(tokens)?,
            }
            .into(),
        ))
    } else {
        primary(tokens)
    }
}
fn sequence(tokens: &mut Scanner) -> Result<Expr> {
    // this can probably be improved...
    let seq: Vec<TokenData> = (0..)
        .map_while(|i| {
            if i == 0 {
                Some(
                    match_next!(tokens, NUMBER_LITERAL(_) | TRUE | FALSE)
                        .ok_or(parse_err!(tokens, "expected literal expression")),
                )
            } else {
                if match_next!(tokens, COMMA | UNDERSCORE).is_some() {
                    Some(
                        match_next!(tokens, NUMBER_LITERAL(_) | TRUE | FALSE)
                            .ok_or(parse_err!(tokens, "expected literal expression")),
                    )
                } else {
                    None
                }
            }
        })
        .collect::<Result<Vec<TokenData>>>()?;
    let seq: Vec<NumberType> = seq.into_iter().map(|t| t.into()).collect();
    if seq.len() > 1 {
        Ok(Expr::Sequence(
            SequenceNode {
                value: seq.try_into()?,
            }
            .into(),
        ))
    } else {
        Ok(Expr::AtomicCast(
            AtomicCastNode {
                value: {
                    LiteralNode {
                        value: seq[0].into(),
                    }
                },
            }
            .into(),
        ))
    }
}
fn primary(tokens: &mut Scanner) -> Result<Expr> {
    if let Some(name) = match_next!(tokens, IDENTIFIER(_)) {
        Ok(Expr::Assignment(AssignmentNode { name }.into()))
    } else if match_next!(tokens, OPEN_PAREN).is_some() {
        match expression(tokens) {
            Ok(expression) => {
                let _ = match_next!(tokens, CLOSE_PAREN)
                    .ok_or(parse_err!(tokens, "expected closing paren"));
                Ok(Expr::Grouping(GroupingNode { expression }.into()))
            }
            Err(e) => Err(e),
        }
    } else if let Some(value) = match_next!(tokens, FALSE | TRUE) {
        Ok(Expr::Literal(
            LiteralNode {
                value: value.into(),
            }
            .into(),
        ))
    } else {
        sequence(tokens)
    }
}
