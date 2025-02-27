#[rustfmt::skip]
macro_rules! nums {
    () => {
        "0"| "1" | "2" | "3"| "4"| "5" | "6" | "7" | "8"| "9"
    };
}
#[rustfmt::skip]
macro_rules! alphabet {
    () => {
        "a"|"b"|"c"|"d"|"e"|"f"|"g"|"h"|"i"|"j"|"k"|"l"|"m"|
            "n"|"o"|"p"|"q"|"r"|"s"|"t"|"u"|"v"|"w"|"x"|"y"|"z"
    };
}
#[rustfmt::skip]
macro_rules! delims {
    () => {
        "+"|"-"|"*"|"/"|">"|"<"|"."|";"|","|"["|"]"|"{"|"}"|"("|")"|"|"
    };
}

use crate::token::{Location, Token, TokenData};
use std::iter::Peekable;
use std::str::Chars;
use unicode_segmentation::{Graphemes, UnicodeSegmentation};
use Token::*;

pub type Lexer<'s> = Peekable<TokenStream<'s>>;

pub trait Lex {
    fn lex(&self) -> Lexer;
}

impl Lex for str {
    fn lex(&self) -> Lexer {
        TokenStream::new(self).peekable()
    }
}

#[derive(Clone)]
pub struct TokenStream<'s> {
    source: Peekable<Graphemes<'s>>,
    // line: usize,
    // eof: bool,
    state: LexState,
}

#[derive(Debug, Clone)]
struct LexState {
    in_quote: bool,
    trailing_equal: bool,
    trailing_whitespace: bool,
    trailing_delim: bool,
    eof: bool,
    line: usize,
}

impl Default for LexState {
    fn default() -> Self {
        Self {
            in_quote: false,
            trailing_equal: false,
            trailing_whitespace: false,
            trailing_delim: false,
            line: 1,
            eof: false,
        }
    }
}

impl Iterator for TokenStream<'_> {
    type Item = TokenData;
    fn next(&mut self) -> Option<Self::Item> {
        let mut charlist: String = String::new();
        loop {
            match self.source.next() {
                Some("\n") => {
                    self.state.line += 1;
                    continue;
                }
                Some(" " | "\t") => {
                    continue;
                }
                Some(c) => {
                    charlist.push_str(c);
                }
                None => {
                    if charlist.is_empty() {
                        return None;
                    }
                    self.state.eof = true;
                    if let Some(token) = self.complete_token(&mut charlist) {
                        return Some(TokenData {
                            token,
                            loc: Location { line: self.state.line },
                        });
                    } else {
                        panic!("remove trailing tokens");
                    }
                }
            }
            if let Some(token) = self.complete_token(&mut charlist) {
                return Some(TokenData {
                    token,
                    loc: Location { line: self.state.line },
                });
            }
        }
    }
}

impl<'s> TokenStream<'s> {
    pub fn new(text: &'s str) -> Self {
        Self {
            source: text.graphemes(true).peekable(),
            state: LexState::default(),
        }
    }
    fn complete_token(&mut self, charlist: &mut String) -> Option<Token> {
        if charlist.len() < 1 {
            return None;
        };
        self.state.in_quote = charlist.starts_with(&['"']);
        self.state.trailing_equal = false;
        self.state.trailing_whitespace = false;
        self.state.trailing_delim = false;
        if let Some(&trailing) = self.source.peek() {
            match trailing {
                " " | "\t" | "\n" => {
                    self.state.trailing_whitespace = true;
                }
                "=" => {
                    self.state.trailing_equal = true;
                }
                delims!() => {
                    self.state.trailing_delim = true;
                }

                _ => {}
            }
        }
        //STRING LITERALS
        if self.state.in_quote {
            charlist.push_str(
                &self
                    .source
                    .by_ref()
                    .take_while(|c| !matches!(c, &"\""))
                    .collect::<String>(),
            );
            let s: String = charlist[1..].to_owned();
            return Some(STRING_LITERAL(s));
        }
        //NUMBER LITERALS
        if let Some(nums!() | ".") = charlist.graphemes(true).next() {
            let mut seen_decimal = false;
            loop {
                match self.source.peek() {
                    Some(&nums!()) => charlist.push_str(self.source.next().unwrap()),
                    Some(&".") => {
                        if !seen_decimal {
                            seen_decimal = true;
                            charlist.push_str(self.source.next().unwrap())
                        } else {
                            panic!("unexpected token: .")
                        }
                    }
                    Some(&alphabet!()) => panic!(),
                    _ => break,
                }
            }
            if seen_decimal {
                return Some(FLOAT_LITERAL(charlist.clone())); // also parse number
            } else {
                return Some(INTEGER_LITERAL(charlist.clone()));
            }
        }

        let tok: Option<Token> = match charlist.as_str() {
            "+" => Some(if self.state.trailing_equal {
                assert_eq!(self.source.next().unwrap(), "=");
                PLUS_EQUAL
            } else {
                PLUS
            }),
            "-" => Some(if self.state.trailing_equal {
                assert_eq!(self.source.next().unwrap(), "=");
                MINUS_EQUAL
            } else if let Some(&">") = self.source.peek() {
                self.source.next();
                ARROW
            } else {
                MINUS
            }),
            "*" => Some(if self.state.trailing_equal {
                assert_eq!(self.source.next().unwrap(), "=");
                STAR_EQUAL
            } else {
                STAR
            }),
            "/" => Some(if self.state.trailing_equal {
                assert_eq!(self.source.next().unwrap(), "=");
                SLASH_EQUAL
            } else {
                SLASH
            }),
            ">" => Some(if self.state.trailing_equal {
                assert_eq!(self.source.next().unwrap(), "=");
                GREATER_EQUAL
            } else {
                GREATER
            }),
            "<" => Some(if self.state.trailing_equal {
                assert_eq!(self.source.next().unwrap(), "=");
                LESS_EQUAL
            } else {
                LESS
            }),
            "!" => Some(if self.state.trailing_equal {
                assert_eq!(self.source.next().unwrap(), "=");
                BANG_EQUAL
            } else {
                BANG
            }),
            "=" => Some(if self.state.trailing_equal {
                assert_eq!(self.source.next().unwrap(), "=");
                EQUAL_EQUAL
            } else {
                EQUAL
            }),
            "." => Some(DOT),
            ":" => Some(COLON),
            ";" => Some(SEMICOLON),
            "_" => Some(UNDERSCORE),
            "|" => Some(VBAR),
            "\\" => Some(BACKSLASH),
            "," => Some(COMMA),
            "[" => Some(OPEN_BRACKET),
            "]" => Some(CLOSE_BRACKET),
            "{" => Some(OPEN_BRACE),
            "}" => Some(CLOSE_BRACE),
            "(" => Some(OPEN_PAREN),
            ")" => Some(CLOSE_PAREN),
            ident => {
                if !self.state.in_quote
                    && (self.state.trailing_whitespace || self.state.trailing_equal || self.state.trailing_delim || self.state.eof)
                {
                    match ident {
                        "true" => Some(TRUE),
                        "false" => Some(FALSE),
                        "and" => Some(AND),
                        "or" => Some(OR),
                        "if" => Some(IF),
                        "else" => Some(ELSE),
                        "elseif" => Some(ELSEIF),
                        "fn" => Some(FN),
                        "let" => Some(LET),
                        "in" => Some(IN),
                        "print" => Some(PRINT),
                        "array" => Some(ARRAY),
                        "frame" => Some(FRAME),
                        _ => Some(IDENTIFIER(charlist.clone())),
                    }
                } else {
                    // possible error here if we want to avoid creating bad idents
                    None
                }
            }
        };
        tok
    }
}
