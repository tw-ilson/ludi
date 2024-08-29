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
pub use crate::tokens::*;
use std::iter::Peekable;
use std::str::Chars;
use unicode_segmentation::{Graphemes, UnicodeSegmentation};
use Token::*;

pub type Lexer<'a> = Peekable<TokenStream<'a>>;
pub fn lex<'a>(s: &'a str) -> Lexer<'a> {
    TokenStream::new(s).peekable()
}

#[derive(Clone)]
pub struct TokenStream<'a> {
    source: Peekable<Graphemes<'a>>,
    line: usize,
    eof: bool,
}
impl<'a> Iterator for TokenStream<'a> {
    type Item = TokenData;
    fn next(&mut self) -> Option<Self::Item> {
        let mut charlist: String = String::new();
        loop {
            match self.source.next() {
                Some("\n") => {
                    self.line += 1;
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
                    self.eof = true;
                    if let Some(token) = self.complete_token(&mut charlist) {
                        return Some(TokenData {
                            token,
                            line: self.line,
                        });
                    } else {
                        panic!("remove trailing tokens");
                    }
                }
            }
            if let Some(token) = self.complete_token(&mut charlist) {
                return Some(TokenData {
                    token,
                    line: self.line,
                });
            }
        }
    }
}

impl<'a> TokenStream<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            source: text.graphemes(true).peekable(),
            line: 1,
            eof: false,
        }
    }
    fn complete_token(&mut self, charlist: &mut String) -> Option<Token> {
        if charlist.len() < 1 {
            return None;
        };
        let in_quote: bool = charlist.starts_with(&['"']);
        let mut trailing_equal: bool = false;
        let mut trailing_whitespace: bool = false;
        let mut trailing_delim: bool = false;
        if let Some(&trailing) = self.source.peek() {
            match trailing {
                " " | "\t" | "\n" => {
                    trailing_whitespace = true;
                }
                "=" => {
                    trailing_equal = true;
                }
                delims!() => {
                    trailing_delim = true;
                }

                _ => {}
            }
        }
        //STRING LITERALS
        if in_quote {
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
            "+" => Some(if trailing_equal {
                assert_eq!(self.source.next().unwrap(), "=");
                PLUS_EQUAL
            } else {
                PLUS
            }),
            "-" => Some(if trailing_equal {
                assert_eq!(self.source.next().unwrap(), "=");
                MINUS_EQUAL
            } else if let Some(&">") = self.source.peek() {
                self.source.next();
                ARROW
            } else {
                MINUS
            }),
            "*" => Some(if trailing_equal {
                assert_eq!(self.source.next().unwrap(), "=");
                STAR_EQUAL
            } else {
                STAR
            }),
            "/" => Some(if trailing_equal {
                assert_eq!(self.source.next().unwrap(), "=");
                SLASH_EQUAL
            } else {
                SLASH
            }),
            ">" => Some(if trailing_equal {
                assert_eq!(self.source.next().unwrap(), "=");
                GREATER_EQUAL
            } else {
                GREATER
            }),
            "<" => Some(if trailing_equal {
                assert_eq!(self.source.next().unwrap(), "=");
                LESS_EQUAL
            } else {
                LESS
            }),
            "!" => Some(if trailing_equal {
                assert_eq!(self.source.next().unwrap(), "=");
                BANG_EQUAL
            } else {
                BANG
            }),
            "=" => Some(if trailing_equal {
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
            "true" => Some(TRUE),
            "false" => Some(FALSE),
            "and" => Some(AND),
            "or" => Some(OR),
            "fn" => Some(FN),
            "let" => Some(LET),
            "in" => Some(IN),
            "print" => Some(PRINT),
            "array" => Some(ARRAY),
            "frame" => Some(FRAME),
            _ => {
                if !in_quote
                    && (trailing_whitespace || trailing_equal || trailing_delim || self.eof)
                {
                    Some(IDENTIFIER(charlist.clone()))
                } else {
                    None
                }
            }
        };
        tok
    }
}
