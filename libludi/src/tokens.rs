#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    PLUS,
    MINUS,
    STAR,
    SLASH,
    EQUAL,
    DOT,
    SEMICOLON,
    COMMA,
    UNDERSCORE,
    BACKSLASH,
    OPEN_BRACKET,
    CLOSE_BRACKET,
    OPEN_BRACE,
    CLOSE_BRACE,
    OPEN_PAREN,
    CLOSE_PAREN,

    AND,
    OR,
    BANG,
    GREATER,
    LESS,
    GREATER_EQUAL,
    LESS_EQUAL,
    BANG_EQUAL,
    EQUAL_EQUAL,
    PLUS_EQUAL,
    MINUS_EQUAL,
    STAR_EQUAL,
    SLASH_EQUAL,

    FN,
    LET,
    PRINT,
    TRUE,
    FALSE,

    ARRAY,
    FRAME,


    IDENTIFIER(String),
    STRING_LITERAL(String),
    NUMBER_LITERAL(String),

    EOF
}

pub fn same_variant(lhs: &Token, rhs: &Token) -> bool {
    std::mem::discriminant(lhs) == std::mem::discriminant(rhs)
}

#[derive(Eq, Debug, Clone, PartialEq)]
pub struct TokenData {
    pub token: Token,
    pub line: usize,
}
