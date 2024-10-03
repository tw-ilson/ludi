use crate::err::{parse_err, Error, ErrorKind, Result};
use crate::err_at_tok_msg;
use crate::token::{Location, Token};
use crate::token::{Token::IDENTIFIER, TokenData};
use std::cell::{RefCell,OnceCell};
use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;
use std::str::FromStr;

#[derive(Eq, Debug, Clone)]
pub struct Name {
    pub name: String,
    pub loc: Location,
}

pub type EnvRef<Symbol> = Rc<Env<Symbol>>;
type EnvMap<Symbol> = HashMap<Name, Symbol>;

pub struct Env<Symbol>
{
    // A scoped symbol table
    table: EnvMap<Symbol>,
    // The outer scope
    prev: Option<EnvRef<Symbol>>,
}

impl<S> Env<S> where S: Clone {
    pub fn new(prev: Option<Rc<Env<S>>>) -> Env<S> {
        Env {
            table: HashMap::new(),
            prev,
        }
    }
    pub fn put(&mut self, ident: Name, val: S) -> Option<S> {
        self.table
            .insert(ident, val.into())
    }
    pub fn get(&self, ident: &Name) -> Result<S> {
        if let Some(val) = self.table.get(ident) {
            Ok(val.clone())
        } else if let Some(p) = &self.prev {
            p.get(ident)
        } else {
            Err(parse_err!(format!("Unknown symbol name: {}", ident.name)))
        }
    }
}
impl<S:Debug> Debug for Env<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}\n->\n{:?}", self.prev, self.table)
    }
}
impl<S> Default for Env<S> where S:Clone {
    fn default() -> Self {
        Env::new(None)
    }
}

impl std::hash::Hash for Name {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}
impl TryFrom<TokenData> for Name {
    type Error = Error;
    fn try_from(value: TokenData) -> Result<Self> {
        if let Token::IDENTIFIER(name) = value.token {
            Ok(Name {
                name,
                loc: value.loc,
            })
        } else {
            Err(parse_err!(err_at_tok_msg!(
                value,
                "Trying to use non-identifier token as name"
            )))
        }
    }
}
impl FromStr for Name {
    type Err = crate::err::Error;
    fn from_str(s: &str) -> Result<Self> {
        Ok(Self {
            name: s.into(),
            loc: Location { line: 1 }
        })
    }
}
impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
