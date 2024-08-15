use crate::data::{Data, DataType};
use crate::err::{parse_err, err_at_tok, LangError, Result};
use crate::tokens::Token;
use crate::tokens::{Token::IDENTIFIER, TokenData};
use std::any::Any;
use std::borrow::Cow;
use std::cell::{RefCell,OnceCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::str::FromStr;

#[derive(Eq, Debug, Clone)]
pub struct Name {
    pub name: String,
    pub line: usize,
}

pub type EnvRef = Rc<Env>;
type Symbol = OnceCell< DataType>;
type EnvMap = RefCell<HashMap<Name, Symbol>>;

pub struct Env
{
    // A scoped symbol table
    table: EnvMap,
    // The outer scope
    prev: Option<EnvRef>,
}

impl Env{
    pub fn new(prev: Option<Rc<Env>>) -> Env {
        Env {
            table: RefCell::new(HashMap::new()),
            prev,
        }
    }
    pub fn put(&self, ident: Name, val: DataType) -> Option<Symbol> {
        self.table
            .borrow_mut()
            .insert(ident, val.into())
    }
    pub fn get(&self, ident: Name) -> Result<Symbol> {
        if let Some(val) = self.table.borrow().get(&ident) {
            Ok(val.clone())
        } else if let Some(p) = &self.prev {
            p.get(ident)
        } else {
            parse_err!(format!("Unknown symbol name: {}", ident.name))
        }
    }
}
impl Default for Env {
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
    type Error = LangError;
    fn try_from(value: TokenData) -> Result<Self> {
        if let Token::IDENTIFIER(name) = value.token {
            Ok(Name {
                name,
                line: value.line,
            })
        } else {
            parse_err!(err_at_tok!(
                value,
                "Trying to use non-identifier token as name"
            ))
        }
    }
}
impl FromStr for Name {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> anyhow::Result<Self, Self::Err> {
        Ok(Self {
            name: s.into(),
            line:1
        })
    }
}
impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
