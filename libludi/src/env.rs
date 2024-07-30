use crate::data::{Data, DataType};
use crate::err::{parse_err, err_at_tok, LangError, Result};
use crate::tokens::Token;
use crate::tokens::{Token::IDENTIFIER, TokenData};
use std::any::Any;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::str::FromStr;

#[derive(Eq, Debug, Clone)]
pub struct Name {
    pub name: String,
    pub line: usize,
}

pub type EnvRef<'this, 'prev> = Rc<Env<'this, 'prev>>;
type Symbol<'a> = Cow<'a, DataType>;
type EnvMap<'a> = RefCell<HashMap<Name, Symbol<'a>>>;

pub struct Env<'this, 'prev>
where
    'prev: 'this,
{
    // A scoped symbol table
    table: EnvMap<'this>,
    // The outer scope
    prev: Option<EnvRef<'prev, 'static>>,
}

impl Env<'_, '_> {
    pub fn new<'a, 'b: 'a>(prev: Option<Rc<Env<'b, 'static>>>) -> Env<'a, 'b> {
        Env {
            table: RefCell::new(HashMap::new()),
            prev,
        }
    }
    pub fn put(&self, ident: Name, val: DataType) -> Option<Symbol> {
        self.table
            .borrow_mut()
            .insert(ident, Cow::Owned(val))
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
impl Default for Env<'_, '_> {
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
