use crate::err::{Error, LudiError, ParseErrorKind, Result};
use crate::token::{Location, Token};
use crate::token::{Token::IDENTIFIER, TokenData};
use std::cell::{OnceCell, RefCell};
use std::collections::{BTreeMap, HashMap};
use std::fmt::Debug;
use std::rc::Rc;
use std::str::FromStr;

/*
*  ┌────────────┐       ┌────────────┐
*  │    Map     │       │    Map     │
*  ├────────────┤       ├────────────┤
*  │ d: <data>  │       │ a: <data>  │
*  │ e: <data>  │ ────► │ b: <data>  │ ─ ─ ─►
*  │ f: <data>  │       │ c: <data>  │
*  │ ...        │       │ ...        │
*  └────────────┘       └────────────┘
*/

#[derive(Eq, Debug, Clone)]
pub struct Name {
    pub name: String,
    pub loc: Location,
}

// type Map<N, S> = HashMap<N, S>;
type Map<N, S> = BTreeMap<N, S>;

pub type EnvLink<Symbol> = Option<Box<EnvMap<Symbol>>>;
pub struct EnvMap<Symbol> {
    // A scoped symbol table
    table: Map<Name, Symbol>,
    // The outer scope
    prev: EnvLink<Symbol>,
}

pub struct Env<S> {
    head: EnvLink<S>,
}
impl<S> Env<S> {
    pub fn new() -> Self {
        Self {
            head: Some(EnvMap::new(None).into()),
        }
    }
    pub fn new_with<I>(list: I) -> Self
    where
        I: Iterator<Item = (Name, S)>,
    {
        Self {
            head: Some(EnvMap::new_with(None, list).into()),
        }
    }
    pub fn push(&mut self) {
        let new_node = EnvMap::new(self.head.take());
        self.head = Some(new_node.into());
    }
    pub fn push_with<I>(&mut self, list: I)
    where
        I: Iterator<Item = (Name, S)>,
    {
        let new_node = EnvMap::new_with(self.head.take(), list);
        self.head = Some(new_node.into());
    }
    pub fn pop(&mut self) {
        match self.head.take() {
            None => {}
            Some(frame) => {
                self.head = frame.prev;
            }
        }
    }
    pub fn get(&self, ident: &Name) -> Result<&S> {
        match &self.head {
            Some(table) => table.get(ident),
            None => Err(anyhow::anyhow!("Unknown symbol name: {}", ident.name)),
        }
    }
    pub fn put(&mut self, ident: Name, val: S) -> Option<S> {
        match &mut self.head {
            Some(table) => table.put(ident, val),
            None => None,
        }
    }
}

impl<S> Drop for Env<S> {
    fn drop(&mut self) {
        let mut cur_link = self.head.take();
        while let Some(mut boxed_node) = cur_link {
            cur_link = boxed_node.prev.take();
        }
    }
}

impl<S> EnvMap<S> {
    pub fn new(prev: EnvLink<S>) -> Self {
        Self {
            table: Map::new(),
            prev,
        }
    }
    pub fn new_with<I>(prev: EnvLink<S>, list: I) -> Self
    where
        I: Iterator<Item = (Name, S)>,
    {
        Self {
            table: Map::from_iter(list),
            prev,
        }
    }
    pub fn put(&mut self, ident: Name, val: S) -> Option<S> {
        self.table.insert(ident, val.into())
    }
    pub fn get(&self, ident: &Name) -> Result<&S> {
        if let Some(val) = self.table.get(ident) {
            Ok(val)
        } else if let Some(p) = &self.prev {
            p.get(ident)
        } else {
            Err(anyhow::anyhow!("Unknown symbol name: {}", ident.name))
        }
    }
}
impl<S: Debug> Debug for EnvMap<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}\n->\n{:?}", self.prev, self.table)
    }
}
impl<S> Default for EnvMap<S>
where
    S: Clone,
{
    fn default() -> Self {
        Self::new(None)
    }
}

impl std::hash::Hash for Name {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}
impl std::cmp::Ord for Name {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.name.cmp(&other.name)
    }
}
impl std::cmp::PartialOrd for Name {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.name.cmp(&other.name))
    }
}
impl TryFrom<TokenData> for Name {
    type Error = anyhow::Error;
    fn try_from(value: TokenData) -> Result<Self> {
        if let Token::IDENTIFIER(name) = value.token {
            Ok(Name {
                name,
                loc: value.loc,
            })
        } else {
            Err(Error::parse_err(ParseErrorKind::Ident, "Trying to use non-identifier token as name")
                .at_token(value).into())
        }
    }
}
impl FromStr for Name {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self> {
        Ok(Self {
            name: s.into(),
            loc: Location { line: 1 },
        })
    }
}

impl From<&str> for Name {
    fn from(value: &str) -> Self {
        Self::from_str(value).expect("bad name")
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
