use crate::atomic::NumberType;
use crate::data::{Data, DataType};
use crate::err::{err_at_tok, LangError, Result};
use crate::tokens::{Token::IDENTIFIER, TokenData};
use std::any::Any;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type EnvRef<'this, 'prev> = Rc<Env<'this, 'prev>>;
type Symbol<'a> = Cow<'a, DataType>;
type EnvMap<'a> = RefCell<HashMap<String, Symbol<'a>>>;

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
    pub fn put(&self, ident: &str, val: DataType) -> Option<Symbol> {
        self.table
            .borrow_mut()
            .insert(ident.to_owned(), Cow::Owned(val))
    }
    pub fn get(&self, ident: &str, line: usize) -> Result<Symbol> {
        if let Some(val) = self.table.borrow().get(ident) {
            Ok(val.clone())
        } else if let Some(p) = &self.prev {
            p.get(ident, line)
        } else {
            Err(LangError::ParseErr(err_at_tok!(
                TokenData {
                    token: IDENTIFIER(ident.to_owned()),
                    line,
                },
                "Unknown Symbol name!".to_string()
            )))
        }
    }
}
impl Default for Env<'_, '_> {
    fn default() -> Self {
        Env::new(None)
    }
}
