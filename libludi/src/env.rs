use crate::atomic::NumberType;
use crate::data::{Data, DataType};
use crate::err::{err_at_tok, LangError, Result};
use crate::tokens::{Token::IDENTIFIER, TokenData};
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// going to want a better pattern then rc-refcell.
pub type EnvRef = Rc<Env>;
type Symbol = Rc<DataType>;
type EnvMap = RefCell<HashMap<String, Symbol>>;

pub struct Env {
    // A scoped symbol table
    table: EnvMap,
    // The outer scope
    prev: Option<EnvRef>,
}

impl Env {
    pub fn new(prev: Option<Rc<Env>>) -> Env {
        Env {
            table: RefCell::new(HashMap::new()),
            prev,
        }
    }
    pub fn put(&self, ident: &str, val: Symbol) -> Option<Symbol> {
        self.table.borrow_mut().insert(ident.to_owned(), val)
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
impl Default for Env {
    fn default() -> Self {
        Env::new(None)
    }
}
