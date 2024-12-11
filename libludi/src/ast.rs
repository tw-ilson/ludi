use std::fmt::Display;
use std::fs::write;
use std::ops::Deref;

use crate::atomic::Literal;
use crate::env::Name;
use crate::err::{Error, LudiError, Result};
use crate::shape::{ArrayProps, Shape, ShapeOps};
use crate::token::TokenData;
use crate::types::{PrimitiveFuncType, Type};
use itertools::Itertools;

pub type ParseTree = Vec<Expr>;
pub type NodeRef<T> = Box<T>;

#[macro_export]
macro_rules! ast {
    ($(
        $base_name:ident {
         $($variant:ident {
             $($childname:ident: $childtype:ty),*
         })|+
    })*
    ) => {
        $(
        // Enums define the parse tree from grammar
        define_enum!($base_name {
         $($variant {
             $($childname: $childtype),*
         }),+});

        // data structures held by nodes
        define_nodes!($base_name {
         $($variant {
             $($childname: $childtype),*
         }),+});
        // initialize nodes with shorthand function
        define_constructors!($base_name {
         $($variant {
             $($childname: $childtype),*
         }),+});
        )*
    }
}
#[macro_export]
macro_rules! define_enum {
    ($base_name:ident {
         $($variant:ident {
             $($childname:ident: $childtype:ty),*
         }),+
    }
    ) => {
        #[derive(Debug, Clone, PartialEq)]
        pub enum $base_name {
            $(
                $variant(NodeRef<paste::paste!{[<$variant Node>]}>)
            ),+
        }
    }
}
#[macro_export]
macro_rules! define_nodes {
    ($base_name:ident {
         $($variant:ident {
             $($childname:ident: $childtype:ty),*
         }),+
    }
    ) => {
        $(
        paste::paste!{
        #[derive(Debug, Clone, PartialEq)]
        pub struct [<$variant Node>] {$(pub $childname: $childtype),*}
        }
        )+
    };
}

#[macro_export]
macro_rules! define_constructors {
    ($base_name:ident {
         $($variant:ident {
             $($childname:ident: $childtype:ty),*
         }),+
    }
    ) => {
        $(
            paste::paste!{
                // Gives a shorthand syntax for constructing node
                pub fn [< $variant:snake:lower _node >]($($childname : $childtype),*) -> $base_name {
                    $base_name::$variant(
                        [< $variant Node >] {
                            $($childname,)*
                        }.into()
                    )
                }
            }
        )+
    };
}
pub use ast;
pub use define_constructors;
pub use define_enum;

#[derive(Debug, Clone, PartialEq)]
pub enum Callee {
    Expression(Expr),
    Primitive(PrimitiveFuncType),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Arg(pub Name, pub Type);

#[derive(Debug, Eq, Clone)]
pub struct FuncSignature {
    pub args: Vec<Arg>,
    pub ret: Vec<Type>,
}

// AST productions for the lang of format:
// meta-symbol {
//      production { attributes... }
//      ...
//      }
ast! {
    Stmt {
          Expr { expression: Expr }
        | Print { expression: Expr }
        | Unit {}
    }
    Expr {
          FnDef { signature: FuncSignature, body:Expr }
        | FnCall { callee: Callee, args: Vec<Expr> }
        | Frame { expression_list: Vec<Expr> }
        | AtomLiteral { value: Literal }
        | ArrayLiteral { value: Vec<Literal> }
        | Let { name:Name, initializer: Expr, region: Option<Expr> }
        | Term { name: Name }
        // | Condition 
    }
}

impl Display for FuncSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for Arg(_, t) in &self.args {
            write!(f, "{}, ", t)?;
        }
        write!(f, " -> ")?;
        for t in &self.ret {
            write!(f, "{}, ", t)?;
        }
        Ok(())
    }
}

impl PartialEq for FuncSignature {
    fn eq(&self, other: &Self) -> bool {
        self.args
            .clone()
            .into_iter()
            .zip(other.args.clone())
            .all(|(Arg(_, t1), Arg(_, t2))| t1 == t2)
            && self.ret == other.ret
    }
}
