use std::fmt::Display;
use std::fs::write;
use std::ops::Deref;

use crate::atomic::Literal;
use crate::shape::{Shape, ShapeOps, ArrayProps};
use crate::types::{Type, PrimitiveFuncType};
use crate::env::Name;
use crate::err::{Error, ErrorKind, LudiError, Result};
use crate::token::TokenData;
use derive_more::Display;
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
        #[derive(Debug, PartialEq)]
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
        #[derive(Debug, PartialEq)]
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

pub use define_constructors;
pub use define_enum;
pub use ast;

// AST productions for the lang of format:
// Symbol {
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
          FnDef { signature: CallSignature, body:Expr }
        | FnCall { callee: Callee, args: Vec<Expr> }
        | Frame { expression_list: Vec<Expr> }
        | AtomLiteral { value: Literal }
        | ArrayLiteral { value: Vec<Literal> }
        | Let { name:Name, initializer: Expr, region: Option<Expr> }
        | Term { name: Name }
        // | AtomicCast {value: Expr }
        // | ShapeCast {value: Expr }
    }
}

#[derive(Debug, PartialEq)]
pub enum Callee  {
    Expression(Expr),
    Primitive(PrimitiveFuncType),
}

// pub fn binary_operation(op: &'static str, left: Expr, right: Expr) -> Expr {
//     let args = vec![left, right];
//     match op {
//     }
// }


#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TypeSignature(pub Type, pub Shape);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct OptionalTypeSignature(pub Option<Type>, pub Shape);

#[derive(Debug, Eq, Clone)]
pub struct CallSignature {
    pub args: Vec<(Name, OptionalTypeSignature)>,
    pub ret: OptionalTypeSignature,
}

impl Display for CallSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (_, t_arg) in &self.args {
            write!(f, "{}, ", t_arg)?
        }
        write!(f, "-> {}", self.ret)?;
        Ok(())
    }
}

impl PartialEq for CallSignature {
    fn eq(&self, other: &Self) -> bool {
        self.args
            .clone()
            .into_iter()
            .zip(other.args.clone())
            .all(|((_, t1), (_, t2))| t1 == t2)
            && self.ret == other.ret
    }
}


impl TryFrom<OptionalTypeSignature> for TypeSignature {
    type Error = Error;
    fn try_from(value: OptionalTypeSignature) -> Result<Self> {
        Ok(TypeSignature(
            value
                .0
                .ok_or(Error::compile_err("expected explicit type annotations"))?,
            value.1,
        ))
    }
}


impl std::fmt::Display for TypeSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}{}]",
            self.0,
            self.1
                .shape_slice()
                .iter()
                .fold("".to_string(), |d, acc| format!("{} {}", acc, d))
        )
    }
}

impl std::fmt::Display for OptionalTypeSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}{}]",
            if let Some(d) = &self.0 {
                d.to_string()
            } else {
                "".to_string()
            },
            self.1
                .shape_slice()
                .iter()
                .fold("".to_string(), |d, acc| format!("{} {}", acc, d))
        )
    }
}

