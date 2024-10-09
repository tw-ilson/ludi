use anyhow::Error;

use crate::token::TokenData;
use crate::{
    atomic::Literal,
    env::{Env, Name},
    err::{ErrorKind, LudiError, Result},
    shape::Shape,
};
use std::{fmt::Display, rc::Rc, str::FromStr};

pub type TypeEnv = Env<Type>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Sort {
    Dim,
    Shape,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    Atom,
    Array,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func {
    parameters: Vec<Array>,
    return_type: Array,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Abstraction<T> {
    parameters: Vec<T>,
    body: Array,
}

pub type Forall = Abstraction<Kind>;
pub type Pi = Abstraction<Sort>;
pub type Sigma = Abstraction<Sort>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Arr {
    pub element: Atom,
    pub shape: Shape,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Array {
    ArrayRef(Name),
    Arr(Arr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Atom {
    AtomRef(Name),
    Func(Rc<Func>),
    Forall(Rc<Forall>),
    Pi(Rc<Pi>),
    Sigma(Rc<Sigma>),
    Literal(AtomicDataType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Atom(Atom),
    Array(Array),
    // Unit, // the type for expressions that evaluate to a meaningless non-value
    // Never, // the type-error type
}

#[repr(u8)]
#[derive(derive_more::Display, Eq, Debug, Copy, Clone, PartialEq, Hash)]
pub enum AtomicDataType {
    //Numeric
    UInt8,
    Int8,
    UInt16,
    Int16,
    UInt32,
    Int32,
    UInt64,
    Int64,
    BFloat16,
    Float16,
    Float32,
    Float64,
    Complex,

    //Non-Numeric
    Character,
    Boolean,
}

// define primitives
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum PrimitiveFuncType {
    // arithmetic
    Add,
    Sub,
    Mul,
    Div,
    IntToFloat,
    FloatToInt,
    IntToBool,
    BoolToInt,

    //logical
    If,
    Or,
    And,
    Not,
    Eq,
    Ne,
    Gt,
    GtEq,
    Lt,
    LtEq,
    Neg,
    Inv,
    Mod,

    // array
    Reshape,
    Reduce,
    Scan,
    Fold,
    Trace,
    Reverse,
    Filter,
    Append,
    Rotate,
    Iota,
    Slice,
    Scatter,
}

impl Type {
    fn kind(&self) -> Kind {
        match self {
            Self::Atom(_) => Kind::Atom,
            Self::Array(_) => Kind::Array,
        }
    }
}

impl FromStr for AtomicDataType {
    type Err = crate::err::Error;
    fn from_str(s: &str) -> Result<Self> {
        match s {
            "u8" => Ok(AtomicDataType::UInt8),
            "i8" => Ok(AtomicDataType::Int8),
            "u16" => Ok(AtomicDataType::UInt16),
            "i16" => Ok(AtomicDataType::Int16),
            "u32" => Ok(AtomicDataType::UInt32),
            "i32" => Ok(AtomicDataType::Int32),
            "u64" => Ok(AtomicDataType::UInt64),
            "i64" => Ok(AtomicDataType::Int64),
            "bf16" => Ok(AtomicDataType::BFloat16),
            "f16" => Ok(AtomicDataType::Float16),
            "f32" => Ok(AtomicDataType::Float32),
            "f64" => Ok(AtomicDataType::Float64),
            "complex" => Ok(AtomicDataType::Complex),
            "char" => Ok(AtomicDataType::Character),
            "bool" => Ok(AtomicDataType::Boolean),
            "()" => Err(Error::parse_err("Unit type not supported in function signature")),
            _ => Err(Error::parse_err("not a known builtin type")),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Atom(atom) => match atom {
                Atom::AtomRef(atomref) => write!(f, "{}", atomref),
                Atom::Func(func) => todo!(),
                Atom::Literal(literal) => todo!(),
                _ => todo!(),
            },
            Self::Array(array) => match array {
                Array::ArrayRef(arrayref) => todo!(),
                Array::Arr(arr) => todo!(),
            },
        }
    }
}

impl TryFrom<&str> for PrimitiveFuncType {
    type Error = crate::err::Error;
    fn try_from(value: &str) -> crate::err::Result<Self> {
        use crate::token::Token;
        match value {
            "reshape" => Ok(Self::Reshape),
            "reduce" => Ok(Self::Reduce),
            "scan" => Ok(Self::Scan),
            "fold" => Ok(Self::Fold),
            "trace" => Ok(Self::Trace),
            "reverse" => Ok(Self::Reverse),
            "filter" => Ok(Self::Filter),
            "append" => Ok(Self::Append),
            "rotate" => Ok(Self::Rotate),
            "iota" => Ok(Self::Iota),
            "slice" => Ok(Self::Slice),
            "scatter" => Ok(Self::Scatter),
            _ => Err(Error::parse_err("tried to parse a primitive function")),
        }
    }
}

pub trait TypedTree {
    fn get_type(&self) -> Type;
}
pub type NodeRef<T> = Box<T>;
macro_rules! ast_typed {
    ($(
        $base_name:ident {
         $($variant:ident {
             $($childname:ident: $childtype:ty),*
         })|+
    })*
    ) => {
        $(
        // Enums define the parse tree from grammar
        define_enum_typed!($base_name {
         $($variant {
             $($childname: $childtype),*
         }),+});

        // data structures held by nodes
        define_nodes_typed!($base_name {
         $($variant {
             $($childname: $childtype),*
         }),+});
        // initialize nodes with shorthand function
        define_constructors_typed!($base_name {
         $($variant {
             $($childname: $childtype),*
         }),+});
        impl TypedTree for $base_name {
            fn get_type(&self) -> Type {
                match self {
                    $(
                        Self::$variant {ty, ..} => ty.clone(),
                    )+
                }
            }
        }
        )*
    }
}
macro_rules! define_enum_typed {
    ($base_name:ident {
         $($variant:ident {
             $($childname:ident: $childtype:ty),*
         }),+
    }
    ) => {
        paste::paste!{
        #[derive(Debug, PartialEq)]
        pub enum $base_name {
            $(
                $variant { data: NodeRef<[< Typed $variant Node >]>, ty: Type}
            ),+
        }
        }
    }
}
macro_rules! define_nodes_typed {
    ($base_name:ident {
         $($variant:ident {
             $($childname:ident: $childtype:ty),*
         }),+
    }
    ) => {
        $(
        paste::paste!{
        #[derive(Debug, PartialEq)]
        pub struct [<Typed $variant Node>] {$(pub $childname: $childtype),*}
        }
        )+
    };
}
macro_rules! define_constructors_typed {
    ($base_name:ident {
         $($variant:ident {
             $($childname:ident: $childtype:ty),*
         }),+
    }
    ) => {
        $(
            paste::paste!{
                // Gives a shorthand syntax for constructing node
                pub fn [< typed_ $variant:snake:lower _node >](ty:Type, $($childname : $childtype),*) -> $base_name {
                    $base_name::$variant {
                        data: [< Typed $variant Node >] {
                            $($childname,)*
                        }.into(),
                        ty
                    }
                }
            }
        )+
    };
}

use crate::ast::Callee;
use crate::ast::{CallSignature, Expr};

// Creates an AST where each node reference is attached to a data structure holding its type
// information
// Note: this is no longer serving the interpreter so let bodies must be present
ast_typed! {
    TypedExpr {
          FnDef { signature: CallSignature, body:TypedExpr }
        | FnCall { callee: Callee, args: Vec<TypedExpr> }
        | Frame { expression_list: Vec<TypedExpr> }
        | AtomLiteral { value: Literal }
        | ArrayLiteral { value: Vec<Literal> }
        | Let { name:Name, initializer: TypedExpr, region: TypedExpr }
        | Term { name: Name }
    }
}

pub trait TypeCheck<T: TypedTree> {
    fn type_check(self, table: &mut TypeEnv) -> Result<T>;
}

impl TypeCheck<TypedExpr> for Expr {
    fn type_check(self, table: &mut TypeEnv) -> Result<TypedExpr> {
        match self {
            Self::AtomLiteral(node) => {
                let ty = Type::Atom(Atom::Literal(match &node.value {
                    Literal::Int { .. } => AtomicDataType::Int64,
                    Literal::Float { .. } => AtomicDataType::Float64,
                    Literal::Char { .. } => AtomicDataType::Character,
                    Literal::Bool { .. } => AtomicDataType::Boolean,
                }));
                Ok(typed_atom_literal_node(ty, node.value))
            }
            Self::Frame(node) => {
                let frame_n = node.expression_list.len();
                let checked_arr = node
                    .expression_list
                    .into_iter()
                    .map(|expr| expr.type_check(table))
                    .collect::<Result<Vec<TypedExpr>>>()?;

                let t1 = checked_arr.get(1).unwrap().get_type();
                //                          ^ need to check for empty frame

                if checked_arr.iter().all(|t_expr| t_expr.get_type() == t1) {
                    match t1 {
                        Type::Atom(atom) => {
                            let ty = Type::Array(Array::Arr(Arr {
                                element: atom,
                                shape: Shape::new(&[frame_n]),
                            }));
                            Ok(typed_frame_node(ty, checked_arr))
                        },
                        Type::Array(Array::Arr(arr)) => { // this is the case for n-d array literal
                            let ty = Type::Array(Array::Arr(Arr {
                                element: arr.element,
                                shape: Shape::new(&[frame_n]).concat(arr.shape)
                            }));
                            Ok(typed_frame_node(ty, checked_arr)) // is it necessary to retain the
                                                                  // inner information?
                        },
                        Type::Array(Array::ArrayRef(array_ref)) => todo!(),
                    }
                } else {
                    // return type_err!()
                    todo!()
                }
            }
            Self::Term(node) => {
                let ty = table.get(&node.name)?;
                // let ty = match ty {
                //     Type::Atom(atom) => (),
                //     Type::Array(array) => (),
                // };
                Ok(typed_term_node(ty, node.name))
            }
            Self::Let(node) => {
                let initializer = node.initializer.type_check(table)?;
                table.put(node.name.clone(), initializer.get_type());
                let region = match node.region {
                    Some(body) => body.type_check(table)?,
                    None => return Err(Error::compile_err("dangling let body not allowed here")),
                };
                Ok(typed_let_node(
                    region.get_type(),
                    node.name,
                    initializer,
                    region,
                ))
            }
            Self::FnDef(node) => {
                // Some kind of pi or sigma expression?
                todo!()
            }
            Self::FnCall(node) => {
                // look up type of callee
                // and provide index to type w/ function arguments
                todo!()
            }
            _ => todo!()
        }
    }
}
