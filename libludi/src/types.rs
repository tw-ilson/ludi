use anyhow::Error;

use crate::token::TokenData;
use crate::{
    ast::{Expr, FuncSignature},
    atomic::Literal,
    env::{Env, Name},
    err::{ErrorKind, LudiError, Result},
    shape::Shape,
};
use std::{fmt::Display, rc::Rc, str::FromStr};

pub type TypeEnv = Env<Type>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sort {
    Dim,
    Shape,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    Atom,
    Array,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func {
    pub parameters: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Abstraction<T> {
    pub parameters: Vec<T>,
    pub body: Array,
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
    Tuple(Vec<Type>),
    Unit, // bottom type
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Atom(Atom),
    Array(Array),
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
            "()" => Err(Error::parse_err(
                "Unit type not supported in function signature",
            )),
            _ => Err(Error::parse_err("not a known builtin type")),
        }
    }
}

impl Into<Array> for Atom {
    fn into(self) -> Array {
        Array::Arr(Arr {
            shape: Shape::empty(),
            element: self,
        })
    }
}

impl TryFrom<&str> for Atom {
    type Error = Error;
    fn try_from(value: &str) -> Result<Self> {
        use std::str::FromStr;
        if let Ok(atomic_type) = AtomicDataType::from_str(value) {
            return Ok(Atom::Literal(atomic_type));
        } else {
            // TODO: support other kinds of type annotations
            todo!()
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

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Atom(atom) => match atom {
                // Atom::AtomRef(atomref) => write!(f, "{}", atomref),
                Atom::Func(_func) => todo!(),
                Atom::Literal(literal) => literal.fmt(f),
                _ => todo!(),
            },
            Self::Array(array) => match array {
                Array::ArrayRef(_arrayref) => todo!(),
                Array::Arr(_arr) => todo!(),
            },
        }
    }
}

pub trait GetType {
    fn get_type(&self) -> Type;
}

pub mod typed_ast {
    use super::*;

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
            impl GetType for $base_name {
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
                    $variant { node: NodeRef<[<$variant Node >]>, ty: Type}
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
            pub struct [<$variant Node>] {$(pub $childname: $childtype),*}
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
                    pub fn [<$variant:snake:lower _node >](ty:Type, $($childname : $childtype),*) -> $base_name {
                        $base_name::$variant {
                            node: [<$variant Node >] {
                                $($childname,)*
                            }.into(),
                            ty
                        }
                    }
                }
            )+
        };
    }

    // Creates an AST where each node reference is attached to a data structure holding its type
    // information
    // Note: this is no longer serving the interpreter so let bodies must be present
    ast_typed! {
        TypedExpr {
              FnDef { body:TypedExpr }
            | FnCall { callee: Callee, args: Vec<TypedExpr> }
            | Frame { expression_list: Vec<TypedExpr> }
            | AtomLiteral { value: Literal }
            | ArrayLiteral { value: Vec<Literal> }
            | Let { name:Name, initializer: TypedExpr, region: TypedExpr }
            | Term { name: Name }
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum Callee {
        Expression(TypedExpr),
        Primitive(PrimitiveFuncType),
    }
}
pub mod typecheck {
    use crate::ast;

    use super::*;
    use itertools::Itertools;
    use typed_ast::*;

    pub trait TypeCheck<T> {
        fn type_check(self, table: &mut TypeEnv) -> Result<T>;
    }

    impl TypeCheck<typed_ast::TypedExpr> for Expr {
        fn type_check(self, table: &mut TypeEnv) -> Result<typed_ast::TypedExpr> {
            match self {
                Self::AtomLiteral(node) => {
                    let ty = Type::Atom(Atom::Literal(match &node.value {
                        Literal::Int { .. } => AtomicDataType::Int64,
                        Literal::Float { .. } => AtomicDataType::Float64,
                        Literal::Char { .. } => AtomicDataType::Character,
                        Literal::Bool { .. } => AtomicDataType::Boolean,
                    }));
                    Ok(typed_ast::atom_literal_node(ty, node.value))
                }
                Self::ArrayLiteral(_) => todo!(),
                Self::Frame(node) => {
                    let frame_n = node.expression_list.len();
                    let checked_arr = node
                        .expression_list
                        .into_iter()
                        .map(|expr| expr.type_check(table))
                        .collect::<Result<Vec<typed_ast::TypedExpr>>>()?;

                    if let Some(expr1) = checked_arr.get(1) {
                        let t1 = expr1.get_type();
                        if checked_arr.iter().all(|t_expr| t_expr.get_type() == t1) {
                            match t1 {
                                Type::Atom(atom) => {
                                    let ty = Type::Array(Array::Arr(Arr {
                                        element: atom,
                                        shape: Shape::new(&[frame_n]),
                                    }));
                                    Ok(typed_ast::frame_node(ty, checked_arr))
                                }
                                Type::Array(Array::Arr(arr)) => {
                                    // this is the case for n-d array literal
                                    let ty = Type::Array(Array::Arr(Arr {
                                        element: arr.element,
                                        shape: Shape::new(&[frame_n]).concat(arr.shape),
                                    }));
                                    Ok(typed_ast::frame_node(ty, checked_arr)) // is it necessary to retain the
                                                                               // inner information?
                                }
                                Type::Array(Array::ArrayRef(_array_ref)) => todo!(),
                            }
                        } else {
                            // return type_err!()
                            panic!()
                        }
                    } else {
                        // return type_err!()
                        panic!()
                    }
                }
                Self::Term(node) => {
                    let ty = table.get(&node.name)?;
                    Ok(typed_ast::term_node(ty, node.name))
                }
                Self::Let(node) => {
                    let initializer = node.initializer.type_check(table)?;
                    table.put(node.name.clone(), initializer.get_type());
                    let region = match node.region {
                        Some(body) => body.type_check(table)?,
                        None => {
                            return Err(Error::compile_err("dangling let body not allowed here"))
                        }
                    }; // No restrictions on region type
                    Ok(typed_ast::let_node(
                        region.get_type(),
                        node.name,
                        initializer,
                        region,
                    ))
                }
                Self::FnDef(node) => {
                    // TODO: IMPORTANT: Some kind of pi or sigma expression?
                    let body = node.body.type_check(table)?;
                    if body.get_type() != node.signature.ret[0] {
                        return Err(anyhow::anyhow!(
                            "type error - expected function to return X"
                        ));
                    }
                    let ty = Type::Atom(Atom::Func(Rc::new(node.signature.try_into()?)));
                    Ok(typed_ast::fn_def_node(ty, body))
                }
                Self::FnCall(node) => {
                    // look up type of callee
                    match node.callee {
                        ast::Callee::Expression(call_expr) => {
                            let typed_call_expr = call_expr.type_check(table)?;
                            if let Type::Atom(Atom::Func(func_type)) = typed_call_expr.get_type() {
                                let ty = func_type.return_type.clone();
                                let callee = typed_ast::Callee::Expression(typed_call_expr);
                                let args = node
                                    .args
                                    .into_iter()
                                    .map(|arg_expr| arg_expr.type_check(table))
                                    .collect::<Result<Vec<TypedExpr>>>()?;
                                Ok(typed_ast::fn_call_node(ty, callee, args))
                            } else {
                                Err(anyhow::anyhow!("type error - not a function!"))
                            }
                        }
                        ast::Callee::Primitive(prim) => todo!(),
                    }
                }
            }
        }
    }

    // impl TypeCheck<Type> for PrimitiveFuncType {
    //     fn type_check(self, _table: &mut TypeEnv) -> Result<Type> {
    //         let ty = Type::Atom(Atom::Func(match self {
    //             Self::Add => Func {
    //                 parameters:
    //             }.into(),
    //             // Self::Sub => (),
    //             // Self::Mul => (),
    //             // Self::Div => (),
    //             _ => todo!()
    //         }));
    //         Ok(ty)
    //     }
    // }

    impl TryInto<Func> for FuncSignature {
        type Error = Error;
        fn try_into(self) -> Result<Func> {
            let parameters: Vec<Type> = self
                .args
                .into_iter()
                .map(|ast::Arg(_, ty)| ty)
                .collect_vec();
            let return_type = match self.ret.len() {
                0 => Type::Atom(Atom::Unit),
                1 => self.ret.into_iter().next().unwrap(),
                _ => Type::Atom(Atom::Tuple(self.ret)),
            };
            Ok(Func {
                parameters,
                return_type,
            })
        }
    }
}
