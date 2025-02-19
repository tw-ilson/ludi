use crate::err::{Error, ParseErrorKind, TypeErrorKind};
use crate::token::TokenData;
use crate::{
    ast::{Expr, FuncSignature},
    atomic::Literal,
    env::{Env, Name},
    err::{LudiError, Result},
    shape::Shape,
};
use std::{fmt::Display, rc::Rc, str::FromStr};

pub type TypeEnv = Env<Type>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ref<T> {
    name: Name,
    ty: Box<T>,
}

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
    ArrayRef(Ref<Array>),
    Arr(Arr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Atom {
    AtomRef(Ref<Atom>),
    Func(Rc<Func>), // Note: Maybe this is not a type in the formal sense?
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

// common patterns for atomic data type matching:

macro_rules! numeric {
    () => {
        UInt8
            | Int8
            | UInt16
            | Int16
            | UInt32
            | Int32
            | UInt64
            | Int64
            | BFloat16
            | Float16
            | Float32
            | Float64
            | Complex
    };
}
macro_rules! integer {
    () => {
        UInt8 | Int8 | UInt16 | Int16 | UInt32 | Int32 | UInt64 | Int64
    };
}
macro_rules! integer_signed {
    () => {
        Int8 | Int16 | Int32 | Int64
    };
}
macro_rules! integer_unsigned {
    () => {
        UInt8 | UInt16 | UInt32 | UInt64
    };
}
macro_rules! float {
    () => {
        BFloat16 | Float16 | Float32 | Float64
    };
}

// define primitives
#[derive(derive_more::Display, Clone, Eq, PartialEq, Debug)]
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
    fn add_dimension(self, dim: Shape) -> Self {
        match self {
            Self::Atom(atom) => {
                if dim.is_empty() {
                    Self::Atom(atom)
                } else {
                    Self::Array(atom.add_dimension(dim))
                }
            }
            Self::Array(array) => Self::Array(array.add_dimension(dim)),
        }
    }
}

impl Atom {
    fn add_dimension(self, dim: Shape) -> Array {
        Array::Arr(Arr {
            element: self,
            shape: dim,
        })
    }
}

impl Array {
    fn add_dimension(self, dim: Shape) -> Self {
        match self {
            Array::ArrayRef(_arrayref) => todo!(),
            Array::Arr(arr) => Array::Arr(arr.add_dimension(dim)),
        }
    }
}

impl Arr {
    fn add_dimension(mut self, dim: Shape) -> Self {
        self.shape = dim.concat(self.shape);
        self
    }
}

impl FromStr for AtomicDataType {
    type Err = anyhow::Error;
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
            "()" => Err(Error::type_err(
                TypeErrorKind::Unsupported,
                "Unit type not supported in function signature",
            )
            .into()),
            _ => Err(Error::type_err(
                crate::err::TypeErrorKind::Unknown,
                "not a known builtin type",
            )
            .into()),
        }
    }
}
impl TryFrom<&str> for AtomicDataType {
    type Error = anyhow::Error;
    fn try_from(value: &str) -> Result<Self> {
        FromStr::from_str(value)
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
    type Error = anyhow::Error;
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
    type Error = anyhow::Error;
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
            _ => Err(Error::parse_err(
                ParseErrorKind::FnCall,
                "tried to parse a primitive function",
            )
            .into()),
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

impl Atom {
    fn upgrade(self) -> Array {
        Array::Arr(Arr {
            element: self,
            shape: Shape::new(&[]),
        })
    }
}

pub trait GetType {
    fn get_type(&self) -> &Type;
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
                fn get_type(&self) -> &Type {
                    match self {
                        $(
                            Self::$variant {ty, ..} => ty,
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
    ast_typed! {
        TypedExpr {
              FnDef { body:TypedExpr } // function signature is captured by type
            | FnCall { callee: Callee, args: Vec<TypedExpr> }
            | Frame { expression_list: Vec<TypedExpr> }
            | AtomLiteral { value: Literal }
            | ArrayLiteral { value: Vec<Literal> }
            | Let { name:Name, initializer: TypedExpr, region: Option<TypedExpr> }
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
    use super::{
        typed_ast::{self, TypedExpr}, Arr, Array, Atom, AtomicDataType, Func, GetType, PrimitiveFuncType, Type,
        TypeEnv,
    };
    use crate::{
        ast::{self, Arg, FuncSignature},
        atomic::Literal,
        err::{Error, ParseErrorKind, Result, TypeErrorKind},
        shape::Shape,
    };
    use itertools::Itertools;
    use std::rc::Rc;

    pub trait TypeCheck<T> {
        fn type_check(self, table: &mut TypeEnv) -> Result<T>;
    }

    pub struct TypedTree {
        pub toplevel_expressions: Vec<typed_ast::TypedExpr>,
    }

    impl TypeCheck<TypedTree> for ast::ParseTree {
        fn type_check(self, table: &mut TypeEnv) -> Result<TypedTree> {
            Ok(TypedTree {
                toplevel_expressions: self
                    .toplevel_expressions
                    .into_iter()
                    .map(|expr| expr.type_check(table))
                    .collect::<Result<Vec<TypedExpr>>>()?,
            })
        }
    }

    impl TypeCheck<typed_ast::TypedExpr> for ast::Expr {
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
                                        element: atom.clone(),
                                        shape: Shape::new(&[frame_n]),
                                    }));
                                    Ok(typed_ast::frame_node(ty, checked_arr))
                                }
                                Type::Array(Array::Arr(arr)) => {
                                    // this is the case for n-d array literal
                                    let ty = Type::Array(Array::Arr(Arr {
                                        element: arr.element.clone(),
                                        shape: Shape::new(&[frame_n]).concat(arr.shape.clone()),
                                    }));
                                    Ok(typed_ast::frame_node(ty, checked_arr)) // is it necessary to retain the
                                                                               // inner information?
                                }
                                Type::Array(Array::ArrayRef(_array_ref)) => todo!(),
                            }
                        } else {
                            Err(Error::type_err(
                                TypeErrorKind::TypeMismatch,
                                &format!("Found non-conforming type in frame; expected {}", t1),
                            )
                            .into())
                        }
                    } else {
                        Err(Error::type_err(TypeErrorKind::Unsupported, "empty frame").into())
                    }
                }
                Self::Term(node) => {
                    let ty = table.get(&node.name)?;
                    Ok(typed_ast::term_node(ty.clone(), node.name))
                }
                Self::Let(node) => {
                    let initializer = node.initializer.type_check(table)?;
                    table.put(node.name.clone(), initializer.get_type().clone());
                    let region = match node.region {
                        Some(body) => body.type_check(table)?,
                        None => {
                            // return Err(Error::parse_err(
                            //     ParseErrorKind::LetExpr,
                            //     "dangling let body not allowed here",
                            // )
                            // .into());

                            // alternatively, return Unit
                            return Ok(typed_ast::let_node(
                                Type::Atom(Atom::Unit),
                                node.name,
                                initializer,
                                None,
                            ));
                        }
                    }; // No restrictions on region type
                    Ok(typed_ast::let_node(
                        region.get_type().clone(),
                        node.name,
                        initializer,
                        Some(region),
                    ))
                }
                Self::FnDef(node) => {
                    table.push();
                    for Arg(name, arg_ty) in &node.signature.args {
                        table.put(name.clone(), arg_ty.clone());
                    }
                    let body = node.body.type_check(table)?;
                    table.pop();

                    if body.get_type() != &node.signature.ret[0] {
                        return Err(Error::type_err(
                            TypeErrorKind::TypeMismatch,
                            "Function body does not match signature!",
                        )
                        .into());
                    }
                    let ty = Type::Atom(Atom::Func(Rc::new(node.signature.try_into()?)));
                    Ok(typed_ast::fn_def_node(ty, body))
                }
                Self::FnCall(node) => {
                    use AtomicDataType::*;

                    let args = node
                        .args
                        .into_iter()
                        .map(|arg_expr| arg_expr.type_check(table))
                        .collect::<Result<Vec<typed_ast::TypedExpr>>>()?;

                    match node.callee {
                        ast::Callee::Expression(call_expr) => {
                            let typed_call_expr = call_expr.type_check(table)?;
                            if let Type::Atom(Atom::Func(func_type)) = typed_call_expr.get_type() {
                                // TODO: check func type signature
                                // check if parameters <= arguments.
                                let frame = std::iter::zip(&func_type.parameters, &args)
                                    .fuse()
                                    .map(|(param_type, arg)| {
                                        let arg_ty = arg.get_type();
                                        arg_ty.view(&param_type).ok_or(Error::type_err(
                                            TypeErrorKind::ShapeMismatch,
                                            "Mismatched function argument shape!",
                                        ))
                                    })
                                    .process_results(|mut lift_dim_iter| {
                                        let shape = lift_dim_iter.next();
                                        // if all frame shapes match_then we can lift
                                        match shape {
                                            Some(shape) => if lift_dim_iter.all(|next_shape| next_shape == shape) { Ok(shape) } else { Err(Error::type_err(TypeErrorKind::ShapeMismatch, "Non-uniform lift dimension in function call!"))},
                                            None => Ok(Shape::empty())
                                        }

                                    })??;
                                let ty = func_type.return_type.clone().add_dimension(frame);
                                let callee = typed_ast::Callee::Expression(typed_call_expr);
                                Ok(typed_ast::fn_call_node(ty, callee, args))
                            } else {
                                Err(anyhow::anyhow!("type error - not a function!"))
                            }
                        }
                        ast::Callee::Primitive(prim) => match prim {
                            PrimitiveFuncType::Add
                            | PrimitiveFuncType::Sub
                            | PrimitiveFuncType::Mul
                            | PrimitiveFuncType::Div
                            | PrimitiveFuncType::Mod => {
                                if args.len() != 2 {
                                    return Err(Error::type_err(
                                        TypeErrorKind::Unsupported,
                                        "arithmetic operation must have 2 arguments!",
                                    )
                                    .into());
                                }
                                let mut arg_iter = args.into_iter();
                                let (argl, argr) =
                                    (arg_iter.next().unwrap(), arg_iter.next().unwrap());

                                let (ty_l, ty_r) = (argl.get_type(), argr.get_type());

                                // TODO: figure out auto-mapping semantics for primitives

                                let frame_l = ty_l.view(ty_r);
                                let frame_r = ty_r.view(ty_l);

                                if !matches!(
                                    ty_l,
                                    Type::Atom(Atom::Literal(numeric!()))
                                        | Type::Array(Array::Arr(Arr {
                                            element: Atom::Literal(numeric!()),
                                            ..
                                        }))
                                ) {
                                    return Err(Error::type_err(
                                        TypeErrorKind::Unsupported,
                                        "only numeric values are supported for arithmetic operations"
                                    )
                                    .into());
                                }
                                if frame_l.is_none() && frame_r.is_none() {
                                    return Err(Error::type_err(
                                        TypeErrorKind::ShapeMismatch,
                                        &format!("incompatible shapes for operation"),
                                    )
                                    .into());

                                // This is a reordering operation
                                } else if frame_l.is_some() {
                                    Ok(typed_ast::fn_call_node(
                                        ty_l.clone(),
                                        typed_ast::Callee::Primitive(prim),
                                        vec![argl, argr],
                                    ))
                                } else if frame_r.is_some() {
                                    Ok(typed_ast::fn_call_node(
                                        ty_r.clone(),
                                        typed_ast::Callee::Primitive(prim),
                                        vec![argr, argl],
                                    ))
                                } else {
                                    unreachable!()
                                }
                            }
                            PrimitiveFuncType::Neg | PrimitiveFuncType::Inv => {
                                if args.len() != 1 {
                                    return Err(Error::type_err(
                                        TypeErrorKind::Unsupported,
                                        "Arithmetic operation only supports one argument",
                                    )
                                    .into());
                                }
                                let arg1 = args.into_iter().next().unwrap();
                                let ty = arg1.get_type();
                                if !matches!(
                                    ty,
                                    Type::Atom(Atom::Literal(integer_signed!() | float!()))
                                        | Type::Array(Array::Arr(Arr {
                                            element: Atom::Literal(integer_signed!() | float!()),
                                            ..
                                        }))
                                ) {
                                    return Err(Error::type_err(
                                        TypeErrorKind::Unsupported,
                                        "Type Unsupported",
                                    )
                                    .into());
                                }
                                return Ok(typed_ast::fn_call_node(
                                    ty.clone(),
                                    typed_ast::Callee::Primitive(prim),
                                    vec![arg1],
                                ));
                            }
                            PrimitiveFuncType::Eq | PrimitiveFuncType::Ne => {
                                if args.len() != 2 {
                                    return Err(Error::type_err(
                                        TypeErrorKind::Unsupported,
                                        "arithmetic operation must have 2 arguments!",
                                    )
                                    .into());
                                }
                                let mut arg_iter = args.into_iter();
                                let (argl, argr) =
                                    (arg_iter.next().unwrap(), arg_iter.next().unwrap());

                                let (ty_l, ty_r) = (argl.get_type(), argr.get_type());
                                todo!()
                            }
                            PrimitiveFuncType::Gt
                            | PrimitiveFuncType::GtEq
                            | PrimitiveFuncType::Lt
                            | PrimitiveFuncType::LtEq => {
                                todo!()
                            }
                            PrimitiveFuncType::Or => todo!(),
                            PrimitiveFuncType::And => todo!(),
                            PrimitiveFuncType::Not => todo!(),
                            PrimitiveFuncType::If => todo!(),
                            PrimitiveFuncType::Reshape => todo!(),
                            PrimitiveFuncType::Reduce => todo!(),
                            PrimitiveFuncType::Scan => todo!(),
                            PrimitiveFuncType::Fold => todo!(),
                            PrimitiveFuncType::Trace => todo!(),
                            PrimitiveFuncType::Reverse => todo!(),
                            PrimitiveFuncType::Filter => todo!(),
                            PrimitiveFuncType::Append => todo!(),
                            PrimitiveFuncType::Rotate => todo!(),
                            PrimitiveFuncType::Iota => todo!(),
                            PrimitiveFuncType::Slice => todo!(),
                            PrimitiveFuncType::Scatter => todo!(),
                            PrimitiveFuncType::IntToFloat => todo!(),
                            PrimitiveFuncType::IntToBool => todo!(),
                            PrimitiveFuncType::BoolToInt => todo!(),
                            PrimitiveFuncType::FloatToInt => todo!(),
                        },
                    }
                }
            }
        }
    }

    use crate::shape::Frame;
    impl Frame for super::Array {
        fn view(&self, other: &Self) -> Option<Shape> {
            let arr_a: &Arr = match self {
                Array::ArrayRef(_name) => todo!(),
                Array::Arr(arr) => arr,
            };
            let arr_b: &Arr = match other {
                Array::ArrayRef(_name) => todo!(),
                Array::Arr(arr) => arr,
            };
            arr_a
                .shape
                .view(&arr_b.shape)
                .filter(|_| arr_a.element == arr_b.element)
        }
    }
    impl Frame for Type {
        fn view(&self, other: &Self) -> Option<Shape> {
            match (self, other) {
                (Type::Array(a), Type::Array(b)) => {
                    let arr_a: &Arr = match a {
                        Array::ArrayRef(_name) => todo!(),
                        Array::Arr(arr) => arr,
                    };
                    let arr_b: &Arr = match b {
                        Array::ArrayRef(_name) => todo!(),
                        Array::Arr(arr) => arr,
                    };
                    arr_a
                        .shape
                        .view(&arr_b.shape)
                        .filter(|_| arr_a.element == arr_b.element)
                }
                (Type::Atom(a), Type::Atom(b)) => (a == b).then_some(Shape::new(&[])),
                (Type::Atom(atom), array @ Type::Array(..)) => {
                    Frame::view(&Type::Array(atom.clone().upgrade()), array)
                }
                (array @ Type::Array(..), Type::Atom(atom)) => {
                    Frame::view(array, &Type::Array(atom.clone().upgrade()))
                }
            }
        }
    }
    // impl std::cmp::PartialOrd for Array {
    //     fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    //         todo!()
    //     }
    // }
    impl TryInto<Func> for FuncSignature {
        type Error = anyhow::Error;
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
