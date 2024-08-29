use crate::tokens::TokenData;
use crate::env::Name;
use crate::data::DataType;
use crate::atomic::AtomicType;
use crate::array::ShapeVec;
use derive_more::Display;

pub type ParseTree = Vec<Expr>;
pub type NodeRef<T> = Box<T>; 

#[macro_export]
macro_rules! define_ast {
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
                pub fn [< $variant:snake:lower _node>]($($childname : $childtype),*) -> $base_name {
                    $base_name::$variant(
                        [<$variant Node>] {
                            $($childname,)*
                        }.into()
                    )
                }
            }
        )+
    };
}
pub(crate) use define_ast;
pub(crate) use define_enum;
pub(crate) use define_nodes;
pub(crate) use define_constructors;

#[derive(Debug, PartialEq)]
struct AnnotatedNode<Props, Node>{
    pub props: Props,
    pub node: Node
}

struct AnnotatedAST<A> {
    props: A,
    ast: ParseTree
}

macro_rules! annotated_ast {
    ($(
        $base_name:ident {
         $($variant:ident $variant_data:ident {
             $($childname:ident: $childtype:ty),*
         })|+
    })*
    ) => {
    $(
        annotated_nodes!($base_name {
         $($variant $variant_data {
             $($childname: $childtype),*
         }),+});
     )*
    };
}

macro_rules! annotated_nodes {
    ($base_name:ident {
        $($variant:ident $variant_data:ident {
             $($childname:ident: $childtype:ty),*
        }),+
    }) => {
        $(
            paste! {
                pub type [<Annotated $variant_data>]<Props> = AnnotatedNode<Props, $variant_data>;
            }
        )+
    }
}

// define primitives
#[derive(Copy, Clone, Eq, PartialEq, Debug, Display)]
pub enum BinaryOpType {
    ADD, SUB, MUL, DIV
}
#[derive(Copy, Clone, Eq, PartialEq, Debug, Display)]
pub enum UnaryOpType {
    NEG, INV
}

#[derive(Debug, Eq, Clone)]
pub struct CallSignature {
    pub args: Vec<(Name, ShapeVec)>,
    pub ret: Option<ShapeVec>,
}
impl PartialEq for CallSignature {
    fn eq(&self, other: &Self) -> bool {
        self.args.clone().into_iter().zip(other.args.clone()).all(|((_, s1), (_, s2))| s1 == s2)
            && self.ret == other.ret
    }
}

// AST productions for the lang of format:
// Symbol { 
//      production { attributes... } 
//      ...
//      } 
define_ast! {
    Stmt {
          ExprStmt { expression: Expr }
        | PrintStmt { expression: Expr }
    }
    Expr {
          UnaryOperation { operator: UnaryOpType, right: Expr }
        | BinaryOperation { left: Expr, operator: BinaryOpType, right: Expr }
        | FnDef { signature: CallSignature, body:Expr }
        | FnCall { callee: Expr, args: Vec<Expr> }
        | Grouping { expression: Expr }
        | Frame { expression_list: Vec<Expr> }
        | Literal {value: DataType }
        | Let { name:Name, initializer: Expr, region: Option<Expr> }
        | Term {name: Name }
        | AtomicCast {value: DataType }
        | ShapeCast {value: DataType }
    }
}


