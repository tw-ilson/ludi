use crate::tokens::TokenData;
use crate::env::Name;
use crate::data::DataType;
use crate::atomic::AtomicType;
// use crate::array::ArrayType;
use crate::array::ShapeVec;
use crate::r#fn::CallSignature;

pub type AstProgram = Vec<Stmt>;
type NodeRef<T> = Box<T>; 
macro_rules! define_ast {
    ($(
        $base_name:ident {
         $($variant:ident $variant_data:ident {
             $($childname:ident: $childtype:ty),*
         })|+
    })*
    ) => {
        $(
        define_enum!($base_name {
         $($variant $variant_data {
             $($childname: $childtype),*
         }),+});
        define_nodes!($base_name {
         $($variant $variant_data {
             $($childname: $childtype),*
         }),+});
        )*
    }
}
macro_rules! define_enum {
    ($base_name:ident {
         $($variant:ident $variant_data:ident {
             $($childname:ident: $childtype:ty),*
         }),+
    }
    ) => {
        #[derive(Debug, PartialEq)]
        pub enum $base_name {
            $(
                $variant(NodeRef<$variant_data>)
            ),+
        }
    }
}
macro_rules! define_nodes {
    ($base_name:ident {
         $($variant:ident $variant_data:ident {
             $($childname:ident: $childtype:ty),*
         }),+
    }
    ) => {
        $(
        #[derive(Debug, PartialEq)]
        pub struct $variant_data {$(pub $childname: $childtype),*}
        )+
    };
}

// define primitives
#[derive(Copy, Clone, Eq, PartialEq, Debug, derive_more::Display)]
pub enum BinaryOpType {
    ADD, SUB, MUL, DIV
}
#[derive(Copy, Clone, Eq, PartialEq, Debug, derive_more::Display)]
pub enum UnaryOpType {
    NEG, INV
}


// AST productions for the lang of format:
// Grammar { 
//      type data { attributes... } 
//      ...
//      } 
define_ast! {
    Stmt {
          ExprStmt ExprStmtNode { expression: Expr }
        | PrintStmt PrintStmtNode { expression: Expr }
        | AssignStmt AssignStmtNode { name:Name, initializer: Expr }
    }
    Expr {
          UnaryOperation UnaryNode { operator: UnaryOpType, right: Expr }
        | BinaryOperation BinaryNode { left: Expr, operator: BinaryOpType, right: Expr }
        | FnDef FnDefNode { signature: CallSignature, body:Expr }
        | FnCall FnCallNode { callee: Expr, args: Vec<Expr> }
        | Grouping GroupingNode { expression: Expr }
        | Frame FrameNode { expression_list: Vec<Expr> }
        // | Array ArrayNode { value: ArrayType }
        | Literal LiteralNode {value: DataType }
        | Assignment AssignmentNode {name: Name }
        | AtomicCast AtomicCastNode {value: DataType }
        | ShapeCast ShapeCastNode {value: DataType }
    }
}
