use crate::tokens::TokenData;
use crate::env::Name;
use crate::atomic::AtomicType;
use crate::array::ArrayType;
use crate::array::ShapeVec;

pub type AstProgram = Vec<Stmt>;
type NodeRef<T> = Box<T>; // might need this as Arc if we want multitreading parsing
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
pub enum BinaryOpType {
    ADD, SUB, MUL, DIV
}
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
          ExprStmt ExprStmtNode {expression: Expr}
        | PrintStmt PrintStmtNode {expression: Expr}
        | AssignStmt AssignStmtNode {name:Name, initializer: Expr}
    }
    Expr {
          UnaryOperation UnaryNode {operator: UnaryOpType, right: Expr}
        | BinaryOperation BinaryNode {left: Expr, operator: BinaryOpType, right: Expr}
        | FnDef FnDefNode {args:Vec<(Name, ShapeVec)>, ret:Option<ShapeVec>, body:Expr}
        | FnCall FnCallNode {callee: Expr, args: Vec<Expr>}
        | Grouping GroupingNode {expression: Expr}
        | Frame FrameNode { expression_list: Vec<Expr> }
        | Array ArrayNode { value: ArrayType }
        | Literal LiteralNode {value: AtomicType}
        | Assignment AssignmentNode {name: Name}
        | AtomicCast AtomicCastNode {value: AtomicType}
        | ShapeCast ShapeCastNode {value:ArrayType}
    }
}
