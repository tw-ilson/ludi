use crate::tokens::TokenData;
use crate::atomic::NumberType;
use crate::array::ArrayType;

pub type Program = Vec<Stmt>;
type NodeRef<T> = Box<T>; // might need this as Arc if we want multitreading
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
// AST productions for the lang of format:
// Grammar { 
//      type data { attributes... } 
//      ...
//      } 
define_ast! {
    Stmt {
          ExprStmt ExprStmtNode {expression: Expr}
        | BlockStmt BlockStmtNode {statements: Vec<Stmt>}
        | PrintStmt PrintStmtNode {expression: Expr}
        | AssignStmt AssignStmtNode {name:TokenData, initializer: Expr}
        | FnStmt FnStmtNode {name:TokenData, args:Vec<TokenData>, body:Stmt}
    }
    Expr {
          Binary BinaryNode {left: Expr, operator: TokenData, right: Expr}
        | FnCall FnCallNode {callee: Expr, args: Vec<Expr>}
        | Conditional ConditionalNode {cond: Expr, body:Stmt, elsebody: Option<Stmt>}
        | Logical LogicalNode {left: Expr, operator: TokenData, right: Expr}
        | Grouping GroupingNode {expression: Expr}
        | Sequence SequenceNode {value: ArrayType}
        | AtomicCast AtomicCastNode {value: LiteralNode}
        | Literal LiteralNode {value: NumberType}
        | Unary UnaryNode {operator: TokenData, right: Expr}
        | Assignment AssignmentNode {name: TokenData}
    }
}
