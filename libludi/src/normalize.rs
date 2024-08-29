use crate::ast;
use crate::ast::{define_ast, define_constructors, define_enum, define_nodes};
use crate::data::DataType;
use crate::env::Name;
use crate::err::Result;
use itertools::zip_eq;

// A-Normalized ast
// in addition, grouping elimination
// and casting
define_ast! {
    NormalExpr {
        Let { name: Name, initializer: NormalVal, region: NormalExpr }
        | Val { value: NormalVal }
    }
    NormalVal {
         UnaryOperation { operator: ast::UnaryOpType, right: NormalExpr }
        | FnDef { signature: ast::CallSignature, body:NormalExpr }
        | BinaryOperation { left: NormalExpr, operator: ast::BinaryOpType, right: NormalExpr }
        | FnCall { callee: Name, args: Vec<Name> }
        | Literal { value: DataType }
        | Frame { expression_list: Vec<NormalExpr> }
        | Term { name: Name }
    }
}

type NodeRef<T> = Box<T>;

pub type NormalizedAST = Vec<NormalExpr>;

impl From<NormalVal> for NormalExpr {
    fn from(value: NormalVal) -> Self {
        NormalExpr::Val(ValNode { value }.into())
    }
}

pub trait Normalize<Tgt> {
    fn normalize(self, v0: &mut usize) -> Tgt;
}

impl Normalize<NormalizedAST> for ast::ParseTree {
    fn normalize(self, v0: &mut usize) -> NormalizedAST {
        self.into_iter().map(|expr| expr.normalize(v0)).collect()
    }
}

fn next_name(v0: &mut usize) -> Name {
    use std::str::FromStr;
    let name = format!("v{}", v0);
    (*v0) += 1;
    Name::from_str(&name).expect("uncaught lexer bug")
}

fn fncall_to_assignment_list(
    fncall: ast::FnCallNode,
    v0: &mut usize,
) -> Vec<(Name, Option<NormalVal>)> {
    fn arg_to_assignment_list(
        arg_expr: ast::Expr,
        v0: &mut usize,
    ) -> impl Iterator<Item = (Name, Option<NormalVal>)> + '_ {
        match arg_expr {
            ast::Expr::Term(node) => Box::new(std::iter::once((node.name, None)))
                as Box<dyn Iterator<Item = (Name, Option<NormalVal>)> + '_>,
            ast::Expr::FnCall(node) => Box::new(
                fncall_to_assignment_list(*node, v0).into_iter().chain(std::iter::once((
                            next_name(v0),  
                            None
                            )))),
            _ => Box::new(std::iter::once((
                next_name(v0),
                Some(
                    Normalize::<Option<NormalVal>>::normalize(arg_expr, v0)
                        .expect("expected value -- potential parser bug"),
                ),
            ))),
        }
    }

    fncall
        .args
        .into_iter()
        .flat_map(move |arg_expr| {
            arg_to_assignment_list(arg_expr, v0).collect::<Vec<(Name, Option<NormalVal>)>>()
        })
        .collect()
}

// converts a function to A-Normal Form on the AST
impl Normalize<NormalExpr> for ast::FnCallNode {
    fn normalize(self, v0: &mut usize) -> NormalExpr {
        let (arg_names, arg_vals): (Vec<Name>, Vec<Option<NormalVal>>) = self
            .args
            .into_iter()
            .flat_map(|arg_expr| match arg_expr {
                ast::Expr::Term(node) => [(node.name, None)],
                _ => [(
                    next_name(v0),
                    Some(
                        Normalize::<Option<NormalVal>>::normalize(arg_expr, v0)
                            .expect("expected value -- potential parser bug"),
                    ),
                )],
            })
            .unzip();

        // base case
        let fn_val = match self.callee {
            ast::Expr::Term(node) => fn_call_node(node.name, arg_names.clone()).into(),
            _ => {
                let callee_name = next_name(v0);
                NormalExpr::Let(
                    LetNode {
                        name: callee_name.clone(),
                        initializer: Normalize::<Option<NormalVal>>::normalize(self.callee, v0)
                            .expect("expected value -- potential parser bug"),
                        region: NormalVal::FnCall(
                            FnCallNode {
                                callee: callee_name,
                                args: arg_names.clone(),
                            }
                            .into(),
                        )
                        .into(),
                    }
                    .into(),
                )
            }
        };

        // recursive
        zip_eq(arg_names, arg_vals).fold(fn_val, |acc, (arg_name, arg_val)| match arg_val {
            None => acc,
            Some(initializer) => let_node(arg_name, initializer, acc),
        })
    }
}

impl Normalize<NormalExpr> for ast::LetNode {
    fn normalize(self, v0: &mut usize) -> NormalExpr {
        NormalExpr::Let(
            LetNode {
                name: self.name,
                initializer: match self.initializer.normalize(v0) {
                    Some(val) => val,
                    None => panic!(),
                },
                region: self
                    .region
                    .expect("unexpected -- the details aren't iron clad")
                    .normalize(v0),
            }
            .into(),
        )
    }
}

impl Normalize<Option<NormalVal>> for ast::Expr {
    fn normalize(self, v0: &mut usize) -> Option<NormalVal> {
        match self {
            ast::Expr::Let(_) => panic!(),    // error
            ast::Expr::FnCall(_) => panic!(), //error
            ast::Expr::Term(node) => Some(term_node(node.name)),
            ast::Expr::Literal(node) => Some(literal_node(node.value)),
            ast::Expr::FnDef(node) => Some(fn_def_node(node.signature, node.body.normalize(v0))),
            ast::Expr::UnaryOperation(node) => Some(unary_operation_node(
                node.operator,
                node.right.normalize(v0),
            )),
            ast::Expr::BinaryOperation(node) => Some(binary_operation_node(
                node.left.normalize(v0),
                node.operator,
                node.right.normalize(v0),
            )),
            ast::Expr::Frame(_) => todo!(), // TODO: is frame a function call?
            ast::Expr::Grouping(_) => todo!(),
            ast::Expr::ShapeCast(_) => todo!(),
            ast::Expr::AtomicCast(_) => todo!(),
        }
    }
}

impl Normalize<NormalExpr> for ast::Expr {
    fn normalize(self, v0: &mut usize) -> NormalExpr {
        match self {
            ast::Expr::FnCall(node) => node.normalize(v0), // see above
            ast::Expr::Let(node) => node.normalize(v0),
            // Expr::FnDef(node) => NormalVal::FnDef(
            //     FnDefNode {
            //         signature: node.signature,
            //         body: node.body.normalize(),
            //     }
            //     .into(),
            // )
            // .into(),

            // Expr::Literal(node) => NormalVal::Literal(LiteralNode { value: node.value }.into()),
            //
            // Expr::Grouping(node) => node.expression.normalize(),
            //
            // Expr::BinaryOperation(node) => NormalExpr::BinaryOperation(
            //     BinaryNode {
            //         left: node.left.normalize(),
            //         operator: node.operator,
            //         right: node.right.normalize(),
            //     }
            //     .into(),
            // ),
            // Expr::UnaryOperation(node) => NormalExpr::UnaryOperation(
            //     UnaryNode {
            //         operator: node.operator,
            //         right: node.right.normalize(),
            //     }
            //     .into(),
            // ),
            // ast::Expr::Frame(_) => todo!(),
            // ast::Expr::AtomicCast(_) => todo!(),
            // ast::Expr::ShapeCast(_) => todo!(),

            // Expr::Term(node) => NormalVal::Term(TermNode { name: node.name }.into()),
            _ => todo!(),
        }
    }
}
