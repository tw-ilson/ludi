use crate::ast::{ast, define_constructors, define_enum, define_nodes};
use crate::type::DataType;
use crate::env::Name;
use crate::err::{LangError, Result};
use crate::parse_err;
use itertools::Itertools;

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

pub trait Normalize<Tgt> {
    fn normalize(self, v0: &mut usize) -> Tgt;
}

impl Normalize<Result<NormalizedAST>> for ast::ParseTree {
    fn normalize(self, v0: &mut usize) -> Result<NormalizedAST> {
        self.into_iter().map(|expr| expr.normalize(v0)).collect()
    }
}
impl Normalize<Result<NormalExpr>> for ast::Expr {
    fn normalize(self, v0: &mut usize) -> Result<NormalExpr> {
        Normalize::<ast::Expr>::normalize(self, v0).try_into()
    }
}

impl Normalize<ast::Expr> for ast::Expr {
    fn normalize(self, v0: &mut usize) -> ast::Expr {
        match self {
            ast::Expr::Let(node) => node.normalize(v0),
            ast::Expr::FnCall(node) => node.normalize(v0),
            _ => self,
        }
    }
}

fn next_name(v0: &mut usize) -> Name {
    use std::str::FromStr;
    let name = format!("v{}", v0);
    (*v0) += 1;
    Name::from_str(&name).expect("uncaught lexer bug")
}

fn args_to_assignment_list(
    args: Vec<ast::Expr>,
    v0: &mut usize,
) -> (Vec<Name>, Vec<Option<ast::Expr>>) {
    args.into_iter()
        .map(|arg_expr| match arg_expr {
            ast::Expr::Term(node) => (node.name, None),
            _ => (next_name(v0), Some(arg_expr)),
        })
        .unzip()
}

fn expression_in_assignments(
    arg_names: Vec<Name>,
    arg_vals: Vec<Option<ast::Expr>>,
    inner_expr: ast::Expr,
    v0: &mut usize,
) -> ast::Expr {
    itertools::zip_eq(arg_names, arg_vals).fold(
        inner_expr,
        |acc, (arg_name, arg_val)| match arg_val {
            None => acc,
            Some(initializer) => ast::let_node(arg_name, initializer, Some(acc)).normalize(v0),
        },
    )
}

fn normalize_fncall_with_inner(
    args: Vec<ast::Expr>,
    inner: ast::Expr,
    v0: &mut usize,
) -> ast::Expr {
    let (arg_names, arg_vals) = args_to_assignment_list(args, v0);
    expression_in_assignments(arg_names, arg_vals, inner, v0)
}

fn make_normal_fn_callee(callee: ast::Expr, arg_names: Vec<Name>, v0: &mut usize) -> ast::Expr {
    let arg_names_expr = arg_names
        .iter()
        .map(|an| ast::term_node(an.clone()))
        .collect_vec();
    match callee {
        ast::Expr::Term(node) => ast::fn_call_node(ast::term_node(node.name), arg_names_expr),
        _ => {
            let callee_name = next_name(v0);
            ast::let_node(
                callee_name.clone(),
                callee,
                Some(ast::fn_call_node(
                    ast::term_node(callee_name),
                    arg_names_expr,
                )),
            )
            // .normalize(v0)
        }
    }
}

// converts a function to A-Normal Form on the AST
impl Normalize<ast::Expr> for ast::FnCallNode {
    fn normalize(self, v0: &mut usize) -> ast::Expr {
        let (arg_names, arg_vals) = args_to_assignment_list(self.args, v0);
        expression_in_assignments(
            arg_names.clone(),
            arg_vals,
            make_normal_fn_callee(self.callee, arg_names, v0),
            v0,
        )
    }
}

impl Normalize<ast::Expr> for ast::LetNode {
    fn normalize(self, v0: &mut usize) -> ast::Expr {
        match self.initializer {
            ast::Expr::FnCall(node) => {
                let (arg_names, arg_vals) = args_to_assignment_list(node.args, v0);
                expression_in_assignments(
                    arg_names.clone(),
                    arg_vals,
                    ast::let_node(
                        self.name,
                        make_normal_fn_callee(node.callee, arg_names, v0),
                        self.region,
                    ),
                    // .normalize(v0),
                    v0,
                )
            }
            // ast::Expr::UnaryOperation(node) => todo!(),
            // ast::Expr::BinaryOperation(node) => todo!(),
            _ => ast::let_node(self.name, self.initializer, self.region),
        }
    }
}

impl Normalize<ast::Expr> for ast::UnaryOperationNode {
    fn normalize(self, v0: &mut usize) -> ast::Expr {
        match self.right {
            ast::Expr::FnCall(node) => {
                let (arg_names, arg_vals) = args_to_assignment_list(node.args, v0);
                expression_in_assignments(
                    arg_names.clone(),
                    arg_vals,
                    ast::unary_operation_node(
                        self.operator,
                        make_normal_fn_callee(node.callee, arg_names, v0),
                    ),
                    v0,
                )
            }
            _ => ast::unary_operation_node(self.operator, self.right),
        }
    }
}

impl Normalize<ast::Expr> for ast::BinaryOperationNode {
    fn normalize(self, v0: &mut usize) -> ast::Expr {
        let ((left_arg_names, left_arg_vals), left_expr_normal) = match self.left {
            ast::Expr::FnCall(node) => {
                let (arg_names, arg_vals) = args_to_assignment_list(node.args, v0);
                (
                    (arg_names.clone(), arg_vals),
                    make_normal_fn_callee(node.callee, arg_names, v0),
                )
            }
            _ => ((vec![], vec![]), self.left),
        };
        let ((right_arg_names, right_arg_vals), right_expr_normal) = match self.right {
            ast::Expr::FnCall(node) => {
                let (arg_names, arg_vals) = args_to_assignment_list(node.args, v0);
                (
                    (arg_names.clone(), arg_vals),
                    make_normal_fn_callee(node.callee, arg_names, v0),
                )
            }
            _ => ((vec![], vec![]), self.right),
        };
        expression_in_assignments(
            left_arg_names
                .into_iter()
                .chain(right_arg_names.into_iter())
                .collect(),
            left_arg_vals
                .into_iter()
                .chain(right_arg_vals.into_iter())
                .collect(),
            ast::binary_operation_node(left_expr_normal, self.operator, right_expr_normal),
            v0,
        )
    }
}
// Verification boilerplate

impl From<NormalVal> for NormalExpr {
    fn from(value: NormalVal) -> Self {
        NormalExpr::Val(ValNode { value }.into())
    }
}

impl From<ast::Expr> for Name {
    fn from(value: ast::Expr) -> Self {
        match value {
            ast::Expr::Term(node) => node.name,
            _ => panic!("normalized AST expects name in this position, got {:#?}", value),
        }
    }
}

impl TryFrom<ast::Expr> for NormalVal {
    type Error = LangError;
    fn try_from(value: ast::Expr) -> Result<Self> {
        use ast::Expr;
        match value {
            Expr::Let(_) => parse_err!("Failed to Normalize, found let-stmt in value position"),
            Expr::Grouping(_) => parse_err!("Failed to Normalize, found grouping in AST"),
            Expr::FnCall(node) => Ok(fn_call_node(
                node.callee.into(),
                node.args.into_iter().map_into().collect(),
            )),
            Expr::FnDef(node) => Ok(fn_def_node(node.signature, node.body.try_into()?)),
            Expr::UnaryOperation(node) => {
                Ok(unary_operation_node(node.operator, node.right.try_into()?))
            }
            Expr::BinaryOperation(node) => Ok(binary_operation_node(
                node.left.try_into()?,
                node.operator,
                node.right.try_into()?,
            )),
            Expr::Term(node) => Ok(term_node(node.name)),
            Expr::Frame(node) => Ok(frame_node(
                node.expression_list
                    .into_iter()
                    .map(|e| e.try_into())
                    .try_collect()?,
            )),
            Expr::Literal(node) => Ok(literal_node(node.value)),
            Expr::AtomicCast(_) => panic!(),
            Expr::ShapeCast(_) => panic!(),
        }
    }
}

impl TryFrom<ast::Expr> for NormalExpr {
    type Error = LangError;
    fn try_from(value: ast::Expr) -> Result<Self> {
        use ast::Expr;
        match value {
            Expr::Let(node) => Ok(let_node(
                node.name,
                node.initializer.try_into()?,
                node.region
                    .expect("normalized ast cannot contain inferred let-region")
                    .try_into()?,
            )),
            Expr::Grouping(node) => Ok(node.expression.try_into()?),
            _ => Ok(val_node(value.try_into()?)),
        }
    }
}
