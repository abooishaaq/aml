use crate::ty::Type;
use core::fmt::Display;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Num(i64),
    Bool(bool),
    Var(String),
    Call(Arc<Expr>, Arc<Expr>),
    Proj(Arc<Expr>, Arc<Expr>),
    Tuple(Vec<Expr>),
    Object(Vec<(String, Expr)>),
    IfElse(Arc<Expr>, Arc<Expr>, Arc<Expr>),
    ArithOp(Arc<Expr>, ArithOp, Arc<Expr>),
    CmpOp(Arc<Expr>, CmpOp, Arc<Expr>),
    Assign(String, Arc<Expr>),
    In(Arc<Expr>, Arc<Expr>),
    Unit,
}

pub type TypedExpr = (Expr, Type);

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub args: Vec<String>,
    pub body: Expr,
}

pub type Program = Vec<Func>;

#[derive(Debug, Clone)]
pub struct TypedFunc {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub body: TypedExpr,
}

impl Display for CmpOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CmpOp::Eq => write!(f, "=="),
            CmpOp::Ne => write!(f, "!="),
            CmpOp::Lt => write!(f, "<"),
            CmpOp::Le => write!(f, "<="),
            CmpOp::Gt => write!(f, ">"),
            CmpOp::Ge => write!(f, ">="),
        }
    }
}

impl Display for ArithOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArithOp::Add => write!(f, "+"),
            ArithOp::Sub => write!(f, "-"),
            ArithOp::Mul => write!(f, "*"),
            ArithOp::Div => write!(f, "/"),
        }
    }
}

pub fn stringify_expr(expr: &Expr, indent: usize) -> String {
    match expr {
        Expr::Num(n) => format!("{}", n),
        Expr::Bool(b) => format!("{}", b),
        Expr::Var(x) => x.clone(),
        Expr::Call(e, es) => {
            format!("{} {}", stringify_expr(e, 0), stringify_expr(&es, 0))
        }
        Expr::Proj(e, x) => format!("{}.{}", stringify_expr(e, 0), stringify_expr(x, 0)),
        Expr::Tuple(es) => {
            let mut s = format!("(");
            for (i, e) in es.iter().enumerate() {
                if i > 0 {
                    s += ", ";
                }
                s += &stringify_expr(e, 0);
            }
            s += ")";
            s
        }
        Expr::Object(fields) => {
            let mut s = format!("{{\n");
            for (i, (x, e)) in fields.iter().enumerate() {
                if i > 0 {
                    s += ",\n";
                }
                s += &" ".repeat(indent + 4);
                s += &format!("{}: {}", x, stringify_expr(e, indent + 4));
            }
            s += "\n";
            s += &" ".repeat(indent);
            s += "}";
            s
        }
        Expr::IfElse(e1, e2, e3) => {
            let blanks = " ".repeat(indent + 4);
            format!(
                "if {} \n{}then {}\n{}else {}",
                stringify_expr(e1, indent),
                blanks,
                stringify_expr(e2, indent + 4),
                blanks,
                stringify_expr(e3, indent + 4)
            )
        }
        Expr::ArithOp(e1, op, e2) => format!(
            "({} {} {})",
            stringify_expr(e1, 0),
            op,
            stringify_expr(e2, 0)
        ),
        Expr::CmpOp(e1, op, e2) => format!(
            "({} {} {})",
            stringify_expr(e1, 0),
            op,
            stringify_expr(e2, 0)
        ),
        Expr::Assign(x, e) => format!("{} = {}", x, stringify_expr(e, 0)),
        Expr::In(e1, e2) => {
            let blanks = " ".repeat(4);
            format!(
                "{} in\n{}{}",
                stringify_expr(e1, indent),
                blanks,
                stringify_expr(e2, indent + 4)
            )
        }
        Expr::Unit => "()".to_string(),
    }
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.name)?;
        for (i, x) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", x)?;
        }
        write!(
            f,
            ") ->\n{}{}\n{}",
            " ".repeat(4),
            stringify_expr(&self.body, 4),
            ";;\n"
        )
    }
}
