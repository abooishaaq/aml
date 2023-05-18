use crate::ty::Type;

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
    Var(String),
    Call(Box<Expr>, Vec<Expr>),
    Proj(Box<Expr>, String),
    Tuple(Vec<Expr>),
    Object(Vec<(String, Expr)>),
    IfElse(Box<Expr>, Box<Expr>, Box<Expr>),
    ArithOp(Box<Expr>, ArithOp, Box<Expr>),
    CmpOp(Box<Expr>, CmpOp, Box<Expr>),
    Assign(String, Box<Expr>),
    In(Box<Expr>, Box<Expr>),
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

pub type TypedProgram = Vec<TypedFunc>;
