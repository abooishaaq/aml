use crate::ast::*;
use crate::ty::{Scheme, TVar, Type};
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::sync::Arc;

type TypeSubst = HashMap<usize, Type>;

#[derive(Debug, Clone)]
struct Constraint {
    ty1: Type,
    ty2: Type,
    ex: Vec<Arc<Expr>>,
}

impl Display for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} <--> {} in", self.ty1, self.ty2)?;
        for ex in &self.ex {
            write!(f, "\n\t{}", stringify_expr(ex.as_ref(), 0))?;
        }
        write!(f, "\n")
    }
}

#[derive(Debug, Clone)]
pub struct Env {
    types: HashMap<String, Scheme>,
}

impl Env {
    fn new() -> Self {
        Env {
            types: HashMap::new(),
        }
    }

    fn extend(&mut self, name: String, scheme: Scheme) {
        self.types.insert(name, scheme);
    }

    fn lookup(&self, name: &str) -> Option<&Scheme> {
        self.types.get(name)
    }
}

impl Constraint {
    fn new(ty1: Type, ty2: Type, ex: Vec<Arc<Expr>>) -> Self {
        Constraint { ty1, ty2, ex }
    }
}

trait Substitutable {
    fn apply(&mut self, tysub: &TypeSubst);
    fn ftv(&self) -> HashSet<usize>;
}

impl Substitutable for Type {
    fn apply(&mut self, tysub: &TypeSubst) {
        match self {
            Type::TypeVar(i) => {
                if let Some(ty) = tysub.get(&i.id) {
                    *self = ty.clone();
                } else {
                    *self = Type::TypeVar(i.clone());
                }
            }
            Type::Func(t1, t2) => {
                t1.apply(tysub);
                t2.apply(tysub);
            }
            Type::Tuple(tys) => {
                for ty in tys {
                    ty.apply(tysub);
                }
            }
            _ => {}
        }
    }

    fn ftv(&self) -> HashSet<usize> {
        match self {
            Type::TypeVar(i) => {
                let mut set = HashSet::new();
                set.insert(i.id);
                set
            }
            Type::Func(t1, t2) => t1.ftv().union(&t2.ftv()).cloned().collect(),
            Type::Tuple(tys) => {
                let mut set = HashSet::new();
                for ty in tys {
                    set.extend(ty.ftv());
                }
                set
            }
            _ => HashSet::new(),
        }
    }
}

impl Substitutable for Constraint {
    fn apply(&mut self, tysub: &TypeSubst) {
        self.ty1.apply(tysub);
        self.ty2.apply(tysub);
    }

    fn ftv(&self) -> HashSet<usize> {
        self.ty1.ftv().union(&self.ty2.ftv()).cloned().collect()
    }
}

impl Substitutable for Scheme {
    fn apply(&mut self, tysub: &TypeSubst) {
        self.ty.apply(tysub);
        self.tyvars.clear();
        self.tyvars.extend(self.ty.ftv());
    }

    fn ftv(&self) -> HashSet<usize> {
        self.ty.ftv()
    }
}

impl Substitutable for Env {
    fn apply(&mut self, tysub: &TypeSubst) {
        for scheme in self.types.values_mut() {
            scheme.apply(tysub);
        }
    }

    fn ftv(&self) -> HashSet<usize> {
        let mut set = HashSet::new();
        for scheme in self.types.values() {
            set.extend(scheme.ftv());
        }
        set
    }
}

#[derive(Debug)]
pub enum Err {
    InfiniteRecursion(TVar, Type),
    UnboundVariable(String),
    UnificationMismatch(Type, Type),
}

impl Display for Err {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Err::InfiniteRecursion(ty1, ty2) => {
                write!(f, "Infinite recursion: {} = {}", ty1, ty2)
            }
            Err::UnboundVariable(name) => write!(f, "Unbound variable: {}", name),
            Err::UnificationMismatch(ty1, ty2) => {
                write!(f, "Unification mismatch: {} != {}", ty1, ty2)
            }
        }
    }
}

pub struct Infer<'a> {
    tyvar_count: usize,
    env: &'a mut Env,
}

const TYPE_INT: Type = Type::TypeCon("int");
const TYPE_BOOL: Type = Type::TypeCon("bool");
const TYPE_UNIT: Type = Type::TypeCon("unit");

impl<'a> Infer<'a> {
    pub fn new(env: &'a mut Env) -> Self {
        Infer {
            tyvar_count: 0,
            env,
        }
    }

    fn fresh(&mut self) -> Type {
        let tyvar = Type::TypeVar(TVar::new(self.tyvar_count));
        self.tyvar_count += 1;
        tyvar
    }

    fn generalize(&self, ty: &Type, env: &Env) -> Scheme {
        let env_ftv = env.ftv();
        let ty_ftv = ty.ftv();
        let ftv = ty_ftv.difference(&env_ftv).cloned().collect();
        Scheme::new_with_vars(ftv, ty.clone())
    }

    fn normalize(&self, scheme: Scheme) -> Scheme {
        // nub == uniq
        let ftv = scheme.ftv();
        let mut tvar = 0;
        let ord = ftv.iter().map(|tv| {
            let tv = *tv;
            tvar += 1;
            (tv, TVar::new(tvar - 1))
        });
        let mut subst = TypeSubst::new();
        for (tv, ty) in ord {
            subst.insert(tv, Type::TypeVar(ty));
        }
        let mut sch = scheme.clone();
        sch.apply(&subst);
        sch
    }

    fn close_over(&self, ty: &Type) -> Scheme {
        let env = Env::new();
        self.normalize(self.generalize(ty, &env))
    }

    fn infer(&mut self, expr: &Expr) -> Result<(Type, Vec<Constraint>), Err> {
        match expr {
            Expr::Num(_) => Ok((TYPE_INT, vec![])),
            Expr::Bool(_) => Ok((TYPE_BOOL, vec![])),
            Expr::Var(name) => {
                let res = self
                    .env
                    .lookup(name)
                    .ok_or(Err::UnboundVariable(name.to_string()));
                match res {
                    Ok(scheme) => Ok((scheme.ty.clone(), vec![])),
                    Err(err) => Err(err),
                }
            }
            Expr::Call(func, arg) => {
                let (func_ty, func_c) = self.infer(func)?;
                let (arg_ty, arg_c) = self.infer(arg)?;
                let ret_ty = self.fresh();
                let (c, ret) = if arg_ty == TYPE_UNIT {
                    (vec![], func_ty)
                } else {
                    (
                        vec![Constraint::new(
                            func_ty.clone(),
                            Type::Func(Box::new(arg_ty), Box::new(ret_ty.clone())),
                            vec![func.clone()],
                        )],
                        ret_ty,
                    )
                };
                Ok((ret, c.into_iter().chain(func_c).chain(arg_c).collect()))
            }
            Expr::ArithOp(e1, _, e2) => {
                let (ty1, c1) = self.infer(e1)?;
                let (ty2, c2) = self.infer(e2)?;
                let cs = vec![
                    Constraint::new(ty1, TYPE_INT, vec![e1.clone()]),
                    Constraint::new(ty2, TYPE_INT, vec![e2.clone()]),
                ];
                Ok((TYPE_INT, cs.into_iter().chain(c1).chain(c2).collect()))
            }
            Expr::CmpOp(e1, _, e2) => {
                let (ty1, c1) = self.infer(e1)?;
                let (ty2, c2) = self.infer(e2)?;
                let cs = vec![Constraint::new(ty1, ty2, vec![e1.clone(), e2.clone()])];
                Ok((TYPE_BOOL, cs.into_iter().chain(c1).chain(c2).collect()))
            }
            Expr::Assign(name, e) => {
                let (ty, c) = self.infer(e)?;
                self.env.extend(name.clone(), self.close_over(&ty));
                Ok((TYPE_UNIT, c))
            }
            Expr::In(e1, e2) => {
                let (_, c1) = self.infer(e1)?;
                let (ty2, c2) = self.infer(e2)?;
                Ok((ty2, c1.into_iter().chain(c2).collect()))
            }
            Expr::IfElse(cond, tr, fl) => {
                let (ty1, c1) = self.infer(cond)?;
                let (ty2, c2) = self.infer(tr)?;
                let (ty3, c3) = self.infer(fl)?;
                let mut cs = vec![
                    Constraint::new(ty1, TYPE_BOOL, vec![cond.clone()]),
                    Constraint::new(ty2.clone(), ty3, vec![tr.clone(), fl.clone()]),
                ];
                cs.extend(c1);
                cs.extend(c2);
                cs.extend(c3);
                Ok((ty2, cs))
            }
            Expr::Tuple(exs) => {
                let mut cs = vec![];
                let mut tys = vec![];
                for ex in exs {
                    let (ty, c) = self.infer(ex)?;
                    tys.push(ty);
                    cs.extend(c);
                }
                let ty = Type::Tuple(tys);
                Ok((ty, cs))
            }
            Expr::Proj(ex, idx) => {
                let (ty, mut c) = self.infer(ex)?;
                let i = if let Expr::Num(n) = **idx {
                    n
                } else {
                    panic!("index must be number");
                };
                let tv = (0..i).map(|_| self.fresh()).collect::<Vec<_>>();
                let tv2 = self.fresh();
                let cs = vec![
                    Constraint::new(ty, Type::Tuple(tv.clone()), vec![ex.clone()]),
                    Constraint::new(tv2.clone(), tv[(i - 1) as usize].clone(), vec![ex.clone()]),
                ];
                c.extend(cs);
                Ok((tv2, c))
            }
            Expr::Unit => Ok((TYPE_UNIT, vec![])),
            _ => unimplemented!(),
        }
    }

    fn infer_func(&mut self, func: &Func) -> Result<Scheme, Err> {
        let fresh_vars = (0..func.args.len())
            .map(|_| self.fresh())
            .collect::<Vec<_>>();
        let retty = self.fresh();
        let mut fn_ty = fresh_vars.iter().rev().fold(retty.clone(), |acc, ty| {
            Type::Func(Box::new(ty.clone()), Box::new(acc))
        });
        self.env
            .extend(func.name.clone(), Scheme::new(fn_ty.clone()));
        for (arg, ty) in func.args.iter().rev().zip(fresh_vars.iter()) {
            self.env.extend(arg.clone(), Scheme::new(ty.clone()));
        }
        let (body_ty, mut cs) = self.infer(&func.body)?;
        cs.push(Constraint::new(
            body_ty,
            retty,
            vec![Arc::new(func.body.clone())],
        ));
        let unifier: Unifier = (TypeSubst::new(), cs);
        let subst = solver(unifier)?;
        fn_ty.apply(&subst);
        Ok(self.close_over(&fn_ty))
    }
}

type Unifier = (TypeSubst, Vec<Constraint>);

fn unify(t1: &Type, t2: &Type) -> Result<Unifier, Err> {
    match (t1, t2) {
        (Type::TypeVar(v1), Type::TypeVar(v2)) if v1 == v2 => Ok((TypeSubst::new(), vec![])),
        (Type::TypeVar(v1), t2) => {
            if occurs_check(v1, t2) {
                Err(Err::InfiniteRecursion(v1.clone(), t2.clone()))
            } else {
                bind(v1, t2)
            }
        }
        (t1, Type::TypeVar(v2)) => {
            if occurs_check(v2, t1) {
                Err(Err::InfiniteRecursion(v2.clone(), t1.clone()))
            } else {
                bind(v2, t1)
            }
        }
        (Type::Func(t11, t12), Type::Func(t21, t22)) => {
            let (s1, c1) = unify(t11, t21)?;
            let (s2, c2) = unify(t12, t22)?;
            let mut subst = s1;
            subst.extend(s2);
            Ok((subst, c1.into_iter().chain(c2).collect()))
        }
        (Type::Tuple(ts1), Type::Tuple(ts2)) => {
            if ts1.len() != ts2.len() {
                Err(Err::UnificationMismatch(t1.clone(), t2.clone()))
            } else {
                let mut subst = TypeSubst::new();
                let mut cs = vec![];
                for (t1, t2) in ts1.iter().zip(ts2.iter()) {
                    let (s, c) = unify(t1, t2)?;
                    subst.extend(s);
                    cs.extend(c);
                }
                Ok((subst, cs))
            }
        }
        (Type::TypeCon(tc1), Type::TypeCon(tc2)) => {
            if tc1 == tc2 {
                Ok((TypeSubst::new(), vec![]))
            } else {
                Err(Err::UnificationMismatch(t1.clone(), t2.clone()))
            }
        }
        (t1, t2) => Err(Err::UnificationMismatch(t1.clone(), t2.clone())),
    }
}

fn occurs_check(a: &TVar, t: &Type) -> bool {
    match t {
        Type::TypeVar(v) => a == v,
        Type::Func(t1, t2) => occurs_check(a, t1) || occurs_check(a, t2),
        Type::Tuple(ts) => ts.iter().any(|t| occurs_check(a, t)),
        _ => false,
    }
}

fn bind(a: &TVar, t: &Type) -> Result<Unifier, Err> {
    if t == &Type::TypeVar(a.clone()) {
        Ok((TypeSubst::new(), vec![]))
    } else if occurs_check(a, t) {
        Err(Err::InfiniteRecursion(a.clone(), t.clone()))
    } else {
        let mut subst = TypeSubst::new();
        let TVar { id: a } = a;
        subst.insert(*a, t.clone());
        Ok((subst, vec![]))
    }
}

fn compose(subst: &mut TypeSubst, subst2: &TypeSubst) {
    for (_, ty) in subst {
        ty.apply(subst2);
    }
}

fn solver(unifier: Unifier) -> Result<TypeSubst, Err> {
    let (mut subst, mut constraints) = unifier;
    while let Some(Constraint { ty1, ty2, ex: _ }) = constraints.pop() {
        let (s, c) = unify(&ty1, &ty2)?;
        constraints.extend(c);
        for c in constraints.iter_mut() {
            c.apply(&s);
        }
        compose(&mut subst, &s);
        subst.extend(s);
    }
    Ok(subst)
}

pub fn infer_top_level(prog: &Program) -> Result<HashMap<String, Scheme>, Err> {
    let mut env = Env::new();

    let mut res = HashMap::new();

    for func in prog {
        let mut lenv = env.clone();
        let mut infer = Infer::new(&mut lenv);
        let sch = infer.infer_func(func)?;
        res.insert(func.name.clone(), sch.clone());
        env.extend(func.name.clone(), sch);
    }

    Ok(res)
}
