use crate::ast::*;
use crate::ty::{Type, TypeClass};
use std::collections::HashMap;

type TypeEnv = HashMap<String, Type>;
type TypeSubst = Vec<(usize, Type)>;

#[derive(Debug)]
pub enum Err {
    Unify(Type, Type),
    InfiniteRecursion(Type, Type),
}

fn subst(tysub: &mut TypeSubst, tyvar: usize, t: &Type) -> Result<bool, Err> {
    let mut changed = false;
    for (_, ty) in tysub.iter_mut() {
        changed = ty.subst(tyvar, t) || changed;
    }
    Ok(changed)
}

fn extend(tysub: &mut TypeSubst, tysub2: TypeSubst) -> Result<(), Err> {
    for (i, ty) in tysub2 {
        if !subst(tysub, i, &ty)? {
            tysub.push((i, ty));
        }
    }
    Ok(())
}

fn occurs_check(tyvar: usize, ty: &Type) -> bool {
    match ty {
        Type::Int => false,
        Type::Bool => false,
        Type::Tuple(tys) => tys.iter().any(|ty| occurs_check(tyvar, ty)),
        Type::Object(fields) => fields.iter().any(|(_, ty)| occurs_check(tyvar, ty)),
        Type::Func(arg_tys, ret_ty) => {
            arg_tys.iter().any(|ty| occurs_check(tyvar, ty)) || occurs_check(tyvar, ret_ty)
        }
        Type::Index(ty, _) => occurs_check(tyvar, ty),
        Type::TypeVar(i) => *i == tyvar,
        _ => false,
    }
}
fn apply_subst(tyenv: &mut TypeEnv, subst: &TypeSubst) {
    for (_, ty) in tyenv {
        for (i, ty2) in subst {
            ty.subst(*i, ty2);
        }
    }
}

pub struct Infer {
    tyvar_count: usize,
    tyclass_num: usize,
}

impl Infer {
    pub fn new() -> Self {
        Infer {
            tyvar_count: 0,
            tyclass_num: 0,
        }
    }

    fn unify(&mut self, ty1: &Type, ty2: &Type) -> Result<TypeSubst, Err> {
        match (ty1, ty2) {
            (Type::TypeVar(i1), Type::TypeVar(i2)) => {
                if *i1 == *i2 {
                    Ok(TypeSubst::new())
                } else {
                    Ok(vec![(*i1, ty2.clone())])
                }
            }
            (Type::TypeVar(i), _) => {
                if occurs_check(*i, ty2) {
                    Err(Err::InfiniteRecursion(ty1.clone(), ty2.clone()))
                } else {
                    Ok(vec![(*i, ty2.clone())])
                }
            }
            (_, Type::TypeVar(i)) => {
                if occurs_check(*i, ty1) {
                    Err(Err::InfiniteRecursion(ty1.clone(), ty2.clone()))
                } else {
                    Ok(vec![(*i, ty1.clone())])
                }
            }
            (Type::Tuple(tys1), Type::Tuple(tys2)) => {
                // right one can be pseudo type
                if tys1.len() < tys2.len() {
                    Err(Err::Unify(ty1.clone(), ty2.clone()))
                } else {
                    let mut subst = TypeSubst::new();
                    for (ty1, ty2) in tys1.iter().zip(tys2.iter()) {
                        extend(&mut subst, self.unify(ty1, ty2)?)?;
                    }
                    Ok(subst)
                }
            }
            (Type::Index(ty1, i1), Type::Index(ty2, i2)) => {
                if i1 != i2 {
                    return Err(Err::Unify(*ty1.clone(), *ty2.clone()));
                }
                self.unify(ty1, ty2)
            }
            (Type::Tuple(tys), Type::Index(ty2, i2)) => {
                if tys.len() < *i2 {
                    return Err(Err::Unify(ty1.clone(), *ty2.clone()));
                }
                self.unify(&tys[*i2], ty2)
            }
            (Type::Index(_, _), Type::Tuple(_)) => {
                return self.unify(ty2, ty1);
            }
            (Type::Object(fields1), Type::Object(fields2)) => {
                if fields1.len() != fields2.len() {
                    return Err(Err::Unify(ty1.clone(), ty2.clone()));
                }
                let mut subst = TypeSubst::new();
                for ((key1, ty1), (key2, ty2)) in fields1.iter().zip(fields2.iter()) {
                    if key1 != key2 {
                        return Err(Err::Unify(ty1.clone(), ty2.clone()));
                    }
                    extend(&mut subst, self.unify(ty1, ty2)?)?;
                }
                Ok(subst)
            }
            (Type::Func(arg_tys1, ret_ty1), Type::Func(arg_tys2, ret_ty2)) => {
                if arg_tys1.len() != arg_tys2.len() {
                    return Err(Err::Unify(ty1.clone(), ty2.clone()));
                }
                let mut subst = TypeSubst::new();
                for (ty1, ty2) in arg_tys1.iter().zip(arg_tys2.iter()) {
                    extend(&mut subst, self.unify(ty1, ty2)?)?;
                }
                extend(&mut subst, self.unify(ret_ty1, ret_ty2)?)?;
                Ok(subst)
            }
            (Type::TypeClass(n1, tc1), Type::TypeClass(n2, tc2)) => {
                if tc1 != tc2 {
                    return Err(Err::Unify(ty1.clone(), ty2.clone()));
                }
                let mut subst = TypeSubst::new();
                self.tyvar_count += 1;
                let tyvar = Type::TypeVar(self.tyvar_count);
                extend(&mut subst, self.unify(ty1, &tyvar)?)?;
                extend(&mut subst, self.unify(ty2, &tyvar)?)?;
                Ok(subst)
            }
            (Type::TypeClass(_, t), ty) => {
                if t.has_instance(ty) {
                    Ok(TypeSubst::new())
                } else {
                    match ty {
                        Type::Index(tyy, _) => self.unify(tyy, ty1),
                        Type::Tuple(tys) => {
                            if tys.len() == 0 {
                                return Err(Err::Unify(ty1.clone(), ty2.clone()));
                            }
                            self.unify(&tys[0], ty1)
                        }
                        Type::Object(fields) => {
                            if fields.len() == 0 {
                                return Err(Err::Unify(ty1.clone(), ty2.clone()));
                            }
                            self.unify(&fields[0].1, ty1)
                        }
                        _ => Err(Err::Unify(ty1.clone(), ty2.clone())),
                    }
                }
            }
            (_, Type::TypeClass(_, _)) => {
                return self.unify(ty2, ty1);
            }
            (ty1, ty2) => {
                if ty1 == ty2 {
                    Ok(TypeSubst::new())
                } else {
                    Err(Err::Unify(ty1.clone(), ty2.clone()))
                }
            }
        }
    }

    fn type_infer_expr(
        &mut self,
        expr: &Expr,
        tyenv: &mut TypeEnv,
    ) -> Result<(Type, TypeSubst), Err> {
        match expr {
            Expr::Num(_) => Ok((Type::Int, TypeSubst::new())),
            Expr::Var(name) => {
                let ty = tyenv.get(name).unwrap();
                if let Type::TypeVar(i) = ty {
                    Ok((ty.clone(), vec![(*i, Type::TypeVar(*i))]))
                } else {
                    Ok((ty.clone(), TypeSubst::new()))
                }
            }
            Expr::Tuple(exprs) => {
                let mut tys = Vec::new();
                let mut subst = TypeSubst::new();
                for expr in exprs {
                    let (ty, s) = self.type_infer_expr(expr, tyenv)?;
                    tys.push(ty);
                    extend(&mut subst, s)?;
                }
                Ok((Type::Tuple(tys), subst))
            }
            Expr::Object(fields) => {
                let mut tys = Vec::new();
                let mut subst = TypeSubst::new();
                for (key, value) in fields {
                    let (ty, s) = self.type_infer_expr(value, tyenv)?;
                    tys.push((key.clone(), ty));
                    extend(&mut subst, s)?;
                }
                Ok((Type::Object(tys), subst))
            }
            Expr::IfElse(cond, then, else_) => {
                let (cond_ty, mut subst) = self.type_infer_expr(cond, tyenv)?;
                extend(&mut subst, self.unify(&cond_ty, &Type::Bool)?)?;
                let (then_ty, subst2) = self.type_infer_expr(then, tyenv)?;
                extend(&mut subst, subst2)?;
                let (else_ty, subst2) = self.type_infer_expr(else_, tyenv)?;
                extend(&mut subst, subst2)?;
                extend(&mut subst, self.unify(&then_ty, &else_ty)?)?;
                Ok((then_ty, subst))
            }
            Expr::ArithOp(ex1, _, ex2) => {
                let (ty1, mut subst) = self.type_infer_expr(ex1, tyenv)?;
                let (ty2, subst2) = self.type_infer_expr(ex2, tyenv)?;
                extend(&mut subst, subst2)?;
                extend(&mut subst, self.unify(&ty1, &Type::Int)?)?;
                extend(&mut subst, self.unify(&ty2, &Type::Int)?)?;
                Ok((Type::Int, subst))
            }
            Expr::CmpOp(ex1, op, ex2) => {
                let (ty1, mut subst) = self.type_infer_expr(ex1, tyenv)?;
                let (ty2, subst2) = self.type_infer_expr(ex2, tyenv)?;
                extend(&mut subst, subst2)?;
                extend(&mut subst, self.unify(&ty1, &ty2)?)?;
                match op {
                    CmpOp::Eq | CmpOp::Ne => {
                        // ty1 and ty2 should be Eq's instance
                        let tyclass = Type::TypeClass(self.tyclass_num, TypeClass::Eq);
                        extend(&mut subst, self.unify(&ty1, &tyclass)?)?;
                        extend(&mut subst, self.unify(&ty2, &tyclass)?)?;
                    }
                    _ => {
                        // ty1 and ty2 should be Ord's instance
                        let tyclass = Type::TypeClass(self.tyclass_num, TypeClass::Ord);
                        extend(&mut subst, self.unify(&ty1, &tyclass)?)?;
                        extend(&mut subst, self.unify(&ty2, &tyclass)?)?;
                    }
                }
                Ok((Type::Bool, subst))
            }
            Expr::Call(func, args) => {
                let (func_ty, mut subst) = self.type_infer_expr(func, tyenv)?;
                let (arg_tys, subst2): (Vec<_>, Vec<_>) = args
                    .iter()
                    .map(|arg| self.type_infer_expr(arg, tyenv))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .unzip();
                extend(&mut subst, subst2.into_iter().flatten().collect())?;
                self.tyvar_count += 1;
                let ret_ty = Type::TypeVar(self.tyvar_count);
                extend(
                    &mut subst,
                    self.unify(&func_ty, &Type::Func(arg_tys, Box::new(ret_ty.clone())))?,
                )?;
                Ok((ret_ty, subst))
            }
            Expr::Assign(name, ex) => {
                let (ty, subst) = self.type_infer_expr(ex, tyenv)?;
                tyenv.insert(name.clone(), ty.clone());
                Ok((ty, subst)) // ignore subst
            }
            Expr::Proj(v, ex) => {
                let (ty, subst) = self.type_infer_expr(v, tyenv)?;
                let idx = ex[1..].parse::<usize>().unwrap();
                self.tyvar_count += 1;
                let temp = Type::Index(Box::new(ty), idx);
                Ok((temp, subst))
            }
            Expr::In(asgn, ex) => {
                let (_, mut subst) = self.type_infer_expr(asgn, tyenv)?;
                let (ty2, subst2) = self.type_infer_expr(ex, tyenv)?;
                extend(&mut subst, subst2)?;
                Ok((ty2, subst))
            }
        }
    }

    pub fn type_infer(&mut self, program: &Program) -> Result<TypedProgram, Err> {
        let mut env = TypeEnv::new();

        for func in program {
            self.tyvar_count += 1;
            env.insert(func.name.clone(), Type::TypeVar(self.tyvar_count));
        }

        for func in program {
            for (_, arg) in func.args.iter().enumerate() {
                self.tyvar_count += 1;
                env.insert(arg.clone(), Type::TypeVar(self.tyvar_count));
            }
            let (retty, mut subst) = self.type_infer_expr(&func.body, &mut env)?;
            let func_ty = Type::Func(
                func.args
                    .iter()
                    .map(|arg| env.get(arg).unwrap().clone())
                    .collect(),
                Box::new(retty.clone()),
            );
            extend(
                &mut subst,
                self.unify(&func_ty, &env.get(&func.name).unwrap())?,
            )?;
            apply_subst(&mut env, &subst);
        }

        let typed_program = program
            .iter()
            .map(|func| {
                let retty = match env.get(&func.name).unwrap().clone() {
                    Type::Func(_, retty) => *retty,
                    _ => unreachable!(),
                };
                Ok(TypedFunc {
                    name: func.name.clone(),
                    args: func
                        .args
                        .iter()
                        .map(|arg| (arg.clone(), env.get(arg).unwrap().clone()))
                        .collect::<Vec<_>>(),
                    body: (func.body.clone(), retty),
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(typed_program)
    }
}
