#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Tuple(Vec<Type>),
    Index(Box<Type>, usize),
    Object(Vec<(String, Type)>),
    Func(Vec<Type>, Box<Type>),
    TypeVar(usize),
}

impl Type {
    pub fn subst(&mut self, tyvar: usize, t: &Type) -> bool {
        let mut changed = false;
        match self {
            Type::Int => {}
            Type::Bool => {}
            Type::Tuple(tys) => {
                for ty in tys {
                    changed = ty.subst(tyvar, t) || changed;
                }
            }
            Type::Object(fields) => {
                for (_, ty) in fields {
                    changed = ty.subst(tyvar, t) || changed;
                }
            }
            Type::Func(arg_tys, ret_ty) => {
                for ty in arg_tys {
                    changed = ty.subst(tyvar, t) || changed;
                }
                changed = ret_ty.subst(tyvar, t) || changed;
            }
            Type::TypeVar(i) => {
                if *i == tyvar {
                    *self = t.clone();
                    changed = true;
                }
            }
            Type::Index(ty, i) => {
                if let Type::TypeVar(j) = ty.as_ref() {
                    if let Type::Tuple(tys) = t {
                        if *j == tyvar && tys.len() >= *i {
                            changed = true;
                            *self = tys[*i - 1].clone();
                        }
                    }
                }
            }
        };
        changed
    }
}
