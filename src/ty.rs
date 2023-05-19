use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeClass {
    Eq,
    Ord,
}

impl TypeClass {
    pub fn has_instance(&self, ty: &Type) -> bool {
        match self {
            TypeClass::Eq => match ty {
                Type::Int => true,
                Type::Bool => true,
                Type::Tuple(tys) => tys.iter().all(|ty| TypeClass::Eq.has_instance(ty)),
                Type::Object(fields) => fields.iter().all(|(_, ty)| TypeClass::Eq.has_instance(ty)),
                Type::Func(_, _) => false,
                _ => false,
            },
            TypeClass::Ord => match ty {
                Type::Int => true,
                Type::Bool => false,
                Type::Tuple(tys) => tys.iter().all(|ty| TypeClass::Ord.has_instance(ty)),
                Type::Object(fields) => {
                    fields.iter().all(|(_, ty)| TypeClass::Ord.has_instance(ty))
                }
                Type::Func(_, _) => false,
                _ => false,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Tuple(Vec<Type>),
    Index(Box<Type>, usize),
    Object(Vec<(String, Type)>),
    Func(Vec<Type>, Box<Type>),
    TypeClass(usize, TypeClass),
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
            _ => {}
        };
        changed
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::Tuple(tys) => {
                write!(f, "(")?;
                for (i, ty) in tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            }
            Type::Object(fields) => {
                write!(f, "{{")?;
                for (i, (name, ty)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, ty)?;
                }
                write!(f, "}}")
            }
            Type::Func(arg_tys, ret_ty) => {
                write!(f, "(")?;
                for (i, ty) in arg_tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ") -> {}", ret_ty)
            }
            Type::TypeClass(_, _) => write!(f, "TypeClass"),
            Type::TypeVar(i) => write!(f, "t{}", i),
            Type::Index(ty, i) => write!(f, "{}[{}]", ty, i),
        }
    }
}