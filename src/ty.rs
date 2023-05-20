use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    TypeVar(TVar),
    TypeCon(&'static str),
    Func(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TVar {
    pub id: usize,
}

impl TVar {
    pub fn new(id: usize) -> Self {
        Self { id }
    }
}

fn tvar_letter(id: usize) -> char {
    (b'a' + (id % 26) as u8) as char
}

impl Display for TVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", tvar_letter(self.id))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scheme {
    pub tyvars: Vec<usize>,
    pub ty: Type,
}

impl Scheme {
    pub fn new(ty: Type) -> Self {
        Self {
            tyvars: Vec::new(),
            ty,
        }
    }
    pub fn new_with_vars(tyvars: Vec<usize>, ty: Type) -> Self {
        Self { tyvars, ty }
    }
}

impl Display for Scheme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.tyvars.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            write!(f, "forall ")?;
            for (i, tyvar) in self.tyvars.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", TVar::new(*tyvar))?;
            }
            write!(f, ". {}", self.ty)
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::TypeCon(name) => write!(f, "{}", name),
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
            Type::Func(arg_tys, ret_ty) => {
                write!(f, "{} -> {}", arg_tys, ret_ty)
            }
            Type::TypeVar(tyvar) => write!(f, "{}", tyvar),
        }
    }
}
