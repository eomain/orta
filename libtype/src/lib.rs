
extern crate libsym;
extern crate libast;

use std::fmt;
use libsym::Error as SError;
use libsym::Scope;
use libsym::Table;
use libsym::TypeInfo;
use libast::Variable;
use libast::DataType;
use libast::SyntaxTree;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    //Id,
    Symbol(SError),
    Custom(String)
}

impl From<&Error> for String {
    fn from(e: &Error) -> Self
    {
        use Error::*;
        match e {
            //Id => "".into(),
            Symbol(s) => String::from(s),
            Custom(s) => s.clone()
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", String::from(self))
    }
}

#[derive(Debug)]
struct Env<'a> {
    pub table: Table<'a>
}

impl<'a> Env<'a> {
    pub fn new() -> Self
    {
       Self {
           table: Table::new()
	   }
    }
}

/*mod efmt {
    use libast::DataType;

    pub fn args(args: &Vec<DataType>) -> String
    {
        let mut s = String::new();
        for i in 1..args.len() {
            s.push();
        }
        s
    }
}*/

mod check {
    use super::Error;
    use libsym::Error as SError;
    use libsym::Scope;
    use libsym::TypeInfo;
    use libast::Typed;
    use libast::DataType;
    use libast::IntType;
    use libast::FloatType;
    use libast::Literal;
    use libast::Value;
    use libast::Return;
    use libast::{Expr, BinaryExpr, CallExpr};
    use libast::Function;

    /*fn id<'a>(scope: &'a Scope, name: &str) -> Result<&'a TypeInfo, Error>
    {
        if let Some(info) = scope.find(name) {
	        Ok(info)
        } else {
            Err(Error::Id)
	    }
    }*/

    fn literal(s: &mut Scope, l: &Literal) -> DataType
    {
        use IntType::*;
        use FloatType::*;
        match l {
            Literal::Signed(i) => {
                const MI32: isize = std::i32::MAX as isize;
                DataType::Integer(match i {
                    0..=0xFF => S8,
                    0..=0xFFFF => S16,
                    0..=MI32 => S32,
                    _ => S64
                })
            },
            Literal::Unsigned(u) => {
                const MU32: usize = std::u32::MAX as usize;
                DataType::Integer(match u {
                    0..=0xFF => U8,
                    0..=0xFFFF => U16,
                    0..=MU32 => U32,
                    _ => U64
                })
            },
            Literal::Float(f) => {
                const MF32: f64 = std::f32::MAX as f64;
                DataType::Float(match f {
                    0.0..=MF32 => F32,
                    _ => F64
                })
            },
            Literal::Boolean(_) => DataType::Boolean,
            Literal::String(_) => DataType::String,
            _ => unimplemented!()
        }
    }

    fn convertable(a: &DataType, b: &DataType) -> bool
    {
        use IntType::*;
        use FloatType::*;
        match a {
            DataType::Integer(a) => {
                if let DataType::Integer(b) = *b {
                    match a {
                        S8  => b == S8 ||b == U8,
                        S16 => b == S8 || b == S16 || b == U8 || b == U16,
                        S32 => b == S8 || b == S16 || b == S32 || b == U8 || b == U16 || b == U32,
                        S64 => b == S8 || b == S16 || b == S32 || b == S64 ||
                               b == U8 || b == U16 || b == U32 || b == U64,
                        U8  => b == U8 || b == S8,
                        U16 => b == U8 || b == U16 || b == S8 || b == S16,
                        U32 => b == U8 || b == U16 || b == U32 || b == S8 || b == S16 || b == S32,
                        U64 => b == U8 || b == U16 || b == U32 || b == U64 ||
                               b == S8 || b == S16 || b == S32 || b == S64
                    }
                } else {
                    false
                }
            },
            DataType::Float(a) => {
                unimplemented!()
            },
            _ => false
        }
    }

    fn value(s: &mut Scope, v: &mut Value, expt: Option<DataType>) -> Result<(), Error>
    {
        *v = match v {
            Value::Unit => return Ok(()),
            Value::Literal(l, _) => {
                let l = l.clone();
                let lit = literal(s, &l);
                if let Some(t) = expt {
                    if convertable(&t, &lit) {
                        Value::Literal(l, t)
                    } else {
                        Value::Literal(l, lit)
                    }
                } else {
                    Value::Literal(l, lit)
                }
            },
            Value::Variable(v) => {
                if let Some(_) = s.find(&v.name) {
                    // TODO
                } else {
                    return Err(Error::Symbol(SError::Undefined(v.name.clone())));
                }

                return Ok(());
            }
        };
        Ok(())
    }

    fn call(s: &mut Scope, c: &mut CallExpr, expt: Option<DataType>) -> Result<(), Error>
    {
        let (args, ret) = if let Some(sym) = s.find(&c.name) {
            if let TypeInfo::Function(sig) = sym {
                sig.clone()
            } else {
                return Err(Error::Symbol(SError::NotFunction(c.name.clone())));
            }
        } else {
            return Err(Error::Symbol(SError::Undefined(c.name.clone())));
        };

        let (a, b) = (args.len(), c.args.len());
        if a != b {
            return Err(Error::Custom(
                format!(
                    "incorrect number of positional arguments\n  found: {}\n  expected {}",
                    b, a
                )
            ));
        }

        let mut i = 0;
        c.rtype = ret;
        for arg in &mut c.args {
            expr(s, arg, Some(args[i].clone()));
            i += 1;
        }
        Ok(())
    }

    fn bexpr(s: &mut Scope, b: &mut BinaryExpr, expt: Option<DataType>) -> Result<(), Error>
    {
        use BinaryExpr::*;
        match b {
            Add(a, b) |
            Sub(a, b) |
            Mul(a, b) |
            Div(a, b) |
            Mod(a, b) => {
                expr(s, a, expt.clone())?;
                expr(s, b, expt)?;
            }
        }
        Ok(())
    }

    fn expr(s: &mut Scope, e: &mut Expr, expt: Option<DataType>) -> Result<(), Error>
    {
        match e {
            Expr::Value(v) => value(s, v, expt)?,
            Expr::Binary(b) => bexpr(s, b, expt)?,
            Expr::Call(c) => call(s, c, expt)?,
            _ => ()
        }
        Ok(())
    }

    fn ret(s: &mut Scope, r: &mut Return, name: &str, expt: DataType) -> Result<(), Error>
    {
        if let Some(e) = &mut r.expr {
            expr(s, &mut *e, Some(expt.clone()))?;
            if *e.get_type() == expt {
                r.dtype = expt;
            } else {
                let msg = format!(
                    "return type in function: {}\n  found: type {}\n  expected: type {}",
                    name, e.get_type(), expt
                );
                return Err(Error::Custom(msg));
            }
        } else {
            r.dtype = DataType::Unit;
        }
        Ok(())
    }

    pub fn fun(s: &mut Scope, f: &mut Function) -> Result<(), Error>
    {
        for e in &mut f.expr {
            if let Expr::Return(r) = e {
                ret(s, r, &f.name, f.ret.clone())?;
            } else {
                expr(s, e, None)?;
            }
        }

        Ok(())
    }
}

pub fn init(ast: &mut SyntaxTree) -> Result<(), Error>
{
    let mut env = Env::new();
    for f in &ast.functions {
        let args = Vec::from(&f.param).iter().map(|a| a.1.clone()).collect();
        let ret = f.ret.clone();
        env.table.insert(&f.name, TypeInfo::Function((args, ret)));
    }

    for f in &mut ast.functions {
        let mut s = env.table.scope();
        check::fun(&mut s, f)?;
    }

    if !env.table.has_main() {
        return Err(Error::Custom("function 'main' undefined".into()));
    }

    Ok(())
}

#[cfg(test)]
mod tests {

}
