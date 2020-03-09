
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
use libast::{Assign, Return};
use libast::{Expr, BinaryExpr, BoolExpr, CallExpr, IfExpr, WhileExpr};
use libast::Function;

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
                    S8  => b == S8 || b == U8,
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
            if let DataType::Float(b) = *b {
                match a {
                    F32 => b == F32,
                    F64 => b == F32 || b == F64
                }
            } else {
                false
            }
        },
        _ => false
    }
}

fn type_error(f: &DataType, e: &DataType) -> Error
{
    Error::Custom(
        format!("type error\n found: {}\n expected: {}", f, e)
    )
}

fn value(i: &mut Info, s: &mut Scope,
         v: &mut Value, expt: Option<DataType>) -> Result<(), Error>
{
    *v = match v {
        Value::Unit => return Ok(()),
        Value::Literal(l, dtype) => {
            let l = l.clone();
            let lit = literal(s, &l);
            if let Some(t) = expt {
                if t == lit {
                    *dtype = lit;
                    return Ok(());
                } else if convertable(&t, &lit) {
                    Value::Literal(l, t)
                } else {
                    return Err(type_error(&lit, &t));
                }
            } else {
                Value::Literal(l, lit)
            }
        },
        Value::Variable(v) => {
            match s.find_type(&v.name) {
                Err(e) => return Err(e.into()),
                Ok((t, f)) => {
                    if let Some(expt) = expt {
                        if t != expt && t != DataType::Unset && f {
                            return Err(type_error(&t, &expt));
                        } else {
                            v.dtype = t.clone();
                        }
                        if convertable(&expt, &t) {
                            v.dtype = expt.clone();
                            s.insert_var(&v.name, expt.clone(), false);
                            i.pass();
                        } else {
                            // TODO: error
                        }
                    } else {
                        v.dtype = t.clone();
                    }
                }
            }
            return Ok(());
        }
    };
    Ok(())
}

fn call(info: &mut Info, s: &mut Scope,
        c: &mut CallExpr, expt: Option<DataType>) -> Result<(), Error>
{
    // check if the call is on a variable (function pointer)
    let var = s.find_var(&c.name).is_ok();

    // get the argument types and the return type
    let (args, ret) = match s.find_type(&c.name) {
        Err(e) => return Err(e.into()),
        Ok((sig, _)) => {
            if let DataType::Function(v, r) = &sig {
                if var {
                    c.var = Some(sig.clone());
                }
                (v.clone(), (**r).clone())
            } else {
                return Err(Error::Custom("not a function!".into()));
            }
        }
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
        if let Err(e) = expr(info, s, arg, Some(args[i].clone())) {
            return Err(Error::SubError(
                format!(
                    "argument type error\n  invoked function: {}\n  positional argument: {}\n",
                    &c.name, (i + 1)
                ),Box::new(e))
            );
        }
        i += 1;
    }
    Ok(())
}

fn bin(i: &mut Info, s: &mut Scope,
         b: &mut BinaryExpr, expt: Option<DataType>) -> Result<(), Error>
{
    use BinaryExpr::*;
    match b {
        Add(a, b) |
        Sub(a, b) |
        Mul(a, b) |
        Div(a, b) |
        Mod(a, b) => {
            expr(i, s, a, expt.clone())?;
            expr(i, s, b, expt)?;
            let (at, bt) = (a.get_type(), b.get_type());
            if at != bt {
                let sub = Box::new(type_error(bt, at));
                return Err(Error::SubError(format!(
                    "binary operation error\n"
                ), sub));
            }
        }
    }
    Ok(())
}

fn expr(i: &mut Info, s: &mut Scope,
        e: &mut Expr, expt: Option<DataType>) -> Result<(), Error>
{
    match e {
        Expr::Value(v) => value(i, s, v, expt)?,
        Expr::Binary(b) => bin(i, s, b, expt)?,
        Expr::If(f) => conditional(i, s, f, expt)?,
        Expr::Assign(a) => assign(i, s, a)?,
        Expr::Call(c) => call(i, s, c, expt)?,
        _ => ()
    }
    Ok(())
}

fn ret(i: &mut Info, s: &mut Scope, r: &mut Return,
       name: &str, expt: DataType) -> Result<(), Error>
{
    if let Some(e) = &mut r.expr {
        if let Err(e) = expr(i, s, &mut *e, Some(expt.clone())) {
            let sub = Box::new(e);
            return Err(Error::SubError(format!(
                "return type error\n  from function: {}\n", name
            ), sub));
        }
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

fn bexpr(i: &mut Info, s: &mut Scope,
         b: &mut BoolExpr, expt: Option<DataType>) -> Result<(), Error>
{
    match b {
        BoolExpr::Expr(e) => expr(i, s, e, expt)?,
        _ => unimplemented!()
    }
    Ok(())
}

fn conditional(i: &mut Info, s: &mut Scope,
               br: &mut IfExpr, expt: Option<DataType>) -> Result<(), Error>
{
    bexpr(i, s, &mut br.cond, Some(DataType::Boolean))?;
    for e in &mut br.expr {
        expr(i, s, e, None)?;
    }
    if let Some(exprs) = &mut br.other {
        for e in exprs {
            expr(i, s, e, None)?;
        }
    }
    Ok(())
}

fn loop_while(i: &mut Info, s: &mut Scope, br: &mut WhileExpr,
              expt: Option<DataType>) -> Result<(), Error>
{
    bexpr(i, s, &mut br.cond, Some(DataType::Boolean))?;
    for e in &mut br.expr {
        expr(i, s, e, None)?;
    }
    Ok(())
}

fn assign(i: &mut Info, s: &mut Scope, a: &mut Assign) -> Result<(), Error>
{
    if s.contains(&a.id) {
        match s.find_var_type(&a.id) {
            Err(e) => return Err(e.into()),
            Ok(t) => a.dtype = t.clone()
        }
    }

    let expt = if a.dtype != DataType::Unset {
        Some(a.dtype.clone())
    } else {
        None
    };

    let f = expt.is_some();

    expr(i, s, &mut a.expr, expt)?;

    a.dtype = a.expr.get_type().clone();
    s.insert_var(&a.id, a.dtype.clone(), f);
    Ok(())
}

#[derive(Debug, Clone, PartialEq)]
struct Info {
    // if a second pass is needed
    pass: bool
}

impl Info {
    fn new() -> Self
    {
        Self {
            pass: false
        }
    }

    fn pass(&mut self)
    {
        self.pass = true;
    }

    fn second_pass(&self) -> bool
    {
        self.pass
    }
}

fn fun(i: &mut Info, s: &mut Scope, f: &mut Function) -> Result<(), Error>
{
    for e in &mut f.expr {
        if let Expr::Return(r) = e {
            ret(i, s, r, &f.name, f.ret.clone())?;
        } else {
            expr(i, s, e, None)?;
        }
    }

    Ok(())
}

pub fn fpass(s: &mut Scope, f: &mut Function) -> Result<(), Error>
{
    for (name, dtype) in &Vec::from(&f.param) {
        s.insert_var(name, dtype.clone(), true);
    }

    let mut info = Info::new();

    fun(&mut info, s, f)?;
    if info.second_pass() {
        fun(&mut info, s, f)?;
    }

    Ok(())
}
