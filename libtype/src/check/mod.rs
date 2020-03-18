
mod cast;
mod method;
mod ret;

use std::rc::Rc;
use super::Error;
use libsym::Error as SError;
use libsym::Scope;
use libsym::TypeInfo;
use libast::Typed;
use libast::DataType;
use libast::DataRecord;
use libast::IntType;
use libast::FloatType;
use libast::{Literal, Variable};
use libast::Value;
use libast::{Assign, Return};
use libast::{
    Expr, BinaryExpr, BoolExpr,
    CallExpr, IfExpr, WhileExpr,
    CompExpr, LogicalExpr
};
use libast::Function;
pub use method::mpass;

fn type_equal<T>(a: &T, b: &T) -> bool
    where T: Typed
{
    let (a, b) = (a.get_type(), b.get_type());
    let (a, b) = (a.derived(), b.derived());
    a == b
}

fn literal(s: &mut Scope, l: &Literal) -> DataType
{
    use IntType::*;
    use FloatType::*;
    match l {
        Literal::Signed(i) => {
            const MIN8: isize = std::i8::MIN as isize;
            const MAX8: isize = std::i8::MAX as isize;
            const MIN16: isize = std::i16::MIN as isize;
            const MAX16: isize = std::i16::MAX as isize;
            const MIN32: isize = std::i32::MIN as isize;
            const MAX32: isize = std::i32::MAX as isize;

            DataType::Integer(match i {
                MIN8..=MAX8 => S8,
                MIN16..=MAX16 => S16,
                MIN32..=MAX32 => S32,
                _ =>  S64
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

fn named<'a>(s: &'a Scope, t: &'a DataType) -> &'a DataType
{
    if let DataType::Named(name) = t {
        match s.find_named_type(name) {
            Err(_) => t,
            Ok(d) => d
        }
    } else {
        t
    }
}

fn convertable(a: &DataType, b: &DataType) -> bool
{
    use IntType::*;
    use FloatType::*;

    if (a == b) {
        return true;
    }

    let (a, b) = (a.derived(), b.derived());

    match a {
        DataType::Integer(a) => {
            match *b {
                DataType::Integer(b) => {
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
                },
                _ => false
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
        DataType::Pointer(_) => {
            match *b {
                DataType::Integer(_) => true,
                _ => false
            }
        }
        _ => false
    }
}

fn type_error(f: &DataType, e: &DataType) -> Error
{
    error!("type error\n found: {}\n expected: {}", f, e)
}

fn variable(i: &mut Info, s: &mut Scope,
            v: &mut Variable, expt: Option<DataType>) -> Result<(), Error>
{
    match s.find_type(&v.name) {
        Err(e) => return Err(e.into()),
        Ok((dtype, fin)) => {
            if let Some(expt) = expt {
                if dtype != expt && dtype != DataType::Unset && fin {
                    return Err(type_error(&dtype, &expt));
                }

                if !dtype.function() {
                    if (dtype == expt || (!expt.unique() && convertable(&expt, &dtype))) {
                        v.dtype = expt.clone();
                        s.insert_var(&v.name, expt.clone(), false);
                        i.pass();
                        return Ok(());
                    }
                }
            }
            v.dtype = dtype.clone();
        }
    }
    Ok(())
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
        Value::Variable(v) => return variable(i, s, v, expt)
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
            if let DataType::Function(v, r) = &sig.derived() {
                if var {
                    c.var = Some(sig.clone());
                }
                (v.clone(), (**r).clone())
            } else {
                return Err(error!("`{}` is not a function", &c.name));
            }
        }
    };

    let (a, b) = (args.len(), c.args.len());
    if a != b {
        return Err(error!(
            "incorrect number of positional arguments\n  found: {}\n  expected {}",
            b, a
        ));
    }

    let mut i = 0;
    c.rtype = ret;
    for arg in &mut c.args {
        if let Err(e) = expr(info, s, arg, Some(args[i].clone())) {
            return Err(suberror!(e,
                "argument type error\n  invoked function: {}\n  positional argument: {}\n",
                &c.name, (i + 1)
            ));
        }
        let (found, expt) = (arg.get_type(), &args[i]);
        if found != expt {
            return Err(type_error(found, expt));
        }
        i += 1;
    }
    Ok(())
}

fn bin(i: &mut Info, s: &mut Scope,
       b: &mut BinaryExpr, expt: Option<DataType>) -> Result<(), Error>
{
    use DataType::*;
    use BinaryExpr::*;
    match b {
        Add(a, b) |
        Sub(a, b) |
        Mul(a, b) |
        Div(a, b) |
        Mod(a, b) => {
            expr(i, s, a, expt.clone())?;
            expr(i, s, b, expt)?;
            let (a, b) = (a.get_type(), b.get_type());
            let (a, b) = (a.derived(), b.derived());

            match (a, b) {
                (Integer(_), Integer(_)) |
                (Float(_), Float(_)) => {
                    if a != b {
                        return Err(suberror!(type_error(b, a), "arithmetic operation error\n"));
                    }
                },
                _ => return Err(error!(
                    "cannot perform arithmetic on types `{}` and `{}`", a, b
                ))
            }
        }
    }
    Ok(())
}

fn cmp(i: &mut Info, s: &mut Scope,
       c: &mut CompExpr, expt: Option<DataType>) -> Result<(), Error>
{
    use DataType::*;
    use CompExpr::*;
    match c {
        Eq(a, b) |
        Ne(a, b) |
        Gt(a, b) |
        Lt(a, b) |
        Ge(a, b) |
        Le(a, b) => {
            expr(i, s, a, None)?;
            expr(i, s, b, Some(a.get_type().clone()))?;

            if type_equal(&**a, &**b) {
                return Ok(())
            } else {
                let (a, b) = (a.get_type(), b.get_type());
                return Err(error!("cannot compare types `{}` and `{}`", a, b))
            }
        }
    }
    Ok(())
}

fn log(i: &mut Info, s: &mut Scope,
       l: &mut LogicalExpr, expt: Option<DataType>) -> Result<(), Error>
{
    use DataType::*;
    use LogicalExpr::*;
    match l {
        And(a, b) |
        Or(a, b) => {
            expr(i, s, a, None)?;
            expr(i, s, b, Some(a.get_type().clone()))?;
        },
        Not(a) => expr(i, s, a, None)?
    }
    Ok(())
}

fn expr(i: &mut Info, s: &mut Scope,
        e: &mut Expr, expt: Option<DataType>) -> Result<(), Error>
{
    match e {
        Expr::Value(v) => value(i, s, v, expt)?,
        Expr::Binary(b) => bin(i, s, b, expt)?,
        Expr::Comp(c) => cmp(i, s, c, expt)?,
        Expr::Logical(l) => log(i, s, l, expt)?,
        Expr::If(f) => conditional(i, s, f, expt)?,
        Expr::While(w) => loop_while(i, s, w, expt)?,
        Expr::Return(r) => ret(i, s, r, i.ret.clone())?,
        Expr::Assign(a) => assign(i, s, a)?,
        Expr::Call(c) => call(i, s, c, expt)?,
        Expr::Cast(c) => cast::cast(i, s, c, expt)?,
        Expr::At(a) => method::at(i, s, a, expt)?,
        Expr::Field(f) => method::field(i, s, f, expt)?,
        Expr::Method(m) => method::method(i, s, m, expt)?,
        _ => unimplemented!()
    }
    Ok(())
}

fn ret(i: &mut Info, s: &mut Scope, r: &mut Return,
       expt: DataType) -> Result<(), Error>
{
    if let Some(e) = &mut r.expr {
        if let Err(e) = expr(i, s, &mut *e, Some(expt.clone())) {
            return Err(suberror!(e, "return type error\n  from function: {}\n", &i.name));
        }
        if *e.get_type() == expt {
            r.dtype = expt;
        } else {
            return Err(error!(
                "return type in function: {}\n  found: type {}\n  expected: type {}",
                &i.name, e.get_type(), expt
            ));
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
    let mut s = Scope::new(Some(s));
    bexpr(i, &mut s, &mut br.cond, Some(DataType::Boolean))?;
    for e in &mut br.expr {
        expr(i, &mut s, e, None)?;
    }
    if let Some(exprs) = &mut br.other {
        for e in exprs {
            expr(i, &mut s, e, None)?;
        }
    }
    Ok(())
}

fn loop_while(i: &mut Info, s: &mut Scope, br: &mut WhileExpr,
              expt: Option<DataType>) -> Result<(), Error>
{
    let mut s = Scope::new(Some(s));
    bexpr(i, &mut s, &mut br.cond, Some(DataType::Boolean))?;
    for e in &mut br.expr {
        expr(i, &mut s, e, None)?;
    }
    Ok(())
}

fn assign(i: &mut Info, s: &mut Scope, a: &mut Assign) -> Result<(), Error>
{
    if s.contains(&a.id) {
        if i.get_count() == 1 {
            return Err(error!("redeclaration of variable `{}`", &a.id));
        }
        match s.find_var_type(&a.id) {
            Err(e) => return Err(e.into()),
            Ok(t) => a.dtype = t.clone()
        }
    }

    let expt = if s.is_final(&a.id) {
        Some(s.get_type(&a.id).unwrap())
    } else {
        if a.dtype != DataType::Unset {
            Some(named(s, &a.dtype).clone())
        } else {
            None
        }
    };

    let f = expt.is_some();

    expr(i, s, &mut a.expr, expt.clone())?;

    if f {
        let (found, expt) = (a.expr.get_type(), &expt.unwrap());
        if found != expt {
            return Err(type_error(found, expt));
        }
    }

    a.dtype = a.expr.get_type().clone();
    s.insert_var(&a.id, a.dtype.clone(), f);
    Ok(())
}

#[derive(Debug, Clone, PartialEq)]
pub struct Info {
    // function name
    name: String,
    // function return type
    ret: DataType,
    // if a second pass is needed
    pass: bool,
    pcount: usize,
    at: Option<Rc<DataRecord>>
}

impl Info {
    fn new(name: &str, ret: DataType) -> Self
    {
        Self {
            name: name.into(),
            ret,
            pass: false,
            pcount: 1,
            at: None
        }
    }

    fn get_count(&self) -> usize
    {
        self.pcount
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
        expr(i, s, e, None)?;
    }

    Ok(())
}

pub fn fpass(s: &mut Scope, f: &mut Function) -> Result<(), Error>
{
    for (name, dtype) in &Vec::from(&f.param) {
        s.insert_var(name, dtype.clone(), true);
    }

    let mut info = Info::new(&f.name, f.ret.clone());

    fun(&mut info, s, f)?;
    if info.second_pass() {
        info.pcount += 1;
        fun(&mut info, s, f)?;
    }

    if !ret::returns(&mut info, s, f) {
        return Err(error!("function: `{}`: expected return statement", &f.name));
    }

    Ok(())
}
