
mod bitwise;
mod cast;
mod complex;
mod method;
mod ptr;
mod ret;

use std::rc::Rc;
use super::Error;
use libsym::Error as SError;
use libsym::Scope;
use libsym::Info as TypeInfo;
use libsym::Definition;
use libsym::{
    Function as Fun,
    Variable as Var,
    VarKind
};
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
    CompExpr, LogicalExpr, Loop, UnsafeExpr
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
        Literal::Character(_) => DataType::Char,
        Literal::String(_) => DataType::String,
        _ => unimplemented!()
    }
}

fn named<'a>(s: &'a Scope, t: &'a DataType) -> Option<Rc<DataType>>
{
    if let DataType::Named(name) = t {
        s.find_typename(name)
    } else {
        None
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
    match s.find(&v.name) {
        None => return Err(error!("undefined variable '{}'", &v.name)),
        Some(info) => {
            let dtype = info.dtype.clone();
            let abs = info.absolute;

            assert_ne!(*dtype, DataType::Unset);

            if let Some(expt) = expt {
                if *dtype != expt && abs {
                    return Err(type_error(&dtype, &expt));
                }

                if !dtype.function() {
                    if (*dtype == expt || (!expt.unique() && convertable(&expt, &dtype))) {
                        v.dtype = expt.clone();

                        let d = Definition::Variable(Var::new(VarKind::Temp));
                        let info = TypeInfo::new(d, Rc::new(expt.clone()), false);
                        s.insert(v.name.clone(), info);

                        i.pass();
                        return Ok(());
                    }
                }
            }
            v.dtype = (*dtype).clone();
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
        Value::Complex(c) => return complex::complex(i, s, c, expt),
        Value::Variable(v) => return variable(i, s, v, expt)
    };
    Ok(())
}

fn call(info: &mut Info, s: &mut Scope,
        c: &mut CallExpr, expt: Option<DataType>) -> Result<(), Error>
{
    // check if the call is on a variable (function pointer)
    let var = match s.find_definition(&c.name) {
        Some(Definition::Variable(_)) => true,
        Some(Definition::Function(f)) => {
            if f.r#unsafe && !info.is_unsafe() {
                return Err(error!("calling function `{}`, outside an 'unsafe'", &c.name));
            } else {
                false
            }
        }
        _ => false
    };

    // get the argument types and the return type
    let (args, ret) = match s.get_type(&c.name) {
        None => return Err(error!("undefined function '{}'", &c.name)),
        Some(dtype) => {
            if let DataType::Function(v, r) = &dtype.derived() {
                if var {
                    c.var = Some((*dtype).clone());
                }
                (v.clone(), r.clone())
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
    c.rtype = (*ret).clone();
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
        Expr::Bit(b) => bitwise::bit(i, s, b, expt)?,
        Expr::If(f) => conditional(i, s, f, expt)?,
        Expr::Loop(l) => loops(i, s, l, expt)?,
        Expr::While(w) => loop_while(i, s, w, expt)?,
        Expr::Return(r) => ret(i, s, r, i.ret.clone())?,
        Expr::Assign(a) => assign(i, s, a)?,
        Expr::Call(c) => call(i, s, c, expt)?,
        Expr::Cast(c) => cast::cast(i, s, c, expt)?,
        Expr::At(a) => method::at(i, s, a, expt)?,
        Expr::Field(f) => method::field(i, s, f, expt)?,
        Expr::Method(m) => method::method(i, s, m, expt)?,
        Expr::Index(e) => complex::index(i, s, e, expt)?,
        Expr::Address(a) => ptr::address(i, s, a, expt)?,
        Expr::Deref(d) => ptr::deref(i, s, d, expt)?,
        Expr::Unsafe(u) => unsafe_expr(i, s, u, expt)?,
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

fn loops(i: &mut Info, s: &mut Scope, lp: &mut Loop,
         expt: Option<DataType>) -> Result<(), Error>
{
    let mut s = Scope::new(Some(s));
    for e in &mut lp.expr {
        expr(i, &mut s, e, None)?;
    }
    Ok(())
}

fn loop_while(i: &mut Info, s: &mut Scope, wl: &mut WhileExpr,
              expt: Option<DataType>) -> Result<(), Error>
{
    let mut s = Scope::new(Some(s));
    bexpr(i, &mut s, &mut wl.cond, Some(DataType::Boolean))?;
    for e in &mut wl.expr {
        expr(i, &mut s, e, None)?;
    }
    Ok(())
}

fn assign(i: &mut Info, s: &mut Scope, a: &mut Assign) -> Result<(), Error>
{
    let abs = if s.contains(&a.id) {
        if a.declare && i.get_count() == 1 {
            return Err(error!("redeclaration of variable `{}`", &a.id));
        }
        match s.find(&a.id) {
            None => unreachable!(),
            Some(info) => {
                match info.definition {
                    Definition::Variable(_) => (),
                    _ => return Err(error!(""))
                }
                a.dtype = (*info.dtype).clone();
                info.abs()
            }
        }
    } else {
        false
    };

    let expt = if abs {
        Some((*s.get_type(&a.id).unwrap()).clone())
    } else {
        if a.dtype != DataType::Unset {
            Some(match named(s, &a.dtype) {
                None => a.dtype.clone(),
                Some(s) => (*s).clone()
            })
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
    let d = Definition::Variable(Var::new(VarKind::Temp));
    let info = TypeInfo::new(d, Rc::new(a.dtype.clone()), f);
    s.insert(a.id.clone(), info);
    Ok(())
}

fn unsafe_expr(i: &mut Info, s: &mut Scope, u: &mut UnsafeExpr,
               expt: Option<DataType>) -> Result<(), Error>
{
    i.unsafe_run(|i| expr(i, s, &mut *u.expr, expt.clone()))?;
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
    at: Option<Rc<DataRecord>>,
    unsafes: usize
}

impl Info {
    fn new(name: &str, ret: DataType) -> Self
    {
        Self {
            name: name.into(),
            ret,
            pass: false,
            pcount: 1,
            at: None,
            unsafes: 0
        }
    }

    fn is_unsafe(&self) -> bool
    {
        self.unsafes > 0
    }

    fn unsafe_run<F>(&mut self, mut f: F) -> Result<(), Error>
        where F: FnMut(&mut Info) -> Result<(), Error>
    {
        self.unsafes += 1;
        f(self)?;
        self.unsafes -= 1;
        Ok(())
    }

    #[inline]
    fn unsafe_inc(&mut self)
    {
        self.unsafes += 1;
    }

    #[inline]
    fn unsafe_dec(&mut self)
    {
        self.unsafes -= 1;
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
    let u = f.prop.r#unsafe;

    if u {
        i.unsafe_inc();

        for e in &mut f.expr {
            expr(i, s, e, None)?;
        }

        i.unsafe_dec();
    } else {
        for e in &mut f.expr {
            expr(i, s, e, None)?;
        }
    }

    Ok(())
}

pub fn fpass(s: &mut Scope, f: &mut Function) -> Result<(), Error>
{
    for (name, dtype) in &Vec::from(&f.param) {
        let d = Definition::Variable(Var::new(VarKind::Temp));
        let info = TypeInfo::new(d, Rc::new(dtype.clone()), true);
        s.insert(name.into(), info);
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
