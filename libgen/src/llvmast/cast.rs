
use super::*;

#[inline]
fn integer(t: &Type) -> bool
{
    match t {
        Type::Int(_) | Type::Uint(_) => true,
        _ => false
    }
}

fn trunc(c: &mut Context, val: Value, from: &Type,
         into: &Type, v: &mut Vec<Inst>) -> Register
{
    let r = c.id.register();
    let op = Operation::Trunc(r.clone(), val, into.clone());
    v.push(Inst::new(op, from.clone()));
    r
}

fn zext(c: &mut Context, val: Value, from: &Type,
        into: &Type, v: &mut Vec<Inst>) -> Register
{
    let r = c.id.register();
    let op = Operation::Zext(r.clone(), val, into.clone());
    v.push(Inst::new(op, from.clone()));
    r
}

fn sext(c: &mut Context, val: Value, from: &Type,
        into: &Type, v: &mut Vec<Inst>) -> Register
{
    let r = c.id.register();
    let op = Operation::Sext(r.clone(), val, into.clone());
    v.push(Inst::new(op, from.clone()));
    r
}

fn fptrunc(c: &mut Context, val: Value, from: &Type,
           into: &Type, v: &mut Vec<Inst>) -> Register
{
    let r = c.id.register();
    let op = Operation::FpTrunc(r.clone(), val, into.clone());
    v.push(Inst::new(op, from.clone()));
    r
}

fn fpext(c: &mut Context, val: Value, from: &Type,
         into: &Type, v: &mut Vec<Inst>) -> Register
{
    let r = c.id.register();
    let op = Operation::FpExt(r.clone(), val, into.clone());
    v.push(Inst::new(op, from.clone()));
    r
}

fn fptoui(c: &mut Context, val: Value, from: &Type,
          into: &Type, v: &mut Vec<Inst>) -> Register
{
    let r = c.id.register();
    let op = Operation::FpToUi(r.clone(), val, into.clone());
    v.push(Inst::new(op, from.clone()));
    r
}

fn fptosi(c: &mut Context, val: Value, from: &Type,
          into: &Type, v: &mut Vec<Inst>) -> Register
{
    let r = c.id.register();
    let op = Operation::FpToSi(r.clone(), val, into.clone());
    v.push(Inst::new(op, from.clone()));
    r
}

fn uitofp(c: &mut Context, val: Value, from: &Type,
          into: &Type, v: &mut Vec<Inst>) -> Register
{
    let r = c.id.register();
    let op = Operation::UiToFp(r.clone(), val, into.clone());
    v.push(Inst::new(op, from.clone()));
    r
}

fn sitofp(c: &mut Context, val: Value, from: &Type,
          into: &Type, v: &mut Vec<Inst>) -> Register
{
    let r = c.id.register();
    let op = Operation::SiToFp(r.clone(), val, into.clone());
    v.push(Inst::new(op, from.clone()));
    r
}

fn ptr_to_ptr(c: &mut Context, val: Value, from: &Type,
              into: &Type, v: &mut Vec<Inst>) -> Register
{
    let r = c.id.register();
    let op = Operation::Bitcast(r.clone(), val, into.clone());
    v.push(Inst::new(op, from.clone()));
    r
}

fn int_to_ptr(c: &mut Context, val: Value, from: &Type,
              into: &Type, v: &mut Vec<Inst>) -> Register
{
    let r = c.id.register();
    let op = Operation::IntToPtr(r.clone(), val, into.clone());
    v.push(Inst::new(op, from.clone()));
    r
}

fn ptr_to_int(c: &mut Context, val: Value, from: &Type,
              into: &Type, v: &mut Vec<Inst>) -> Register
{
    let r = c.id.register();
    let op = Operation::PtrToInt(r.clone(), val, into.clone());
    v.push(Inst::new(op, from.clone()));
    r
}

pub fn cast(c: &mut Context, e: &ast::Cast,
            v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    let (from, val) = unary_expr(c, &e.expr, v);
    let into = type_cast(&e.dtype);

    let value = match (&from, &into) {
        (Type::Pointer(_), Type::Pointer(t)) => {
            Value::Reg(ptr_to_ptr(c, val, &from, &into, v))
        },
        (Type::Uint(_), Type::Pointer(t)) => {
            Value::Reg(int_to_ptr(c, val, &from, &t, v))
        },
        (Type::Pointer(_), Type::Uint(_)) => {
            Value::Reg(ptr_to_int(c, val, &from, &into, v))
        },
        (Type::Uint(a), Type::Int(b)) |
        (Type::Int(a), Type::Int(b)) => {
            if a == b {
                val
            } else if a < b {
                Value::Reg(sext(c, val, &from, &into, v))
            } else {
                Value::Reg(trunc(c, val, &from, &into, v))
            }
        },
        (Type::Int(a), Type::Uint(b)) |
        (Type::Uint(a), Type::Uint(b)) => {
            if a == b {
                val
            } else if a < b {
                Value::Reg(zext(c, val, &from, &into, v))
            } else {
                Value::Reg(trunc(c, val, &from, &into, v))
            }
        },
        (Type::Float, Type::Double) => {
            Value::Reg(fpext(c, val, &from, &into, v))
        },
        (Type::Double, Type::Float) => {
            Value::Reg(fptrunc(c, val, &from, &into, v))
        },
        (Type::Int(_), Type::Float) |
        (Type::Int(_), Type::Double) => {
            Value::Reg(sitofp(c, val, &from, &into, v))
        },
        (Type::Uint(_), Type::Float) |
        (Type::Uint(_), Type::Double) => {
            Value::Reg(uitofp(c, val, &from, &into, v))
        },
        (Type::Float, Type::Int(_)) |
        (Type::Double, Type::Int(_)) => {
            Value::Reg(fptosi(c, val, &from, &into, v))
        },
        (Type::Float, Type::Uint(_)) |
        (Type::Double, Type::Uint(_)) => {
            Value::Reg(fptoui(c, val, &from, &into, v))
        },
        _ => if from == into { val } else { unreachable!() }
    };

    Some(vec![(into, value)])
}
