
use super::*;
use ast::Address;
use ast::Deref;
use llvm::Operation::{
    Alloca, Store, Load
};

pub fn address(c: &mut Context, a: &Address,
               v: &mut Vec<Inst>) -> (Type, Value)
{
    let l = c.id.local(&a.id);
    let r = Register::from(&l);
    (type_cast(&a.dtype), Value::Reg(r))
}

pub fn deref(c: &mut Context, d: &Deref,
             v: &mut Vec<Inst>) -> (Type, Value)
{
    let (_, val) = unary_expr(c, &d.expr, v);
    let reg = match val {
        Value::Reg(r) => r,
        _ => unreachable!()
    };

    let dtype = type_cast(&d.dtype);
    let r = c.id.register();
    let op = Load(r.clone(), dtype.clone(), Rc::new(reg));
    v.push(Inst::new(op, dtype.clone()));
    
    (dtype, Value::Reg(r))
}
