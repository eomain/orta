
use super::*;
use ast::Address;
use llvm::Operation::{
    Alloca, Store
};

pub fn address(c: &mut Context, a: &Address,
               v: &mut Vec<Inst>) -> (Type, Value)
{
    let l = c.id.local(&a.id);
    let r = Register::from(&l);
    (type_cast(&a.dtype), Value::Reg(r))
}
