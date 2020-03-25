
use super::*;
use ast::Expr;
use ast::StructLiteral as Struct;
use llvm::Operation::{
    Alloca, Store, Load, GetElPtr
};

fn get_index(c: &mut Context, a: &(Type, Value), i: usize,
         v: &mut Vec<Inst>) -> Register
{
    let r = c.id.register();
    let op = GetElPtr(
        r.clone(), (Type::Pointer(Box::new(a.0.clone())), a.1.clone()),
        vec![
            (Type::Int(64), Value::Uint(0)),
            (Type::Int(32), Value::Uint(i))
        ]
    );
    v.push(Inst::new(op, a.0.clone()));
    r
}

fn fields(c: &mut Context, s: &Struct, r: &Register,
          v: &mut Vec<Inst>)
{
    let dtype = type_cast(&s.dtype);
    let structure = (dtype.clone(), Value::Reg(r.clone()));

    let mut i = 0;
    for (_, e) in &s.fields {
        let mut r = get_index(c, &structure, i, v);
        set(c, e, &r, v);
        i += 1;
    }
}

pub fn structure(c: &mut Context, s: &Struct,
                 v: &mut Vec<Inst>) -> (Type, Value)
{
    let dtype = type_cast(&s.dtype);
    let (r, alloc) = c.get_reg();
    let reg = Rc::new(r.clone());

    if alloc {
        let op = Alloca(reg, None);
        v.push(Inst::new(op, dtype.clone()));
    }

    fields(c, s, &r, v);

    (Type::Pointer(Box::new(dtype)), Value::Reg(r))
}
