
use super::*;
use ast::ArrayLiteral as Array;
use ast::Index;
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
            (Type::Int(64), Value::Uint(i))
        ]
    );
    v.push(Inst::new(op, a.0.clone()));
    r
}

fn offset(c: &mut Context, o: &(Type, Value), t: Type,
          val: Value, v: &mut Vec<Inst>) -> Register
{
    let r = c.id.register();
    let op = GetElPtr(
        r.clone(), o.clone(),
        vec![
            (t, val)
        ]
    );
    v.push(Inst::new(op, o.0.get_pointer_type()));
    r
}

fn get(c: &mut Context, t: Type, reg: Rc<Register>,
          v: &mut Vec<Inst>) -> Register
{
    let r = c.id.register();
    let op = Load(r.clone(), t.clone(), reg);
    v.push(Inst::new(op, t));
    r
}

pub fn index(c: &mut Context, i: &Index,
             v: &mut Vec<Inst>) -> (Type, Value)
{
    let tval = unary_expr(c, &i.expr, v);
    let r = get_index(c, &tval, 0, v);
    let mut index = (
        tval.0.get_array_to_pointer(),
        Value::Reg(r)
    );

    let (t, val) = unary_expr(c, &i.index, v);
    let r = offset(c, &index, t, val, v);
    let dtype = type_cast(&i.dtype);
    let r = get(c, dtype.clone(), Rc::new(r), v);

    (dtype, Value::Reg(r))
}

pub fn array(c: &mut Context, a: &Array,
             v: &mut Vec<Inst>) -> (Type, Value)
{
    let dtype = type_cast(&a.dtype);

    let r = c.get_reg();
    let reg = Rc::new(r.clone());

    let op = Alloca(reg, None);
    v.push(Inst::new(op, dtype.clone()));

    let array = (dtype.clone(), Value::Reg(r.clone()));
    let mut i = 0;

    let len = a.elements.len();
    if len > 0 {
        let e = &a.elements[0];
        let mut r = get_index(c, &array, i, v);
        let (t, val) = unary_expr(c, e, v);
        let op = Store(val, t.clone(), Rc::new(r.clone()));
        v.push(Inst::new(op, t));

        let mut index = (
            dtype.get_array_to_pointer(),
            Value::Reg(r)
        );

        for i in 1..len {
            let e = &a.elements[i];
            r = offset(c, &index, Type::Int(64), Value::Uint(1), v);
            let (t, val) = unary_expr(c, e, v);
            let op = Store(val, t.clone(), Rc::new(r.clone()));
            v.push(Inst::new(op, t));

            index.1 = Value::Reg(r);
        }
    }

    (Type::Pointer(Box::new(dtype)), Value::Reg(r))
}
