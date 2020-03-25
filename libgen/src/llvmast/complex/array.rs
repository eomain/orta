
use super::*;
use ast::Expr;
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

fn pointer(c: &mut Context, t: Type, val: Value,
           v: &mut Vec<Inst>) -> (Type, Value)
{
    let dtype = t.get_array_type();
    let tval = (t, val);
    let r = get_index(c, &tval, 0, v);
    (Type::Pointer(Box::new(dtype)), Value::Reg(r))
}

pub fn index(c: &mut Context, i: &Index,
             v: &mut Vec<Inst>) -> (Type, Value)
{
    let tval = unary_expr(c, &i.expr, v);
    let index = match &tval.0 {
        Type::Pointer(t) => {
            match **t {
                Type::Array(_, _) => pointer(c, (**t).clone(), tval.1, v),
                _ => tval
            }
        },
        Type::Array(_, _) => {
            let r = get_index(c, &tval, 0, v);
            (tval.0.get_array_to_pointer(), Value::Reg(r))
        },
        _ => unreachable!()
    };

    let (t, val) = unary_expr(c, &i.index, v);
    let r = offset(c, &index, t, val, v);
    let dtype = type_cast(&i.dtype);

    if dtype.array() || dtype.pointer() {
        return (dtype, Value::Reg(r))
    }
    let r = get(c, dtype.clone(), Rc::new(r), v);

    (dtype, Value::Reg(r))
}

#[inline]
fn get_array(e: &Expr) -> &Array
{
    use ast::Value::Complex;
    use ast::ComplexLiteral::Array;
    match e {
        Expr::Value(Complex(Array(a))) => a,
        _ => unreachable!()
    }
}

#[inline]
fn inc(c: &mut Context, o: &(Type, Value),
       v: &mut Vec<Inst>) -> Register
{
    offset(c, o, Type::Int(64), Value::Uint(1), v)
}

fn dimension(c: &mut Context, a: &Array, r: &Register,
             v: &mut Vec<Inst>)
{
    let dtype = type_cast(&a.dtype);
    let array = (dtype.clone(), Value::Reg(r.clone()));
    let mut r = get_index(c, &array, 0, v);

    if dtype.multi_array() {
        let e = get_array(&a.elements[0]);
        dimension(c, e, &r, v);

        for i in 1..a.elements.len() {
            let e = get_array(&a.elements[i]);
            let mut index = (
                dtype.get_array_to_pointer(),
                Value::Reg(r)
            );
            r = inc(c, &index, v);
            dimension(c, e, &r, v);
        }

    } else {
        let len = a.elements.len();
        if len > 0 {
            let e = &a.elements[0];
            set(c, e, &r, v);

            let mut index = (
                dtype.get_array_to_pointer(),
                Value::Reg(r)
            );

            for i in 1..len {
                let e = &a.elements[i];
                r = inc(c, &index, v);
                set(c, e, &r, v);

                index.1 = Value::Reg(r);
            }
        }
    }
}

pub fn array(c: &mut Context, a: &Array,
             v: &mut Vec<Inst>) -> (Type, Value)
{
    let dtype = type_cast(&a.dtype);

    let (r, alloc) = c.get_reg();
    let reg = Rc::new(r.clone());

    if alloc {
        let op = Alloca(reg, None);
        v.push(Inst::new(op, dtype.clone()));
    }
    dimension(c, a, &r, v);

    (Type::Pointer(Box::new(dtype)), Value::Reg(r))
}
