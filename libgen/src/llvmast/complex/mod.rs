
mod array;
mod structure;

use super::*;
use ast::Expr;
use ast::DataType;
use ast::ComplexLiteral as Complex;
use array::*;
pub use array::index;
use llvm::Operation::{
    Store, Load
};

fn first_class_type(d: &DataType) -> bool
{
    use ast::DataType::*;
    match d {
        Integer(_) | Float(_) | Boolean | Char | Pointer(_) => true,
        _ => false
    }
}

fn first_class(e: &Expr) -> bool
{
    first_class_type(e.get_type().derived())
}

pub fn get(c: &mut Context, d: &DataType, t: Type, reg: Register,
           v: &mut Vec<Inst>) -> Register
{
    if !first_class_type(d) {
        return reg;
    }
    let r = c.id.register();
    let op = Load(r.clone(), t.clone(), Rc::new(reg));
    v.push(Inst::new(op, t));
    r
}

fn set(c: &mut Context, e: &Expr, r: &Register,
       v: &mut Vec<Inst>)
{
    let store = if !first_class(e) {
        c.set_reg(r.clone(), false);
        false
    } else {
        true
    };
    let (t, val) = unary_expr(c, e, v);
    if store {
        let op = Store(val, t.clone(), Rc::new(r.clone()));
        v.push(Inst::new(op, t));
    } else {
        c.unset_reg();
    }
}

pub fn complex(c: &mut Context, e: &Complex,
               v: &mut Vec<Inst>) -> (Type, Value)
{
    use Complex::*;
    match e {
        Array(a) => array(c, a, v),
        Struct(s) => structure::structure(c, s, v)
    }
}
