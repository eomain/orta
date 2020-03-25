
mod array;
mod structure;

use super::*;
use ast::Expr;
use ast::ComplexLiteral as Complex;
use array::*;
pub use array::index;
use llvm::Operation::{
    Store
};

fn set(c: &mut Context, e: &Expr, r: &Register,
       v: &mut Vec<Inst>)
{
    let (t, val) = unary_expr(c, e, v);
    let op = Store(val, t.clone(), Rc::new(r.clone()));
    v.push(Inst::new(op, t));
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
