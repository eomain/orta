
mod array;

use super::*;
use ast::ComplexLiteral as Complex;
use array::*;
pub use array::index;

pub fn complex(c: &mut Context, e: &Complex,
               v: &mut Vec<Inst>) -> (Type, Value)
{
    use Complex::*;
    match e {
        Array(a) => array(c, a, v),
        _ => unimplemented!()
    }
}
