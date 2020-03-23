
use super::*;
use ast::BitExpr;

fn ibit(c: &mut Context, b: &ast::BitExpr,
        v: &mut Vec<Inst>) -> (Operation, Type, Register)
{
    let id = c.id.register();
    use ast::BitExpr::*;
    match b {
        And(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            (Operation::And(id.clone(), e1, e2), t, id)
        },
        Or(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            (Operation::Or(id.clone(), e1, e2), t, id)
        },
        Xor(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            (Operation::Xor(id.clone(), e1, e2), t, id)
        },
        Comp(a) => {
            let (t, e1) = unary_expr(c, a, v);
            (Operation::Xor(id.clone(), e1, Value::Int(-1)), t, id)
        },
        Lsh(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            (Operation::Shl(id.clone(), e1, e2), t, id)
        },
        Rsh(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            if signed(&t) {
                (Operation::Ashr(id.clone(), e1, e2), t, id)
            } else {
                (Operation::Lshr(id.clone(), e1, e2), t, id)
            }
        }
    }
}

pub fn bit(c: &mut Context, b: &ast::BitExpr,
       v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    use ast::BitExpr::*;
    use ast::DataType::*;
    let (op, t, id) = match b.get_type() {
        ast::DataType::Integer(_) => ibit(c, b, v),
        _ => unreachable!()
    };

    v.push(Inst::new(op, t.clone()));
    Some(vec![(t, Value::Reg(id))])
}
