
use super::*;
use libast::ExprList;

#[inline]
fn expr(i: &mut Info, s: &mut Scope, e: &Expr) -> bool
{
    match e {
        Expr::Return(_) => true,
        Expr::If(br) => cond(i, s, br),
        _ => false
    }
}

#[inline]
fn exprs(i: &mut Info, s: &mut Scope, exprs: &ExprList) -> bool
{
    for e in exprs {
        if expr(i, s, e) {
            return true;
        }
    }
    false
}

fn cond(i: &mut Info, s: &mut Scope, br: &IfExpr) -> bool
{
    if let Some(other) = &br.other {
        if !exprs(i, s, &br.expr) {
            return false;
        }
        exprs(i, s, other)
    } else {
        false
    }
}

pub fn returns(i: &mut Info, s: &mut Scope, f: &Function) -> bool
{
    if i.ret == DataType::Unit {
        return true;
    }

    if let Some(expr) = f.expr.last() {
        return match expr {
            Expr::Return(_) => true,
            Expr::If(br) => cond(i, s, br),
            _ => false
        };
    }

    false
}
