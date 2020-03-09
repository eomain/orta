
use crate::arithmetic::precedence;
use crate::ParseInfo;
use super::*;
use libtoken::Token;
use libtoken::Operator;
use libtoken::ArithmeticOperator as AOp;
use libtoken::LogicalOperator as LOp;
use libtoken::RelationalOperator as ROp;
use libast::Literal;
use libast::Variable;
use libast::Value;
use libast::Expr;
use libast::BoolExpr;
use libast::BinaryExpr;
use libast::LogicalExpr;
use libast::CompExpr;

pub fn bexpr(info: &mut ParseInfo) -> PResult<BoolExpr>
{
    match info.look() {
        None => Err(Error::from("expected boolean expression")),
        Some(_) => Ok(BoolExpr::Expr(Box::new(expr(info)?)))
    }
}

fn bin_from(e1: Expr, e2: Expr, op: AOp) -> BinaryExpr
{
    let mut e1 = Box::new(e1);
    let mut e2 = Box::new(e2);
    use AOp::*;
    match op {
        Add => BinaryExpr::Add(e1, e2),
        Sub => BinaryExpr::Sub(e1, e2),
        Mul => BinaryExpr::Mul(e1, e2),
        Div => BinaryExpr::Div(e1, e2),
        Mod => BinaryExpr::Mod(e1, e2)
    }
}

pub fn bin(info: &mut ParseInfo, e: Expr, op: AOp) -> PResult<BinaryExpr>
{
    let mut e2 = expr_value(info)?;

    if let Some(nop) = aop(info) {
        info.next();
        if precedence(nop, op) {
            e2 = Expr::Binary(bin(info, e2, nop)?);
            Ok(bin_from(e, e2, op))
        } else {
            let e1 = Expr::Binary(bin_from(e, e2, op));
            Ok(bin(info, e1, nop)?)
        }
    } else {
        Ok(bin_from(e, e2, op))
    }
}

pub fn cmp(info: &mut ParseInfo, e: Expr, op: ROp) -> PResult<CompExpr>
{
    let e2 = Box::new(expr(info)?);
    let e1 = Box::new(e);
    use ROp::*;
    Ok(match op {
        Eq => CompExpr::Eq(e1, e2),
        Ne => CompExpr::Ne(e1, e2),
        Gt => CompExpr::Gt(e1, e2),
        Lt => CompExpr::Lt(e1, e2),
        Ge => CompExpr::Ge(e1, e2),
        Le => CompExpr::Le(e1, e2)
    })
}

#[cfg(test)]
mod tests {
    extern crate liblex;

    use super::*;
    use crate::ParseInfo;
    use libtoken::Token;
    use libtoken::Key;
    use libast::DataType::*;
    use libast::IntType::*;

    #[test]
    fn bin_test()
    {
        extern crate liblex;
        {
            let tokens = liblex::scan(r#"1 * 2 + 3"#.chars().collect()).unwrap();

            let mut info = ParseInfo::new(tokens);
            let expr = expr(&mut info).unwrap();
            println!("{:?}", expr);
        }

        let tokens = liblex::scan(r#"1 * 2 / 3 % 6"#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let expr = expr(&mut info).unwrap();
        println!("{:?}", expr);
    }

    #[test]
    fn cmp_test()
    {
        extern crate liblex;

        let tokens = liblex::scan(r#"1 == 1"#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let expr = expr(&mut info).unwrap();
        println!("{:?}", expr);
    }
}
