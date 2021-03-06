
use crate::arithmetic::precedence;
use crate::ParseInfo;
use super::*;
use libtoken::Token;
use libtoken::Operator;
use libtoken::ArithmeticOperator as AOp;
use libtoken::LogicalOperator as LOp;
use libtoken::RelationalOperator as ROp;
use libtoken::BitwiseOperator as BOp;
use libtoken::AssignmentOperator as ASOp;
use libast::Literal;
use libast::Variable;
use libast::Value;
use libast::Expr;
use libast::BoolExpr;
use libast::BinaryExpr;
use libast::LogicalExpr;
use libast::BitExpr;
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

fn bit_from(e1: Expr, e2: Expr, op: BOp) -> BitExpr
{
    let mut e1 = Box::new(e1);
    let mut e2 = Box::new(e2);
    use BOp::*;
    match op {
        And => BitExpr::And(e1, e2),
        Or => BitExpr::Or(e1, e2),
        Xor => BitExpr::Xor(e1, e2),
        Lshift => BitExpr::Lsh(e1, e2),
        Rshift => BitExpr::Rsh(e1, e2),
        _ => unreachable!()
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
    let e2 = Box::new(cexpr(info)?);
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

pub fn log(info: &mut ParseInfo, e: Expr, op: LOp) -> PResult<LogicalExpr>
{
    let e1 = Box::new(e);
    use LOp::*;
    Ok(match op {
        And => LogicalExpr::And(e1, Box::new(expr(info)?)),
        Or => LogicalExpr::Or(e1, Box::new(expr(info)?)),
        Not => LogicalExpr::Not(e1)
    })
}

pub fn bit(info: &mut ParseInfo, e: Expr, op: BOp) -> PResult<BitExpr>
{
    let e1 = Box::new(e);
    use BOp::*;
    Ok(match op {
        And => BitExpr::And(e1, Box::new(expr(info)?)),
        Or => BitExpr::Or(e1, Box::new(expr(info)?)),
        Xor => BitExpr::Xor(e1, Box::new(expr(info)?)),
        Comp => BitExpr::Comp(e1),
        Lshift => BitExpr::Lsh(e1, Box::new(expr(info)?)),
        Rshift => BitExpr::Rsh(e1, Box::new(expr(info)?))
    })
}

pub fn asn(info: &mut ParseInfo, id: String, op: ASOp) -> PResult<Assign>
{
    let e1 = Expr::from(Variable::new(&id));
    info.next();
    info.next();
    let e2 = expr(info)?;
    use ASOp::*;
    let e = match op {
        Add => Expr::Binary(bin_from(e1, e2, AOp::Add)),
        Sub => Expr::Binary(bin_from(e1, e2, AOp::Sub)),
        Mul => Expr::Binary(bin_from(e1, e2, AOp::Mul)),
        Div => Expr::Binary(bin_from(e1, e2, AOp::Div)),
        Mod => Expr::Binary(bin_from(e1, e2, AOp::Mod)),
        And => Expr::Bit(bit_from(e1, e2, BOp::And)),
        Or => Expr::Bit(bit_from(e1, e2, BOp::Or)),
        Xor => Expr::Bit(bit_from(e1, e2, BOp::Xor)),
        Lshift => Expr::Bit(bit_from(e1, e2, BOp::Lshift)),
        Rshift => Expr::Bit(bit_from(e1, e2, BOp::Rshift)),
    };
    Ok(Assign::new(&id, DataType::Unset, e, false))
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
            let tokens = liblex::scan(r#"
                1 * 2 + 3
            "#.chars().collect()).unwrap();

            let mut info = ParseInfo::new(tokens);
            let expr = expr(&mut info).unwrap();
            println!("{:?}", expr);
        }

        let tokens = liblex::scan(r#"
            1 * 2 / 3 % 6
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let expr = expr(&mut info).unwrap();
        println!("{:?}", expr);
    }

    #[test]
    fn cmp_test()
    {
        extern crate liblex;

        let tokens = liblex::scan(r#"
            1 == 1
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let expr = expr(&mut info).unwrap();
        println!("{:?}", expr);
    }

    #[test]
    fn log_test()
    {
        extern crate liblex;

        let tokens = liblex::scan(r#"
            1 == 1 && true != false
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let e = expr(&mut info).unwrap();
        println!("{:?}", e);

        let tokens = liblex::scan(r#"
            !true
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let e = expr(&mut info).unwrap();
        println!("{:?}", e);
    }

    #[test]
    fn bit_test()
    {
        extern crate liblex;

        let tokens = liblex::scan(r#"
            a << b
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let expr = expr(&mut info).unwrap();
        println!("{:?}", expr);
    }

    #[test]
    fn asn_test()
    {
        extern crate liblex;

        let tokens = liblex::scan(r#"
            a += 5;
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let expr = expr(&mut info).unwrap();
        println!("{:?}", expr);
    }
}
