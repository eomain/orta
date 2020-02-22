
use crate::arithmetic::precedence;
use crate::ParseInfo;
use libast::Literal;
use libast::Variable;
use libast::Value;
use libast::Expr;
use libast::BinaryExpr;
use libtoken::Token;
use libtoken::Operator;
use libtoken::ArithmeticOperator;

fn literal(literal: &Literal) -> Value
{
    Value::Literal(literal.clone())
}

fn variable(id: &str) -> Value
{
    Value::Variable(id.into())
}

fn value(token: &Token) -> Option<Value>
{
    Some(match token {
        Token::Literal(l) => literal(l),
        Token::Symbol(s) => variable(s),
        _ => return None
    })
}

fn check_operator(token: &Token) -> Option<ArithmeticOperator>
{
    if let Token::Operator(operator) = token {
        if let Operator::Arithmetic(operator) = operator {
            return Some(operator.clone());
        }
    }
    None
}

#[inline]
fn into_binary_expr(op: ArithmeticOperator, e1: Expr, e2: Expr) -> BinaryExpr
{
    let e1 = Box::new(e1);
    let e2 = Box::new(e2);

    match op {
        Add => BinaryExpr::Add(e1, e2),
        Sub => BinaryExpr::Sub(e1, e2),
        Mul => BinaryExpr::Mul(e1, e2),
        Div => BinaryExpr::Div(e1, e2),
        Mod => BinaryExpr::Mod(e1, e2)
    }
}

fn binary_expr(info: &mut ParseInfo, mut op: ArithmeticOperator,
               mut e1: Expr) -> Option<BinaryExpr>
{
    use ArithmeticOperator::*;

    let mut e2;
    let token = info.next()?;
    let ahead = info.look()?;
    let operator = check_operator(ahead);

    if let Some(op2) = operator {
        let nexpr = expr(info)?;
        if precedence(op, op2) {
            let bexpr = into_binary_expr(op, e1, nexpr);
            e1 = Expr::Binary(bexpr);
            op = op2;
            info.next();
            e2 = expr(info)?;
        } else {
            let bin = binary_expr(info, op2, nexpr)?;
            e2 = Expr::Binary(bin);
        }
    } else {
        e2 = expr(info)?;
    }

    /*let e2 = match check_operator(token) {
        None => match expr(info) {
            None => return None,
            Some(e) => Box::new(e)
        },

        Some(op2) => {
            if precedence(op, op2) {
                let e1 = Box::new(e);
                let e = Expr::BinaryExpr(
                    match op {
                        Add => BinaryExpr::Add(e1, e2),
                        Sub => BinaryExpr::Sub(e1, e2),
                        Mul => BinaryExpr::Mul(e1, e2),
                        Div => BinaryExpr::Div(e1, e2),
                        Mod => BinaryExpr::Mod(e1, e2)
                    }
                );
            } else {
                expr(info)
            }
        }
    };

    let e2 = match expr(info) {
        None => return None,
        Some(e) => Box::new(e)
    };*/

    Some(into_binary_expr(op, e1, e2))
}

fn expr(info: &mut ParseInfo) -> Option<Expr>
{
    let e = Expr::Value(match value(info.next()?) {
        None => return None,
        Some(v) => v
    });

    let operator = match info.look() {
        None => return Some(e),
        Some(o) => match check_operator(o) {
            None => return Some(e),
            Some(o) => o
        }
    };

    if let Some(binary) = binary_expr(info, operator, e) {
        return Some(Expr::Binary(binary));
    }

    None
}

#[test]
fn expr_test()
{
    use libtoken::TokenStream;
    use libtoken::IntoToken;
    use libtoken::ArithmeticOperator::*;

    let tokens: TokenStream = vec![
        2.token(), Mul.token(), 3.token(), Add.token(), 4.token()
    ];

    let mut parse = ParseInfo::new(tokens);

    println!("{:?}", expr(&mut parse));
}
