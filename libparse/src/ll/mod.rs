
mod branch;
mod expr;
mod fun;
mod iter;
mod meta;

use super::arithmetic::precedence;
use super::Error;
use super::PResult;
use super::ParseInfo;
use libtoken::Token;
use libtoken::Key;
use libtoken::Prim;
use libtoken::Operator;
use libtoken::ArithmeticOperator as AOp;
use libast::Literal;
use libast::Variable;
use libast::Value;
use libast::Assign;
use libast::{ Expr, ExprList };
use libast::{ BinaryExpr, CallExpr, Return };
use libast::DataType;
use libast::IntType;
use libast::ParamList;
use libast::SyntaxTree;

// Converts an expected identifier into a `String`
fn id(info: &mut ParseInfo) -> PResult<String>
{
    let msg = "expected identifier";
    let err: Error = msg.into();
    let token = info.next().ok_or(err)?;
    if let Token::Symbol(id) = token {
        Ok(id.clone())
    } else {
        return Err(msg.into());
    }
}

// Transforms token into the respective data type
fn types(info: &mut ParseInfo) -> PResult<DataType>
{
    let msg = "expected type";
    let token = info.next()
                    .ok_or(Error::from(msg))?;
    Ok(match token {
        Token::Primitive(p) => {
            match p {
                Prim::U8 => DataType::Integer(IntType::U8),
                Prim::U16 => DataType::Integer(IntType::U16),
                Prim::U32 => DataType::Integer(IntType::U32),
                Prim::U64 => DataType::Integer(IntType::U64),
                Prim::S8 => DataType::Integer(IntType::S8),
                Prim::S16 => DataType::Integer(IntType::S16),
                Prim::S32 => DataType::Integer(IntType::S32),
                Prim::S64 => DataType::Integer(IntType::S64),
                Prim::Bool => DataType::Boolean,
                _ => unimplemented!()
            }
        },
        Token::Symbol(s) => {
            unimplemented!()
        },

        _ => return Err(Error::from(msg))
    })
}

// Obtains a value from a variable or literal
fn value(info: &mut ParseInfo) -> PResult<Value>
{
    let msg = "expected variable or literal";
    let token = info.next()
                    .ok_or(Error::from(msg))?;
    match token {
        Token::Literal(l) => Ok(Value::Literal(l.clone(), DataType::Unset)),
        Token::Symbol(s) => Ok(Value::Variable(Variable::new(s))),
        Token::Keyword(k) => match k {
            Key::True => Ok(Value::Literal(Literal::Boolean(true), DataType::Unset)),
            Key::False => Ok(Value::Literal(Literal::Boolean(false), DataType::Unset)),
            _ => Err(Error::from(msg))
        },
        _ => Err(Error::from(msg))
    }
}

fn assign_let(info: &mut ParseInfo) -> PResult<Assign>
{
    token!(Token::Keyword(Key::Let), info.next())?;
    let id = id(info)?;
    let dtype = if token_is!(Token::Colon, info) {
        types(info)?
    } else {
        DataType::Unset
    };
    token!(Token::Assign, info.next())?;
    let expr = expr(info)?;
    Ok(Assign::new(&id, dtype, expr))
}

#[test]
fn assign_let_test()
{
    extern crate liblex;

    let tokens = liblex::scan(r#"let x = "test""#.chars().collect()).unwrap();

    let mut info = ParseInfo::new(tokens);
    let assign = assign_let(&mut info).unwrap();

    assert_eq!(assign.id, String::from("x"));
    let val = Value::Literal(Literal::String("test".into()), DataType::Unset);
    assert_eq!(assign.expr, Expr::Value(val));
}

// A function call expression of the form `<id>(<expr>, <expr>, ...)`
fn call(info: &mut ParseInfo) -> PResult<CallExpr>
{
    let name = id(info)?;
    token!(Token::Lparen, info.next())?;
    let exprs = expr_list(info, &Token::Rparen, Token::Comma)?;
    token!(Token::Rparen, info.next())?;
    Ok(CallExpr::new(&name, exprs))
}

#[test]
fn assign_call_test()
{
    extern crate liblex;

    let tokens = liblex::scan(r#"main("input")"#.chars().collect()).unwrap();

    let mut info = ParseInfo::new(tokens);
    let call = call(&mut info).unwrap();

    assert_eq!(call.name, String::from("main"));
    let val = Value::Literal(Literal::String("input".into()), DataType::Unset);
    assert_eq!(call.args[0], Expr::Value(val));
}

fn ret(info: &mut ParseInfo) -> PResult<Return>
{
    token!(Token::Keyword(Key::Return), info.next())?;
    let expr = match info.look() {
        Some(&Token::Semi) | None => None,
        _ => Some(expr(info)?)
    };
    Ok(Return::new(expr))
}

#[test]
fn ret_test()
{
    extern crate liblex;

    let tokens = liblex::scan(r#"return"#.chars().collect()).unwrap();

    let mut info = ParseInfo::new(tokens);
    let ret = ret(&mut info).unwrap();
    assert_eq!(ret.expr, None);
    println!("{:?}", ret);
}

#[test]
fn ret_expr_test()
{
    extern crate liblex;

    let tokens = liblex::scan(r#"return true"#.chars().collect()).unwrap();

    let mut info = ParseInfo::new(tokens);
    let ret = ret(&mut info).unwrap();
    let val = Value::Literal(Literal::Boolean(true), DataType::Unset);
    assert_eq!(ret.expr, Some(Box::new(Expr::Value(val))));
    println!("{:?}", ret);
}

#[inline]
fn aop(info: &mut ParseInfo) -> Option<AOp>
{
    if let Some(token) = info.look() {
        if let Token::Operator(op) = token {
            if let Operator::Arithmetic(op) = op {
                return Some(*op);
            }
        }
    }
    None
}

fn expr(info: &mut ParseInfo) -> PResult<Expr>
{
    let msg = "expected expression";
    let token = info.look()
                    .ok_or(Error::from(msg))?;
    let e = match token {
        Token::Symbol(_) => {
            if Some(&Token::Lparen) == info.peek() {
                Ok(Expr::Call(call(info)?))
            } else {
                Ok(Expr::Value(value(info)?))
            }
        },
        Token::Literal(_) => {
            Ok(Expr::Value(value(info)?))
        }
        Token::Keyword(k) => {
            match k {
                Key::If => Ok(Expr::If(branch::conditional(info)?)),
                Key::True | Key::False => Ok(Expr::Value(value(info)?)),
                Key::Return => Ok(Expr::Return(ret(info)?)),
                Key::Let => Ok(Expr::Assign(Box::new(assign_let(info)?))),
                _ => Err(Error::from(msg))
            }
        },
        _ => Err(Error::from(msg))
    }?;

    if let Some(op) = aop(info) {
        info.next();
        return Ok(Expr::Binary(expr::bin(info, e, op)?));
    }

    Ok(e)
}

fn expr_list(info: &mut ParseInfo, until: &Token, sep: Token) -> PResult<ExprList>
{
    let mut e = Vec::new();
    while Some(until) != info.look() {
        e.push(expr(info)?);
        if Some(until) != info.look() {
            token!(sep.clone(), info.next())?;
        }
    }
    Ok(e)
}

fn exprs(info: &mut ParseInfo) -> PResult<ExprList>
{
    Ok(block(info, |i| {
        let mut e = Vec::new();
        while Some(&Token::Rbrace) != i.look() {
            let semi = match i.look() {
                Some(&Token::Keyword(Key::If)) |
                Some(&Token::Keyword(Key::While)) => false,
                _ => true
            };
            e.push(expr(i)?);
            if semi {
                token!(Token::Semi, i.next())?;
            }
        }
        Ok(e)
    })?)
}

// Parses a block with a pair of braces.
// A closure is used to determine what should
// be parsed within the block.
fn block<F, T>(info: &mut ParseInfo, f: F) -> PResult<T>
    where F: Fn(&mut ParseInfo) -> PResult<T>
{
    token!(Token::Lbrace, info.next())?;
    let r = f(info)?;
    token!(Token::Rbrace, info.next())?;
    Ok(r)
}

// The entry point of the parser.
// Being a ll recursive descent parser, the top-level
// language constructs are parsed first, in order.
pub fn main(info: &mut ParseInfo) -> PResult<SyntaxTree>
{
    let mut tree = SyntaxTree::new();

    while let Some(token) = info.look() {
        match token {
            Token::Keyword(k) => {
                match k {
                    Key::Pure |
                    Key::Fun => {
                        let f = fun::function(info)?;
                        tree.append(f);
                    },
                    Key::Type => {
                        let s = meta::structure(info)?;
                        tree.append_rec(s);
                    },
                    Key::Foreign => {
                        let f = fun::foreign(info)?;
                        tree.append_dec(f);
                    },
                    _ => unimplemented!()
                }
            },
            _ => unimplemented!()
        }
    }

    Ok(tree)
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
    fn prog_01()
    {
        let tokens = liblex::scan(r#"
            fun main() {
                if (true) {

                } else {

                }
            }
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let p = main(&mut info).unwrap();
        println!("{:?}", p);
    }
}
