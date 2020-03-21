
mod array;
mod branch;
mod expr;
mod fun;
mod iter;
mod meta;
mod slice;

use std::rc::Rc;
use super::arithmetic::precedence;
use super::Error;
use super::PResult;
use super::ParseInfo;
use libtoken::Token;
use libtoken::IntoToken;
use libtoken::Key;
use libtoken::Prim;
use libtoken::Operator;
use libtoken::Operator::Bitwise;
use libtoken::ArithmeticOperator as AOp;
use libtoken::LogicalOperator as LOp;
use libtoken::RelationalOperator as ROp;
use libtoken::BitwiseOperator as BOp;
use libast::Literal;
use libast::Variable;
use libast::Value;
use libast::Assign;
use libast::{ Expr, ExprList, Cast, AtExpr, Address };
use libast::{ BinaryExpr, CallExpr, Return };
use libast::DataType;
use libast::{ IntType, FloatType };
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

fn unit(info: &mut ParseInfo) -> PResult<DataType>
{
    token!(Token::Lparen, info.next())?;
    token!(Token::Rparen, info.next())?;
    Ok(DataType::Unit)
}

#[test]
fn unit_test()
{
    extern crate liblex;
    use DataType::*;

    let tokens = liblex::scan(r#"()"#.chars().collect()).unwrap();

    let mut info = ParseInfo::new(tokens);
    let u = unit(&mut info).unwrap();
    assert_eq!(u, DataType::Unit);
    println!("{:?}", u);
}

// Transforms token into the respective data type
fn type_name(info: &mut ParseInfo) -> PResult<DataType>
{
    let msg = "expected type";
    match info.look() {
        Some(&Token::Lparen) => return Ok(unit(info)?),
        Some(&Token::Lsqr) => return Ok(array::array(info)?),
        Some(&Token::Operator(Bitwise(BOp::Or))) => {
            return Ok(slice::slice(info)?);
        },
        _ => ()
    }

    let token = info.next()
                    .ok_or(Error::from(msg))?;
    Ok(match token {
        Token::Primitive(p) => {
            match p {
                Prim::U8 => DataType::Integer(IntType::U8),
                Prim::U16 => DataType::Integer(IntType::U16),
                Prim::U32 => DataType::Integer(IntType::U32),
                Prim::U64 |
                Prim::UInt => DataType::Integer(IntType::U64),
                Prim::S8 => DataType::Integer(IntType::S8),
                Prim::S16 => DataType::Integer(IntType::S16),
                Prim::S32 => DataType::Integer(IntType::S32),
                Prim::S64 |
                Prim::SInt => DataType::Integer(IntType::S64),
                Prim::F32 => DataType::Float(FloatType::F32),
                Prim::F64 => DataType::Float(FloatType::F64),
                Prim::Bool => DataType::Boolean,
                Prim::Char => DataType::Char,
                Prim::String => DataType::String
            }
        },
        Token::Symbol(s) => {
            DataType::Named(s.into())
        },

        _ => return Err(Error::from(msg))
    })
}

fn fun_ptr(info: &mut ParseInfo) -> PResult<DataType>
{
    let mut v = Vec::new();
    let dtype = type_name(info)?;
    token!(Token::Arrow, info.next())?;
    if dtype != DataType::Unit {
        v.push(dtype);
        while let Some(&Token::Arrow) = info.peek() {
            let dtype = type_name(info)?;
            if dtype == DataType::Unit {
                return Ok(DataType::Function(v, Box::new(dtype)));
            }
            v.push(dtype);
            info.next();
        }
    }
    let r = type_name(info)?;
    Ok(DataType::Function(v, Box::new(r)))
}

#[test]
fn fun_ptr_test()
{
    extern crate liblex;
    use DataType::*;
    use IntType::*;

    let tokens = liblex::scan(r#"i64 -> i64 -> i64"#.chars().collect()).unwrap();

    let mut info = ParseInfo::new(tokens);
    let fptr = fun_ptr(&mut info).unwrap();
    let v = vec![
        Integer(S64), Integer(S64)
    ];
    let r = Box::new(Integer(S64));
    assert_eq!(fptr, Function(v, r));
    println!("{:?}", fptr);

    let tokens = liblex::scan(r#"() -> ()"#.chars().collect()).unwrap();

    let mut info = ParseInfo::new(tokens);
    let fptr = fun_ptr(&mut info).unwrap();
    let v = Vec::new();
    let r = Box::new(Unit);
    assert_eq!(fptr, Function(v, r));
    println!("{:?}", fptr);
}

fn cast(info: &mut ParseInfo) -> PResult<Cast>
{
    token!(Token::Lparen, info.next())?;
    let dtype = types(info)?;
    token!(Token::Rparen, info.next())?;
    let expr = if token_is!(Token::Lparen, info) {
        let e = expr(info)?;
        token!(Token::Rparen, info.look())?;
        e
    } else {
        expr_value(info)?
    };
    Ok(Cast::new(expr, dtype))
}

#[test]
fn cast_test()
{
    extern crate liblex;
    use DataType::*;
    use IntType::*;

    let tokens = liblex::scan(r#"(i64) a"#.chars().collect()).unwrap();

    let mut info = ParseInfo::new(tokens);
    let c = cast(&mut info).unwrap();
    println!("{:?}", c);

    let tokens = liblex::scan(r#"(i64) (2 * a)"#.chars().collect()).unwrap();

    let mut info = ParseInfo::new(tokens);
    let c = cast(&mut info).unwrap();
    println!("{:?}", c);
}

// Transforms token into the respective data type
fn types(info: &mut ParseInfo) -> PResult<DataType>
{
    use Operator::*;
    use BOp::Xor;
    let mut pcount = 0;
    while let Some(&Token::Operator(Bitwise(Xor))) = info.look() {
        pcount += 1;
        info.next();
    }

    let mut dtype = match (info.look(), info.peek()) {
        (_, Some(&Token::Arrow)) => fun_ptr(info)?,
        (Some(&Token::Lparen), Some(&Token::Rparen)) => {
            fun_ptr(info)?
        },
        _ => type_name(info)?
    };

    if pcount > 0 {
        let mut p = DataType::Pointer(Rc::new(dtype));
        for i in 1..pcount {
            p = DataType::Pointer(Rc::new(p));
        }
        dtype = p;
    }

    Ok(dtype)
}

// Obtains a value from a variable or literal
fn value(info: &mut ParseInfo) -> PResult<Value>
{
    let msg = "expected variable or literal";

    match info.peek() {
        Some(&Token::Lbrace) => {
            return Ok(Value::from(meta::struct_literal(info)?));
        },
        _ => ()
    }
    let mut token = info.next()
                    .ok_or(Error::from(msg))?;

    match token {
        Token::Literal(l) => Ok(Value::Literal(l.clone(), DataType::Unset)),
        Token::Symbol(s) => Ok(Value::Variable(Variable::new(s))),
        Token::Keyword(k) => match k {
            Key::True => Ok(Value::Literal(Literal::Boolean(true), DataType::Unset)),
            Key::False => Ok(Value::Literal(Literal::Boolean(false), DataType::Unset)),
            _ => Err(Error::from(msg))
        },
        Token::Operator(Operator::Arithmetic(AOp::Sub)) => {
            match info.next().ok_or(Error::from(msg))? {
                Token::Literal(Literal::Unsigned(u)) => {
                    return Ok(Value::Literal(Literal::Signed(-(*u as isize)), DataType::Unset));
                },
                _ => ()
            }
            Err(Error::from(msg))
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

#[inline]
fn cop(info: &mut ParseInfo) -> Option<ROp>
{
    if let Some(token) = info.look() {
        if let Token::Operator(op) = token {
            if let Operator::Relational(op) = op {
                return Some(*op);
            }
        }
    }
    None
}

#[inline]
fn lop(info: &mut ParseInfo) -> Option<LOp>
{
    if let Some(token) = info.look() {
        if let Token::Operator(op) = token {
            if let Operator::Logical(op) = op {
                return Some(*op);
            }
        }
    }
    None
}

fn at(info: &mut ParseInfo) -> PResult<Expr>
{
    token!(Token::At, info.next())?;
    let at = Expr::At(AtExpr::new());
    match info.look() {
        Some(&Token::Period) => Ok(meta::access(info, at)?),
        _ => Ok(at)
    }
}

#[test]
fn at_test()
{
    extern crate liblex;

    let tokens = liblex::scan(r#"
        @.get(true)
    "#.chars().collect()).unwrap();

    let mut info = ParseInfo::new(tokens);
    let at = at(&mut info).unwrap();
}

fn address(info: &mut ParseInfo) -> PResult<Expr>
{
    token!(Token::Operator(Bitwise(BOp::And)), info.next())?;
    let id = match info.next() {
        Some(Token::Symbol(s)) => s,
        _ => return Err(Error::from("expected identifier"))
    };
    Ok(Expr::Address(Address::new(id)))
}

#[test]
fn address_test()
{
    extern crate liblex;

    let tokens = liblex::scan(r#"
        &data
    "#.chars().collect()).unwrap();

    let mut info = ParseInfo::new(tokens);
    let a = address(&mut info).unwrap();
}

fn expr_value(info: &mut ParseInfo) -> PResult<Expr>
{
    let msg = "expected expression";
    let token = info.look()
                    .ok_or(Error::from(msg))?;
    let mut e = match token {
        Token::Lsqr => Ok(Expr::Value(Value::from(array::literal(info)?))),
        Token::At => Ok(at(info)?),
        Token::Symbol(_) => {
            if Some(&Token::Lparen) == info.peek() {
                Ok(Expr::Call(call(info)?))
            } else {
                Ok(Expr::Value(value(info)?))
            }
        },
        Token::Literal(_) |
        Token::Operator(Operator::Arithmetic(AOp::Sub)) => {
            Ok(Expr::Value(value(info)?))
        }
        Token::Keyword(k) => {
            match k {
                Key::If => Ok(Expr::If(branch::conditional(info)?)),
                Key::Loop => Ok(Expr::Loop(iter::loops(info)?)),
                Key::While => Ok(Expr::While(iter::loop_while(info)?)),
                Key::True | Key::False => Ok(Expr::Value(value(info)?)),
                Key::Return => Ok(Expr::Return(ret(info)?)),
                Key::Let => Ok(Expr::Assign(Box::new(assign_let(info)?))),
                _ => Err(Error::from(msg))
            }
        },
        Token::Operator(Bitwise(BOp::And)) => Ok(address(info)?),
        _ => Err(Error::from(msg))
    }?;

    loop {
        match info.look() {
            Some(&Token::Period) => {
                e = meta::access(info, e)?;
            },
            Some(&Token::Lsqr) => {
                e = array::index(info, e)?;
            },
            _ => break
        }
    }

    Ok(e)
}

fn cexpr(info: &mut ParseInfo) -> PResult<Expr>
{
    let mut e = {
        if let Some(&Token::Lparen) = info.look() {
            Expr::Cast(cast(info)?)
        } else {
            expr_value(info)?
        }
    };

    if let Some(op) = aop(info) {
        info.next();
        e = Expr::Binary(expr::bin(info, e, op)?);
    }

    if let Some(op) = cop(info) {
        info.next();
        e = Expr::Comp(expr::cmp(info, e, op)?);
    }
    Ok(e)
}

fn expr(info: &mut ParseInfo) -> PResult<Expr>
{
    let mut e = cexpr(info)?;

    if let Some(op) = lop(info) {
        info.next();
        e = Expr::Logical(expr::log(info, e, op)?);
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

#[inline]
fn semi(info: &mut ParseInfo) -> bool
{
    match info.look() {
        Some(&Token::Keyword(Key::If)) |
        Some(&Token::Keyword(Key::Loop)) |
        Some(&Token::Keyword(Key::While)) => false,
        _ => true
    }
}

fn sexpr(info: &mut ParseInfo) -> PResult<ExprList>
{
    let semi = semi(info);
    let e = expr(info)?;
    if semi {
        token!(Token::Semi, info.next())?;
    }

    return Ok(vec![e]);
}

fn exprs(info: &mut ParseInfo) -> PResult<ExprList>
{
    token!(Token::Lbrace, info.look())?;
    Ok(block(info, |i| {
        let mut e = Vec::new();
        while Some(&Token::Rbrace) != i.look() {
            let semi = semi(i);
            e.push(expr(i)?);
            if semi {
                token!(Token::Semi, i.next())?;
            }
        }
        Ok(e)
    })?)
}

fn block_exprs(info: &mut ParseInfo) -> PResult<ExprList>
{
    match info.look() {
        Some(&Token::Lbrace) => exprs(info),
        _ => sexpr(info)
    }
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
                    Key::Define => {
                        let (d, n) = meta::define(info)?;
                        tree.defines.insert(n, d);
                    },
                    Key::Extern |
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
                        if let Some(Token::Lbrace) = info.peek() {
                            let mut decs = fun::foreign_block(info)?;
                            while decs.len() > 0 {
                                tree.append_dec(decs.remove(0))
                            }
                        } else {
                            let dec = fun::foreign(info)?;
                            tree.append_dec(dec);
                        }
                    },
                    Key::Unique => {
                        let (unique, name) = meta::unique(info)?;
                        tree.append_types(&name, unique);
                    },
                    // TODO: error
                    _ => unimplemented!()
                }
            },
            // TODO: error
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
