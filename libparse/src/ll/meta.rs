
use crate::Error;
use crate::PResult;
use crate::ParseInfo;
use crate::token_or;
use super::*;
use fun::rtype;
use fun::param;
use libtoken::Token;
use libtoken::IntoToken;
use libtoken::Key;
use libast::DataType;
use libast::Types;
use libast::Unique;
use libast::DataRecord;
use libast::Method;
use libast::MethodAccess;
use libast::FieldAccess;

pub fn unique(info: &mut ParseInfo) -> PResult<(DataType, String)>
{
    token!(Token::Keyword(Key::Unique), info.next())?;
    let name = id(info)?;
    token!(Token::Assign, info.next())?;
    let dtype = types(info)?;
    token!(Token::Semi, info.next())?;
    Ok((DataType::Unique(Unique::new(&name, dtype)), name))
}

pub fn name(info: &mut ParseInfo) -> PResult<DataType>
{
    token!(Token::Keyword(Key::Type), info.next())?;
    let name = id(info)?;
    token!(Token::Assign, info.next())?;
    let dtype = types(info)?;
    token!(Token::Semi, info.next())?;
    Ok(DataType::Types(Types::new(&name, dtype)))
}

// Parse a single attribute
fn attr(info: &mut ParseInfo) -> PResult<(String, DataType)>
{
    let name = id(info)?;
    token!(Token::Colon, info.next())?;
    let dtype = types(info)?;
    Ok((name, dtype))
}

// Parse a sequence of attributes
fn attrs(info: &mut ParseInfo) -> PResult<Vec<(String, DataType)>>
{
    token!(Token::Lbrace, info.next())?;
    let mut v = Vec::new();
    if Some(&Token::Rbrace) != info.look() {
        let p = attr(info)?;
        v.push(p);

        while Some(&Token::Comma) == info.look() {
            info.next();
            let p = attr(info)?;
            v.push(p);
        }
    }
    token!(Token::Rbrace, info.next())?;
    Ok(v)
}

pub fn structure(info: &mut ParseInfo) -> PResult<DataRecord>
{
    token!(Token::Keyword(Key::Type), info.next())?;
    let name = id(info)?;
    let attrs = attrs(info)?;
    Ok(DataRecord::new(name, attrs))
}

// Parse a sequence of method parameters
fn params(info: &mut ParseInfo) -> PResult<ParamList>
{
    token!(Token::Lparen, info.next())?;
    token!(Token::At, info.next())?;
    let mut params = ParamList::new();
    if Some(&Token::Rparen) != info.look() {
        token!(Token::Comma, info.next())?;
        let p = param(info)?;
        params.add(&p.0, p.1);

        while Some(&Token::Comma) == info.look() {
            info.next();
            let p = param(info)?;
            params.add(&p.0, p.1);
        }
    }
    token!(Token::Rparen, info.next())?;
    Ok(params)
}

pub fn method(info: &mut ParseInfo) -> PResult<Method>
{
    let name = id(info)?;
    let param = params(info)?;
    let rtype = rtype(info)?;
    let expr = if token_is!(Token::Assign, info) {
        let expr = vec![expr(info)?];
        token!(Token::Semi, info.next())?;
        expr
    } else {
        exprs(info)?
    };

    Ok(Method::new(&name, param, rtype, expr))
}

pub fn field_access(info: &mut ParseInfo, e: Expr) -> PResult<FieldAccess>
{
    let field = id(info)?;
    Ok(FieldAccess::new(&field, e))
}

pub fn method_access(info: &mut ParseInfo, e: Expr) -> PResult<MethodAccess>
{
    let call = call(info)?;
    Ok(MethodAccess::new(call, e))
}

pub fn access(info: &mut ParseInfo, e: Expr) -> PResult<Expr>
{
    token!(Token::Period, info.next())?;
    match info.look() {
        Some(&Token::Symbol(_)) => (),
        _ => return Err("expected identifier".into())
    }

    match info.peek() {
        Some(&Token::Lparen) => Ok(Expr::Method(method_access(info, e)?)),
        _ => Ok(Expr::Field(field_access(info, e)?)),
        //_ => Err("expected either '.' or '('".into())
    }
}

pub fn define(info: &mut ParseInfo) -> PResult<()>
{
    token!(Token::Keyword(Key::Define), info.next())?;
    let name = id(info)?;
    token!(Token::Lbrace, info.next())?;

    let mut methods = Vec::new();
    {
        while Some(&Token::Rbrace) != info.look() {
            methods.push(method(info)?);
        }
    }

    token!(Token::Rbrace, info.next())?;

    Ok(())
}

#[cfg(test)]
mod tests {

    extern crate liblex;

    use super::*;
    use libast::DataType::Integer;
    use libast::IntType::S64;

    #[test]
    fn structure_test()
    {
        let tokens = liblex::scan(r#"
            type Point {
                x: int,
                y: int
            }
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let s = structure(&mut info).unwrap();

        assert_eq!(s.name, String::from("Point"));
        assert_eq!(s.attr, vec![
            ("x".into(), Integer(S64)),
            ("y".into(), Integer(S64))
        ]);
    }

    #[test]
    fn method_test()
    {
        let tokens = liblex::scan(r#"
            pass(@, x: int): int {
                return x;
            }
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let m = method(&mut info).unwrap();

        let tokens = liblex::scan(r#"
            pass(@, x: int): int = return x;
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let m = method(&mut info).unwrap();
    }

    #[test]
    fn field_access_test()
    {
        let tokens = liblex::scan(r#"
            @.foo.chain().method.calls()
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let a = expr_value(&mut info).unwrap();
        println!("{:?}", a);
    }

    #[test]
    fn method_access_test()
    {
        let tokens = liblex::scan(r#"
            point.invoke(a, b * 2).frame
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let a = expr_value(&mut info).unwrap();
        println!("{:?}", a);
    }

    #[test]
    fn define_test()
    {
        let tokens = liblex::scan(r#"
            define Point {
                x(@): int = return x;
                y(@): int = return y;

                mul(@): int {
                    return x * y;
                }
            }
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let d = define(&mut info).unwrap();
    }

    #[test]
    fn name_test()
    {
        let tokens = liblex::scan(r#"
            type Object = ^i32;
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let n = name(&mut info).unwrap();

        println!("{}", n);
        println!("{}", n.derived());
    }

    #[test]
    fn unique_test()
    {
        let tokens = liblex::scan(r#"
            unique Window = ^i32;
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let (u, _) = unique(&mut info).unwrap();

        println!("{}", u);
        println!("{}", u.derived());
    }
}
