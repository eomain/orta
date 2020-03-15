
use crate::Error;
use crate::PResult;
use crate::ParseInfo;
use crate::token_or;
use super::*;
use libtoken::Token;
use libtoken::IntoToken;
use libtoken::Key;
use libast::Function;
use libast::FunctionProp;
use libast::FunctionDec;
use libast::ParamList;

// Parse a single function parameter
fn param(info: &mut ParseInfo) -> PResult<(String, DataType)>
{
    let name = id(info)?;
    token!(Token::Colon, info.next())?;
    let dtype = types(info)?;
    Ok((name, dtype))
}

// Parse a sequence of function parameters
fn params(info: &mut ParseInfo) -> PResult<ParamList>
{
    token!(Token::Lparen, info.next())?;
    let mut params = ParamList::new();
    if Some(&Token::Rparen) != info.look() {
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

// Parse function properties
fn props(info: &mut ParseInfo) -> FunctionProp
{
    let mut prop = FunctionProp::default();
    if token_is!(Key::Extern.token(), info) {
        prop.external();
    }
    if token_is!(Key::Pure.token(), info) {
        prop.pure();
    }
    prop
}

// Parse function return type
fn rtype(info: &mut ParseInfo) -> PResult<DataType>
{
    if Some(&Token::Colon) == info.look() {
        info.next();
        types(info)
    } else {
        Ok(DataType::Unit)
    }
}

// Parse a function
pub fn function(info: &mut ParseInfo) -> PResult<Function>
{
    let prop = props(info);
    token!(Key::Fun.token(), info.next())?;

    let name = id(info)?;
    let param = params(info)?;
    let rtype = rtype(info)?;
    let expr = exprs(info)?;

    Ok(Function::new(&name, prop, param, rtype, expr))
}

// Parse a sequence of function parameter declarations
fn params_dec(info: &mut ParseInfo) -> PResult<Vec<DataType>>
{
    token!(Token::Lparen, info.next())?;
    let mut v = Vec::new();
    if Some(&Token::Rparen) != info.look() {
        let t = types(info)?;
        v.push(t);

        while Some(&Token::Comma) == info.look() {
            info.next();
            let t = types(info)?;
            v.push(t);
        }
    }
    token!(Token::Rparen, info.next())?;
    Ok(v)
}

// Parse an external function declaration
pub fn foreign(info: &mut ParseInfo) -> PResult<FunctionDec>
{
    token!(Token::Keyword(Key::Foreign), info.next())?;
    token!(Key::Fun.token(), info.next())?;

    let name = id(info)?;
    let param = params_dec(info)?;
    let rtype = rtype(info)?;

    token!(Token::Semi, info.next())?;

    Ok(FunctionDec::new(&name, param, rtype))
}

// Parse an external function declaration
pub fn foreign_block(info: &mut ParseInfo) -> PResult<Vec<FunctionDec>>
{
    token!(Token::Keyword(Key::Foreign), info.next())?;
    token!(Token::Lbrace, info.next())?;

    let mut decs = Vec::new();
    {
        while Some(&Token::Rbrace) != info.look() {
            token!(Key::Fun.token(), info.next())?;

            let name = id(info)?;
            let param = params_dec(info)?;
            let rtype = rtype(info)?;

            token!(Token::Semi, info.next())?;

            let d = FunctionDec::new(&name, param, rtype);
            decs.push(d);
        }
    }

    token!(Token::Rbrace, info.next())?;
    Ok(decs)
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
    fn fun()
    {
        let tokens = liblex::scan(r#"
            pure fun main() {

            }
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let f = function(&mut info).unwrap();
        let p = &f.prop;
        assert_eq!("main", &f.name);
        assert_eq!(Unit, f.ret);
        assert_eq!(p.pure, true);
    }

    #[test]
    fn extern_fun()
    {
        let tokens = liblex::scan(r#"
            extern pure fun main() {

            }
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let f = function(&mut info).unwrap();
        let p = &f.prop;
        assert_eq!(p.external, true);
        assert_eq!(p.pure, true);
    }

    #[test]
    fn declare()
    {
        let tokens = liblex::scan(r#"
            foreign fun add(int, int): int;
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let f = foreign(&mut info).unwrap();
        assert_eq!("add", &f.name);
        assert_eq!(Integer(S64), f.ret);
    }

    #[test]
    fn dec_block()
    {
        let tokens = liblex::scan(r#"
            foreign {
                fun run(() -> ()): bool;
                fun add(int, int): int;
                fun main();
            }
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let f = foreign_block(&mut info).unwrap();
        println!("{:?}", f);
    }
}
