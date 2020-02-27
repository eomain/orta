
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
    if token_is!(Key::Pure.token(), info) {
        prop.pure();
    }
    prop
}

// Parse a function
pub fn function(info: &mut ParseInfo) -> PResult<Function>
{
    let prop = props(info);
    token!(Key::Fun.token(), info.next())?;

    let name = id(info)?;
    let param = params(info)?;

    let rtype = if Some(&Token::Colon) == info.look() {
        info.next();
        types(info)?
    } else {
        DataType::Unit
    };

    let expr = block(info, |i| {
        Ok(vec![])
    })?;

    Ok(Function::new(&name, prop, param, rtype, expr))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ParseInfo;
    use libtoken::Token;
    use libtoken::Key;

    #[test]
    fn fun()
    {
        let tokens = vec![
            Token::Keyword(Key::Pure),
            Token::Keyword(Key::Fun),
            Token::Symbol("main".into()),
            Token::Lparen,
            Token::Rparen,
            Token::Lbrace,
            Token::Rbrace
        ];

        let mut info = ParseInfo::new(tokens);
        let f = function(&mut info).unwrap();
        let p = &f.prop;
        assert_eq!("main", &f.name);
        assert_eq!(p.pure, true);
    }
}
