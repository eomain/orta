
use crate::Error;
use crate::PResult;
use crate::ParseInfo;
use crate::token_or;
use super::*;
use libtoken::Token;
use libtoken::IntoToken;
use libtoken::Key;
use libast::DataType;
use libast::DataRecord;

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
}