
use crate::Error;
use crate::PResult;
use crate::ParseInfo;
use crate::token_or;
use super::*;
use libtoken::Token;
use libtoken::IntoToken;
use libtoken::Key;
use libast::DataType;
use libast::Types;
use libast::Unique;
use libast::DataRecord;

pub fn unique(info: &mut ParseInfo) -> PResult<DataType>
{
    token!(Token::Keyword(Key::Unique), info.next())?;
    let name = id(info)?;
    token!(Token::Assign, info.next())?;
    let dtype = types(info)?;
    token!(Token::Semi, info.next())?;
    Ok(DataType::Unique(Unique::new(name, dtype)))
}

pub fn name(info: &mut ParseInfo) -> PResult<DataType>
{
    token!(Token::Keyword(Key::Type), info.next())?;
    let name = id(info)?;
    token!(Token::Assign, info.next())?;
    let dtype = types(info)?;
    token!(Token::Semi, info.next())?;
    Ok(DataType::Types(Types::new(name, dtype)))
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
        let u = unique(&mut info).unwrap();

        println!("{}", u);
        println!("{}", u.derived());
    }
}
