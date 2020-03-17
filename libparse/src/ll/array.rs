
use crate::Error;
use crate::PResult;
use crate::ParseInfo;
use crate::token_or;
use super::*;
use super::expr::bexpr;
use libtoken::Token;
use libtoken::IntoToken;
use libtoken::Key;
use libast::Array;

fn size(info: &mut ParseInfo) -> PResult<usize>
{
    match info.next() {
        Some(&Token::Literal(Literal::Unsigned(u))) => Ok(u),
        _ => Err("expected size".into())
    }
}

pub fn array(info: &mut ParseInfo) -> PResult<DataType>
{
    token!(Token::Lsqr, info.next())?;
    let size = size(info)?;
    token!(Token::Rsqr, info.next())?;
    let dtype = types(info)?;
    Ok(DataType::from(Array::new(size, dtype)))
}

#[cfg(test)]
mod tests {
    extern crate liblex;

    use super::*;

    #[test]
    fn array_test()
    {

        let tokens = liblex::scan(r#"
            [5] |[2] ^int|
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let a = array(&mut info).unwrap();
        println!("{}", a);
    }
}
