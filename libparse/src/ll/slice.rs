
use crate::Error;
use crate::PResult;
use crate::ParseInfo;
use crate::token_or;
use super::*;
use super::expr::bexpr;
use libtoken::Token;
use libtoken::IntoToken;
use libtoken::Key;
use libtoken::BitwiseOperator as BOp;
use libtoken::Operator::Bitwise;
use libast::Slice;

pub fn slice(info: &mut ParseInfo) -> PResult<DataType>
{
    token!(Token::Operator(Bitwise(BOp::Or)), info.next())?;
    let dtype = types(info)?;
    token!(Token::Operator(Bitwise(BOp::Or)), info.next())?;
    Ok(DataType::from(Slice::new(dtype)))
}

#[cfg(test)]
mod tests {
    extern crate liblex;

    use super::*;

    #[test]
    fn slice_test()
    {

        let tokens = liblex::scan(r#"
            |i8|
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let s = slice(&mut info).unwrap();
        println!("{}", s);
    }
}
