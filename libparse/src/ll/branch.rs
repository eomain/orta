
use crate::Error;
use crate::PResult;
use crate::ParseInfo;
use crate::token_or;
use super::*;
use super::expr::bexpr;
use libtoken::Token;
use libtoken::IntoToken;
use libtoken::Key;
use libast::IfExpr;

// Parse an If expression
pub fn conditional(info: &mut ParseInfo) -> PResult<IfExpr>
{
    token!(Token::Keyword(Key::If), info.next())?;
    token!(Token::Lparen, info.next())?;
    let cond = bexpr(info)?;
    token!(Token::Rparen, info.next())?;

    let expr = exprs(info)?;
    let other = other(info)?;
    Ok(IfExpr::new(cond, expr, other))
}
// Parse a possible else branch
fn other(info: &mut ParseInfo) -> PResult<Option<ExprList>>
{
    if !token_is!(Token::Keyword(Key::Else), info) {
        return Ok(None);
    }
    Ok(Some(exprs(info)?))
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
    fn if_test()
    {
        let tokens = liblex::scan(r#"
            if (true) {

            } else {

            }
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let c = conditional(&mut info).unwrap();
        println!("{:?}", c);
    }
}
