
use crate::Error;
use crate::PResult;
use crate::ParseInfo;
use crate::token_or;
use super::*;
use super::expr::bexpr;
use libtoken::Token;
use libtoken::IntoToken;
use libtoken::Key;
use libast::Loop;
use libast::WhileExpr;

// Parse an unconditional loop
pub fn loops(info: &mut ParseInfo) -> PResult<Loop>
{
    token!(Token::Keyword(Key::Loop), info.next())?;
    let expr = block_exprs(info)?;
    Ok(Loop::new(expr))
}

// Parse a while loop
pub fn loop_while(info: &mut ParseInfo) -> PResult<WhileExpr>
{
    token!(Token::Keyword(Key::While), info.next())?;
    token!(Token::Lparen, info.next())?;
    let cond = bexpr(info)?;
    token!(Token::Rparen, info.next())?;

    let expr = block_exprs(info)?;
    Ok(WhileExpr::new(cond, expr))
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
    fn loop_test()
    {
        let tokens = liblex::scan(r#"
            loop {
                run();
            }
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let l = loops(&mut info).unwrap();
        println!("{:?}", l);
    }

    #[test]
    fn while_test()
    {
        let tokens = liblex::scan(r#"
            while (true) {

            }
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let w = loop_while(&mut info).unwrap();
        println!("{:?}", w);
    }
}
