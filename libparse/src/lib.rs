
extern crate libast;
extern crate libtoken;

use std::fmt;
use libtoken::Token;
use libast::TokenStream;
use libast::SyntaxTree;

#[macro_export]
macro_rules! token {
    ($x: expr, $y: expr) => {
        {
            if let Some(token) = $y {
                if *token == $x {
                    Ok(token)
                } else {
                    Err(Error::Token($x))
                }
            } else {
                Err(Error::Token($x))
            }
        }
    };
}

#[macro_export]
macro_rules! token_is {
    ($x: expr, $parser: expr) => {
        {
            if let Some(token) = $parser.look() {
                if *token == $x {
                    $parser.next();
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }
    };
}

#[macro_export]
macro_rules! parser_error {
    ($x: expr, $y: expr) => {

    };
}

#[macro_export]
macro_rules! token_assert {
    ($x: expr, $y: expr) => {
        assert_eq!($y, Some(&$x));
    };
}

mod ll;

pub struct ParseInfo {
    tokens: TokenStream,
    index: usize
}

impl ParseInfo {
    fn new(tokens: TokenStream) -> Self
    {
        Self {
            tokens,
            index: 0
        }
    }

    fn next(&mut self) -> Option<&Token>
    {
        let index = self.index;
        let token = self.tokens.get(index);
        if let Some(_) = token {
            self.index += 1;
        }
        token
    }

    fn look(&self) -> Option<&Token>
    {
        self.tokens.get(self.index)
    }

    fn peek(&self) -> Option<&Token>
    {
        self.tokens.get(self.index + 1)
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    Custom(String),
    Found(Token),
    Token(Token)
}

impl From<&str> for Error {
    fn from(s: &str) -> Self
    {
        Error::Custom(s.into())
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        use Error::*;
        write!(f, "{}", match self {
            Custom(s) => s,
            _ => "syntax"
        })
    }
}

pub type PResult<T> = Result<T, Error>;

fn token_or(o: Option<&Token>, t: Token) -> PResult<&Token>
{
    if let Some(token) = o {
        if *token == t {
            return Ok(token);
        }
    }
    Err(Error::Token(t))
}

mod arithmetic {

    use libtoken::ArithmeticOperator as AOp;

    fn order(op: AOp) -> usize
    {
        use AOp::*;

        match op {
            Add => 2,
            Sub => 1,
            Mul => 3,
            Div => 5,
            Mod => 4
        }
    }

    pub fn precedence(a: AOp, b: AOp) -> bool
    {
        if order(a) >= order(b) {
            true
        } else {
            false
        }
    }

    #[test]
    fn tests() {
        use AOp::*;

        assert_eq!(precedence(Add, Mul), false);
        assert_eq!(precedence(Sub, Mod), false);
        assert_eq!(precedence(Div, Mul), true);
    }
}

pub fn construct(stream: TokenStream) -> PResult<SyntaxTree>
{
    let mut info;
    info = ParseInfo::new(stream);
    ll::main(&mut info)
}

#[cfg(test)]
mod tests {

}
