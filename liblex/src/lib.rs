
extern crate libtoken;

use std::io::Read;

pub use libtoken::Token;
use libtoken::TokenStream;
use libtoken::IntoToken;
use libtoken::TryIntoToken;

// The type returned if there is an error
// found while lexing.
#[derive(Debug)]
pub enum Error {
    Invalid
}

struct Lexer<'a, 'b, I>
    where I: Iterator<Item=&'a char> {
    stream: &'b mut TokenStream<'a>,
    input: I
}

fn input<'a, 'b, I>(lex: &mut Lexer<'a, 'b, I>)
   where I: Iterator<Item=&'a char>
{
    
}

fn scan<'a, I>(input: I) -> Result<TokenStream<'a>, Error>
    where I: Iterator<Item=&'a char>
{
    let mut tokens = TokenStream::new();

    {
        let mut lexer = Lexer { stream: &mut tokens, input };

        let next = lexer.input.next();
        while let Some(c) = next {
            if c.is_whitespace() {
                continue;
            }

            if let Some(token) = c.token() {
                lexer.stream.push(token);
            }
        }
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    extern crate libtoken;

    use crate::*;
    use libtoken::*;
    use libtoken::Token::*;

    #[test]
    fn input_01()
    {
        let input: Vec<char> = "{}().,".chars().collect();
        let stream = scan(input.iter()).unwrap();
        let mut tokens = stream.iter();

        let some = [
            Some(&Lbrace), Some(&Rbrace),
            Some(&Lparen), Some(&Rparen),
            Some(&Period), Some(&Comma)
        ];

        for s in &some {
            assert_eq!(*s, tokens.next());
        }
    }
}
