
extern crate libtoken;

use std::io::Read;

pub use libtoken::Token;
use libtoken::TokenStream;
use libtoken::IntoToken;
use libtoken::TryIntoToken;
use libtoken::Key;
use libtoken::KeyToken;
use libtoken::ArithmeticOperator;

// The type returned if there is an error
// found while lexing.
#[derive(Debug)]
pub enum Error {
    Invalid,
    CommentEnd
}

#[inline]
fn alpha(c: char) -> bool
{
    c.is_ascii_alphanumeric()
}

#[inline]
fn numeric(c: char) -> bool
{
    c.is_ascii_digit()
}

fn comment(lexer: &mut Lexer)
{
    while let Some(c) = lexer.next() {
        if c == '*' {
            if lexer.check('/') {
                return;
            }
        }
    }
}

struct Lexer<'a> {
    stream: &'a mut TokenStream,
    input: Vec<char>,
    string: String,
    index: usize
}

impl<'a> Lexer<'a> {
    fn new(stream: &'a mut TokenStream, input: Vec<char>) -> Self
    {
        Self {
            stream,
            input,
            string: String::new(),
            index: 0
        }
    }

    #[inline]
    fn read(&mut self) -> Option<char>
    {
        match self.input.get(self.index) {
            None => None,
            Some(c) => Some(*c)
        }
    }

    fn next(&mut self) -> Option<char>
    {
        self.index += 1;
        self.read()
    }

    fn ahead(&mut self) -> Option<char>
    {
        match self.input.get(self.index + 1) {
            None => None,
            Some(c) => Some(*c)
        }
    }

    #[inline]
    fn check(&mut self, c: char) -> bool
    {
        if let Some(s) = self.ahead() {
            if s == c {
                self.next();
                return true;
            }
        }
        false
    }

    fn check_str(&mut self, s: &str) -> bool
    {
        for c in s.chars() {
            if !self.check(c) {
                return false;
            }
        }
        true
    }

    fn read_while<F>(&mut self, f: F)
        where F: Fn(char) -> bool
    {
        let c = self.read().unwrap();
        self.string.push(c);
        while let Some(c) = self.ahead() {
            if !f(c) {
                break;
            }
            self.string.push(c);
            self.next();
        }
    }

    fn accept(&mut self, token: Token)
    {
        self.stream.push(token);
        self.string.clear();
    }
}

/*fn input<'a, 'b, I>(lex: &mut Lexer<'a, 'b, I>)
   where I: Iterator<Item=&'a char>
{

}*/

static KEYWORDS: [(&str, Key); 8] = [
    ("fun", Key::Fun),
    ("pure", Key::Pure),
    ("if", Key::If),
    ("else", Key::Else),
    ("while", Key::While),
    ("for", Key::For),
    ("true", Key::True),
    ("false", Key::False)
];

#[test]
fn keyword_test()
{
    let mut tokens = TokenStream::new();
    let mut lexer = Lexer::new(&mut tokens, "if".chars().collect());
    keyword_rule(&mut lexer);

    assert_eq!(keyword(&mut lexer), Some(Token::Keyword(Key::If)));
}

fn keyword_rule(lexer: &mut Lexer)
{
    lexer.read_while(alpha);
}

fn keyword(lexer: &mut Lexer) -> Option<Token>
{
    let mut key = false;
    for k in &KEYWORDS {
        if k.0 == lexer.string {
            key = true;
            return Some(Token::Keyword(k.1));
        }
    }
    None
}

fn number(lexer: &mut Lexer) -> Token
{
    lexer.read_while(numeric);
    lexer.string.parse::<usize>().unwrap().token()
}

fn operator(lexer: &mut Lexer, c: char) -> Option<Token>
{
    use ArithmeticOperator::*;

    Some(match c {
        '+' => {
            if lexer.check('+') {
                unimplemented!()
            } else {
                Add.token()
            }
        },
        '-' => Sub.token(),
        '*' => Mul.token(),
        '/' => Div.token(),
        '%' => Mod.token(),
        _ => return None
    })
}

fn scan(input: Vec<char>) -> Result<TokenStream, Error>
{
    let mut tokens = TokenStream::new();

    {
        let mut lexer = Lexer::new(&mut tokens, input);
        let mut token;
        while let Some(c) = lexer.read() {
            token = None;
            if c.is_whitespace() {
                lexer.next();
                continue;
            }

            if c == '/' {
                if lexer.check('*') {
                    comment(&mut lexer);
                    lexer.next();
                    continue;
                }
            }

            token = Some(match c {
                '(' => Token::Lparen,
                ')' => Token::Rparen,
                '[' => Token::Lsqr,
                ']' => Token::Rsqr,
                '{' => Token::Lbrace,
                '}' => Token::Rbrace,
                '.' => Token::Period,
                ':' => Token::Colon,
                ';' => Token::Semi,
                ',' => Token::Comma,
                '+' | '-' | '*' | '/' | '%' => {
                    match operator(&mut lexer, c) {
                        None => return Err(Error::Invalid),
                        Some(c) => c
                    }
                },
                '0' => {
                    if let Some(c) = lexer.ahead() {
                        match c {
                            '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => return Err(Error::Invalid),
                            _ => ()
                        }
                    }
                    0.token()
                },
                '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => {
                    number(&mut lexer)
                },
                _ => {
                    if alpha(c) {
                        keyword_rule(&mut lexer);
                        match keyword(&mut lexer) {
                            None => lexer.string.token(),
                            Some(t) => t
                        }
                    } else {
                        return Err(Error::Invalid);
                    }
                }
            });

            if let Some(token) = token {
                lexer.accept(token);
            }
            lexer.next();
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
        let input: Vec<char> = "if{}().,".chars().collect();
        let stream = scan(input).unwrap();
        let mut tokens = stream.iter();

        let some = [
            Some(&Keyword(Key::If)),
            Some(&Lbrace), Some(&Rbrace),
            Some(&Lparen), Some(&Rparen),
            Some(&Period), Some(&Comma)
        ];

        for s in &some {
            assert_eq!(*s, tokens.next());
        }
    }

    static PROG: &str =
    r#"fun main() {
        /*if (true) {

        } else {

        }*/
    }"#;

    #[test]
    fn input_02()
    {
        let input: Vec<char> = PROG.chars().collect();
        let stream = scan(input).unwrap();
        println!("{:?}", stream);

        let sym = Symbol("main".into());

        let some = [
            Some(&Keyword(Key::Fun)),
            Some(&sym),
            Some(&Lparen), Some(&Rparen),
            Some(&Lbrace), Some(&Rbrace)
        ];

        let mut tokens = stream.iter();
        for s in &some {
            assert_eq!(*s, tokens.next());
        }
    }

    #[test]
    fn input_03()
    {
        let input = "0{}".chars().collect();
        let tokens = scan(input).unwrap();
        assert_eq!(tokens.get(0), Some(&0.token()));
    }
}
