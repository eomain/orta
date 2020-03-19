
extern crate libtoken;

mod error;

use std::fmt;
use std::io::Read;

pub use libtoken::Token;
use libtoken::TokenStream;
use libtoken::IntoToken;
use libtoken::TryIntoToken;
use libtoken::Key;
use libtoken::Prim;
use libtoken::ArithmeticOperator as AOp;
use libtoken::LogicalOperator as LOp;
use libtoken::RelationalOperator as ROp;
use libtoken::UnaryOperator as UOp;
use libtoken::BitwiseOperator as BOp;
pub use error::Error;

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

#[inline]
fn base_2(c: char) -> bool
{
    (c == '0' || c == '1')
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
    /* TODO: return error */
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cursor {
    pub line: usize,
    pub offset: usize,
    pub index: usize
}

impl Cursor {
    fn new() -> Self
    {
        Self {
            line: 1,
            offset: 1,
            index: 0
        }
    }
}

impl fmt::Display for Cursor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}:{}", self.line, self.offset)
    }
}

struct Lexer<'a> {
    stream: &'a mut TokenStream,
    input: Vec<char>,
    string: String,
    index: usize,
    cursor: Cursor
}

impl<'a> Lexer<'a> {
    fn new(stream: &'a mut TokenStream, input: Vec<char>) -> Self
    {
        Self {
            stream,
            input,
            string: String::new(),
            index: 0,
            cursor: Cursor::new()
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
        match self.read() {
            None => None,
            Some(c) => {
                if c == '\n' {
                    self.cursor.line += 1;
                    self.cursor.offset = 1;
                } else {
                    self.cursor.offset += 1;
                }
                Some(c)
            }
        }
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

    fn skip_while<F>(&mut self, f: F)
        where F: Fn(char) -> bool
    {
        let c = self.read().unwrap();
        while let Some(c) = self.ahead() {
            if !f(c) {
                break;
            }
            self.next();
        }
    }

    fn accept(&mut self, token: Token)
    {
        self.stream.push(token);
        self.string.clear();
    }
}

fn ident(lexer: &mut Lexer) -> Token
{
    lexer.read_while(|c| alpha(c) || numeric(c) || c == '_');
    lexer.string.token()
}

static KEYWORDS: [(&str, Key); 18] = [
    ("fun", Key::Fun),
    ("pure", Key::Pure),
    ("if", Key::If),
    ("else", Key::Else),
    ("while", Key::While),
    ("for", Key::For),
    ("true", Key::True),
    ("false", Key::False),
    ("return", Key::Return),
    ("let", Key::Let),
    ("var", Key::Var),
    ("type", Key::Type),
    ("foreign", Key::Foreign),
    ("extern", Key::Extern),
    ("break", Key::Break),
    ("unsafe", Key::Unsafe),
    ("unique", Key::Unique),
    ("define", Key::Define)
];

static KEYWORDS_PRIM: [(&str, Prim); 15] = [
    ("u8", Prim::U8),
    ("u16", Prim::U16),
    ("u32", Prim::U32),
    ("u64", Prim::U64),
    ("uint", Prim::U64),
    ("i8", Prim::S8),
    ("i16", Prim::S16),
    ("i32", Prim::S32),
    ("i64", Prim::S64),
    ("int", Prim::S64),
    ("f32", Prim::F32),
    ("f64", Prim::F64),
    ("bool", Prim::Bool),
    ("char", Prim::Char),
    ("string", Prim::String)
];

#[test]
fn keyword_test()
{
    let mut tokens = TokenStream::new();
    let mut lexer = Lexer::new(&mut tokens, "if".chars().collect());
    keyword_rule(&mut lexer);

    assert_eq!(keyword(&mut lexer), Some(Token::Keyword(Key::If)));
}

fn keyword_rule(lexer: &mut Lexer) -> bool
{
    lexer.read_while(alpha);
    if let Some(c) = lexer.ahead() {
        if c == '_' || numeric(c) {
            return false;
        }
    }
    true
}

fn keyword(lexer: &mut Lexer) -> Option<Token>
{
    for k in &KEYWORDS {
        if k.0 == lexer.string {
            return Some(k.1.token());
        }
    }

    for k in &KEYWORDS_PRIM {
        if k.0 == lexer.string {
            return Some(k.1.token());
        }
    }
    None
}

fn number(lexer: &mut Lexer) -> Token
{
    lexer.read_while(numeric);
    if lexer.check('.') {
        lexer.read_while(numeric);
        if Some('.') == lexer.ahead() {
            // TODO: error
        }
        return lexer.string.parse::<f64>().unwrap().token()
    }
    lexer.string.parse::<usize>().unwrap().token()
}

fn binary(lexer: &mut Lexer) -> Result<Token, Error>
{
    let msg = "expected binary digit";
    match lexer.ahead() {
        None => return Err(Error::Custom(msg)),
        Some(c) => if !base_2(c) {
            return Err(Error::Custom(msg));
        }
    }

    lexer.next();
    lexer.string.clear();
    lexer.read_while(base_2);

    Ok(usize::from_str_radix(&lexer.string, 2).unwrap().token())
}

fn octal(lexer: &mut Lexer) -> Token
{
    lexer.string.clear();
    lexer.read_while(numeric);
    usize::from_str_radix(&lexer.string, 8).unwrap().token()
}

fn hex(lexer: &mut Lexer) -> Result<Token, Error>
{
    let msg = "expected hexadecimal";
    match lexer.ahead() {
        None => return Err(Error::Custom(msg)),
        Some(c) => if !alpha(c) && !numeric(c) {
            return Err(Error::Custom(msg));
        }
    }

    lexer.next();
    lexer.string.clear();
    lexer.read_while(|c| alpha(c) || numeric(c));

    Ok(usize::from_str_radix(&lexer.string, 16).unwrap().token())
}

fn escape(s: String, o: &str, n: &str) -> String
{
    match s.rfind(o) {
        None => s,
        Some(_) => s.replace(o, n)
    }
}

static ESCAPE_CHARS: [(&str, &str); 4] = [
    ("\\n", "\n"),
    ("\\r", "\r"),
    ("\\t", "\t"),
    ("\\\\", "\\")
];

fn string(lexer: &mut Lexer) -> Result<Token, Error>
{
    lexer.next();
    let c = lexer.cursor.clone();
    lexer.read_while(|c| c != '"');
    let mut s = lexer.string.to_string();
    for e in &ESCAPE_CHARS {
        s = escape(s, e.0, e.1);
    }
    let token = Token::Literal(libtoken::Literal::String(s));
    if Some('\"') != lexer.ahead() {
        return Err(Error::QuouteEnd(c));
    }
    lexer.next();
    Ok(token)
}

fn operator(lexer: &mut Lexer, c: char) -> Token
{
    use AOp::*;

    match c {
        '+' => {
            /*if lexer.check('+') {
                unimplemented!()
            } else if lexer.check('=') {
                unimplemented!()
            } else*/ {
                Add.token()
            }
        },
        '-' => Sub.token(),
        '*' => Mul.token(),
        '/' => Div.token(),
        '%' => Mod.token(),
        '=' => {
            if lexer.check('=') {
                ROp::Eq.token()
            } else {
                Token::Assign
            }
        },
        '!' => {
            if lexer.check('=') {
                ROp::Ne.token()
            } else {
                UOp::Not.token()
            }
        },
        '&' => {
            if lexer.check('&') {
                LOp::And.token()
            } else {
                BOp::And.token()
            }
        },
        '|' => {
            if lexer.check('|') {
                LOp::Or.token()
            } else {
                BOp::Or.token()
            }
        },
        '>' => {
            if lexer.check('>') {
                BOp::Rshift.token()
            } else if lexer.check('=') {
                ROp::Ge.token()
            } else {
                ROp::Gt.token()
            }
        },
        '<' => {
            if lexer.check('<') {
                BOp::Lshift.token()
            } else if lexer.check('=') {
                ROp::Le.token()
            } else {
                ROp::Lt.token()
            }
        },
        '^' => BOp::Xor.token(),
        '~' => BOp::Comp.token(),
        _ => unreachable!()
    }
}

pub fn scan(input: Vec<char>) -> Result<TokenStream, Error>
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
                if lexer.check('/') {
                    lexer.skip_while(|c| c != '\n');
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
                '@' => Token::At,
                '=' | '!' | '>' | '<' | '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '~' => {
                    if c == '-' && lexer.check('>') {
                        Token::Arrow
                    } else {
                        operator(&mut lexer, c)
                    }
                },
                '0' => {
                    if let Some(c) = lexer.ahead() {
                        match c {
                            '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => octal(&mut lexer),
                            'b' => {
                                lexer.next();
                                binary(&mut lexer)?
                            },
                            'x' => {
                                lexer.next();
                                hex(&mut lexer)?
                            },
                            '.' => number(&mut lexer),
                            _ => 0.token()
                        }
                    } else {
                        0.token()
                    }
                },
                '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => {
                    number(&mut lexer)
                },
                '"' => {
                    string(&mut lexer)?
                },
                '_' => {
                    ident(&mut lexer)
                },
                _ => {
                    if alpha(c) {
                        if keyword_rule(&mut lexer) {
                            match keyword(&mut lexer) {
                                None => lexer.string.token(),
                                Some(t) => t
                            }
                        } else {
                            lexer.next();
                            ident(&mut lexer)
                        }
                    } else {
                        return Err(Error::Invalid(format!("{}", c)));
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

    #[test]
    fn string_literal()
    {
        let input = r#"
            pure fun main(): str {
                return "test";
            }
        "#.chars().collect();

        let stream = scan(input).unwrap();
        println!("{:?}", stream);

        let sym = Symbol("main".into());
        let ret = Symbol("str".into());
        let str = Token::Literal(libtoken::Literal::String("test".into()));

        let some = [
            Some(&Keyword(Key::Pure)),
            Some(&Keyword(Key::Fun)),
            Some(&sym),
            Some(&Lparen), Some(&Rparen),
            Some(&Colon), Some(&ret),
            Some(&Lbrace),
            Some(&Keyword(Key::Return)),
            Some(&str), Some(&Semi),
            Some(&Rbrace)
        ];

        let mut tokens = stream.iter();
        for s in &some {
            assert_eq!(*s, tokens.next());
        }
    }

    #[test]
    fn ident()
    {
        let input = r#"
            _ foo _bar foo_bar
        "#.chars().collect();
        let stream = scan(input).unwrap();

        let mut tokens = stream.iter();
        assert_eq!(tokens.next(), Some(&Symbol("_".into())));
        assert_eq!(tokens.next(), Some(&Symbol("foo".into())));
        assert_eq!(tokens.next(), Some(&Symbol("_bar".into())));
        assert_eq!(tokens.next(), Some(&Symbol("foo_bar".into())));
    }

    #[test]
    fn float()
    {
        let input = "0.5+2.5".chars().collect();
        let stream = scan(input).unwrap();
        let mut tokens = stream.iter();

        assert_eq!(tokens.next(), Some(&Literal(libtoken::Literal::Float(0.5))));
        tokens.next();
        assert_eq!(tokens.next(), Some(&Literal(libtoken::Literal::Float(2.5))));
    }

    #[test]
    fn arrow()
    {
        let input = "->".chars().collect();
        let stream = scan(input).unwrap();
        let mut tokens = stream.iter();

        assert_eq!(tokens.next(), Some(&Arrow));
    }

    #[test]
    fn bitwise()
    {
        use libtoken::BitwiseOperator::*;

        let input = r#"
            &|^~<<>>
        "#.chars().collect();
        let stream = scan(input).unwrap();

        let mut tokens = stream.iter();
        assert_eq!(tokens.next(), Some(&And.token()));
        assert_eq!(tokens.next(), Some(&Or.token()));
        assert_eq!(tokens.next(), Some(&Xor.token()));
        assert_eq!(tokens.next(), Some(&Comp.token()));
        assert_eq!(tokens.next(), Some(&Lshift.token()));
        assert_eq!(tokens.next(), Some(&Rshift.token()));
    }

    #[test]
    fn numeric_base()
    {
        let input = r#"
            0b101 0100 0xFF
        "#.chars().collect();
        let stream = scan(input).unwrap();

        let mut tokens = stream.iter();
        assert_eq!(tokens.next(), Some(&5.token()));
        assert_eq!(tokens.next(), Some(&64.token()));
        assert_eq!(tokens.next(), Some(&255.token()));
    }
}
