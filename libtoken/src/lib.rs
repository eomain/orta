
pub type List<T> = Vec<T>;

pub type TokenStream<'a> = List<Token<'a>>;

// Represents a single unit `token` of the program
// at the source level.
#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Keyword(Key),
    Operator,
    Symbol(&'a str),
    Literal(Literal),
    Lparen,
    Rparen,
    Lsqr,
    Rsqr,
    Lbrace,
    Rbrace,
    Period,
    Colon,
    Semi,
    Comma
}

// language reserved keywords
#[derive(Debug, Clone, PartialEq)]
pub enum Key {
    Fun, // fun
    Pure,
    If,
    Else,
    While
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArithmeticOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Not, // negate
    Sub
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalOperator {
    Eq, // equal
    Ne, // not equal
    Gt, // greater than
    Lt // less than
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Unary(UnaryOperator),
    Arithmetic(ArithmeticOperator),
    Logical(LogicalOperator)
}

// type for all primitive literal values
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Boolean(bool),
    Character(char),
    Signed(IntegerSize, isize),
    Unsigned(IntegerSize, usize),
    Float(f64)
}

// used to store the size of primitive integer types
#[derive(Debug, Clone, PartialEq)]
pub enum IntegerSize {
    S8,
    S16,
    S32,
    S64
}

// trait to convert a type into a token
pub trait IntoToken {
    fn token<'a>(&'a self) -> Token<'a>;
}

// trait to attempt to convert a type into a token
pub trait TryIntoToken {
    fn token<'a>(&'a self) -> Option<Token<'a>>;
}

impl IntoToken for bool {
    fn token<'a>(&'a self) -> Token<'a>
    {
        Token::Literal(Literal::Boolean(*self))
    }
}

impl TryIntoToken for char {
    fn token<'a>(&'a self) -> Option<Token<'a>>
    {
        let token = match self {
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
            _ => return None
        };

        Some(token)
    }
}

impl IntoToken for usize {
    fn token<'a>(&'a self) -> Token<'a>
    {
        use IntegerSize::*;

        const MI32: usize = std::u32::MAX as usize;

        let size = match *self {
            0..=0xFF => S8,
            0..=0xFFFF => S16,
            0..=MI32 => S32,
            _ => S64
        };

        Token::Literal(Literal::Unsigned(size, *self))
    }
}

impl IntoToken for &str {
    fn token<'a>(&'a self) -> Token<'a>
    {
        Token::Symbol(self)
    }
}

pub trait KeyToken {
    fn keyword<'a>(&'a self) -> Option<Token<'a>>;
}

impl KeyToken for &str {
    #[inline]
    fn keyword<'a>(&'a self) -> Option<Token<'a>>
    {
        use Key::*;

        let key = match *self {
            "fun" => Fun,
            "pure" => Pure,
            "if" => If,
            "else" => Else,
            "while" => While,
            _ => return None
        };

        Some(Token::Keyword(key))
    }
}

#[cfg(test)]
mod tests {
    use crate::*;
    use crate::Key::*;
    use crate::Token::Keyword;

    #[test]
    fn token()
    {
        use Literal::*;
        use IntegerSize::*;

        let token_n = 100.token();
        let token_c = '{'.token();
        let token_b = true.token();
        let token_s = "foo".token();

        assert_eq!(Token::Literal(Unsigned(S8, 255)), 255.token());
        assert_eq!(Token::Literal(Unsigned(S16, 256)), 256.token());
        assert_eq!(Token::Literal(Unsigned(S32, 65536)), 65536.token());
    }

    #[test]
    fn keyword()
    {
        assert_eq!(Some(Keyword(Fun)), "fun".keyword());
    }
}
