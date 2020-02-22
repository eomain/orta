
pub type List<T> = Vec<T>;

pub type TokenStream = List<Token>;

// Represents a single unit `token` of the program
// at the source level.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Keyword(Key),
    Primitive(Prim),
    Operator(Operator),
    Symbol(String),
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
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Key {
    Fun, // fun
    Pure,
    If,
    Else,
    While,
    For,
    True,
    False,
    Return
}

impl IntoToken for Key {
    fn token(&self) -> Token
    {
        Token::Keyword(self.clone())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Prim {
    Bool,
    U8,
    U16,
    U32,
    U64,
    UInt,
    S8,
    S16,
    S32,
    S64,
    SInt,
    F32,
    F64
}

impl IntoToken for Prim {
    fn token(&self) -> Token
    {
        Token::Primitive(self.clone())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ArithmeticOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod
}

impl IntoToken for ArithmeticOperator {
    fn token(&self) -> Token
    {
        Token::Operator(Operator::Arithmetic(self.clone()))
    }
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
    Signed(isize),
    Unsigned(usize),
    Float(f64),
    String(String)
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
    fn token(&self) -> Token;
}

// trait to attempt to convert a type into a token
pub trait TryIntoToken {
    fn token(&self) -> Option<Token>;
}

impl IntoToken for bool {
    fn token(&self) -> Token
    {
        Token::Literal(Literal::Boolean(*self))
    }
}

impl TryIntoToken for char {
    fn token(&self) -> Option<Token>
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
    fn token(&self) -> Token
    {
        Token::Literal(Literal::Unsigned(*self))
    }
}

impl IntoToken for f64 {
    fn token(&self) -> Token
    {
        Token::Literal(Literal::Float(*self))
    }
}

impl IntoToken for str {
    fn token(&self) -> Token
    {
        Token::Symbol(self.into())
    }
}

/*pub trait KeyToken {
    fn keyword(&self) -> Option<Token>;
}

impl KeyToken for &str {
    #[inline]
    fn keyword(&self) -> Option<Token>
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
}*/

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

        assert_eq!(Token::Literal(Unsigned(255)), 255.token());
        assert_eq!(Token::Literal(Unsigned(256)), 256.token());
        assert_eq!(Token::Literal(Unsigned(65536)), 65536.token());
    }

    /*#[test]
    fn keyword()
    {
        assert_eq!(Some(Keyword(Fun)), "fun".keyword());
    }*/
}
