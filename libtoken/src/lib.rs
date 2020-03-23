
use std::fmt;

pub type List<T> = Vec<T>;

// A simple type used to store a list of input tokens
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
    Comma,
    Assign,
    Arrow,
    At
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
    Loop,
    True,
    False,
    Return,
    Let,
    Var,
    Const,
    Type,
    Foreign,
    Extern,
    Break,
    Continue,
    Unsafe,
    Unique,
    Define
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
    F64,
    Char,
    String
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
    Not, // negate
    Sub
}

impl IntoToken for UnaryOperator {
    fn token(&self) -> Token
    {
        Token::Operator(Operator::Unary(self.clone()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LogicalOperator {
    And,
    Or,
    Not
}

impl IntoToken for LogicalOperator {
    fn token(&self) -> Token
    {
        Token::Operator(Operator::Logical(self.clone()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RelationalOperator {
    Eq, // equal
    Ne, // not equal
    Gt, // greater than
    Lt, // less than
    Ge, // greater than or equal,
    Le  // less than or equal
}

impl IntoToken for RelationalOperator {
    fn token(&self) -> Token
    {
        Token::Operator(Operator::Relational(self.clone()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BitwiseOperator {
    And,
    Or,
    Xor,
    Comp,
    Lshift,
    Rshift
}

impl IntoToken for BitwiseOperator {
    fn token(&self) -> Token
    {
        Token::Operator(Operator::Bitwise(self.clone()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AssignmentOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod
}

impl IntoToken for AssignmentOperator {
    fn token(&self) -> Token
    {
        Token::Operator(Operator::Assignment(self.clone()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Unary(UnaryOperator),
    Arithmetic(ArithmeticOperator),
    Logical(LogicalOperator),
    Relational(RelationalOperator),
    Bitwise(BitwiseOperator),
    Assignment(AssignmentOperator)
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

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        use Literal::*;
        match self {
            Boolean(b) => write!(f, "{}", b),
            Character(c) => write!(f, "{}", c),
            Signed(i) => write!(f, "{}", i),
            Unsigned(u) => write!(f, "{}", u),
            Float(d) => write!(f, "{}", d),
            String(s) => write!(f, "{}", s)
        }
    }
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

#[cfg(test)]
mod tests {
    use crate::*;
    use crate::Key::*;
    use crate::Token::Keyword;

    #[test]
    fn token()
    {
        use Literal::*;

        let token_n = 100.token();
        let token_c = '{'.token();
        let token_b = true.token();
        let token_s = "foo".token();

        assert_eq!(Token::Literal(Unsigned(255)), 255.token());
        assert_eq!(Token::Literal(Unsigned(256)), 256.token());
        assert_eq!(Token::Literal(Unsigned(65536)), 65536.token());
    }
}
