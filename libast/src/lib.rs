
extern crate libsym;
extern crate libtoken;

pub use libtoken::Literal;
pub use libtoken::TokenStream;

// A variable identifier
#[derive(Debug)]
pub struct Variable {
    name: String
}

// A value that can be either a
// literal or a value denoted by a variable
#[derive(Debug)]
pub enum Value {
    /// returns an empty value `()`
    Unit,
    Literal(Literal),
    Variable(Variable)
}

/// An expression that evaluates to a value
#[derive(Debug)]
pub enum Expr {
    Value(Value),
    Binary(BinaryExpr),
    Logical(LogicalExpr),
    If(IfExpr)
}

#[derive(Debug)]
pub enum BinaryExpr {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>)
}

#[derive(Debug)]
pub enum LogicalExpr {
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Not(Box<Expr>, Box<Expr>)
}

#[derive(Debug)]
pub enum ComparisionExpr {
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>)
}

pub type BoolExpr = ComparisionExpr;

#[derive(Debug)]
pub struct IfExpr {
    cond: BoolExpr,
    expr: ExprList
}

#[derive(Debug)]
pub struct WhileExpr {
    cond: BoolExpr,
    expr: ExprList
}

/// A list of expressions
pub type ExprList = Vec<Expr>;

#[derive(Debug)]
pub struct Param {

}

pub type ParamList = Vec<Param>;

#[derive(Debug)]
pub struct Function {
    param: ParamList,
    expr: ExprList
}

#[derive(Debug)]
pub struct Program {
    functions: Vec<Function>
}

impl Program {
    pub fn new() -> Self
    {
        Self {
            functions: Vec::new()
        }
    }
}

#[derive(Debug)]
pub struct ParseTree {
    program: Program
}

impl ParseTree {
    pub fn new() -> Self
    {
        Self {
            program: Program::new()
        }
    }
}

#[cfg(test)]
mod tests {

}
