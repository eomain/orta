
extern crate libsym;
extern crate libtoken;

use std::rc::Rc;
use std::collections::HashMap;
pub use libtoken::Literal;
pub use libtoken::TokenStream;

#[derive(Debug, Clone, PartialEq)]
pub enum IntType {
    U8,
    U16,
    U32,
    U64,
    S8,
    S16,
    S32,
    S64
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    types: Vec<DataType>
}

// A data record or structure, data type
#[derive(Debug, Clone, PartialEq)]
pub struct DataRecord {
    attr: Vec<(String, DataType)>
}

// Convert a record into an equivalent tuple
impl From<&DataRecord> for Tuple {
    fn from(r: &DataRecord) -> Self
    {
        Tuple {
            types: r.attr.iter().map(|a| a.1.clone()).collect()
        }
    }
}

// All natively supported data types
#[derive(Debug, Clone, PartialEq)]
pub enum DataType {
    Unit,
    Integer(IntType),
    Boolean,
    Tuple(Tuple),
    Array(Box<DataType>),
    //Record(Rc<DataRecord>),
    Record(String),
    Function(Tuple, Box<DataType>)
}

impl DataType {
    // Return whether of not data types are compatible
    fn compat() -> bool
    {
        unimplemented!()
    }
}

// A variable identifier
#[derive(Debug)]
pub struct Variable {
    pub name: String
}

// A value that can be either a
// literal or a value denoted by a variable
#[derive(Debug)]
pub enum Value {
    /// returns an empty value `()`
    Unit,
    Literal(Literal),
    Variable(String)
}

/// An expression that evaluates to a value
#[derive(Debug)]
pub enum Expr {
    Value(Value),
    Binary(BinaryExpr),
    Logical(LogicalExpr),
    If(IfExpr),
    Return(Return),
    Call(CallExpr)
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
pub enum CompExpr {
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>)
}

pub type BoolExpr = CompExpr;

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

#[derive(Debug)]
pub struct Return {
    pub dtype: DataType,
    pub expr: Option<Box<Expr>>
}

#[derive(Debug)]
pub struct CallExpr {
    pub args: ExprList
}

#[derive(Debug)]
pub struct Assign {
    pub var: Variable,
    pub expr: Box<Expr>
}

/// A sequence of expressions
pub type ExprList = Vec<Expr>;

pub struct SortedMap<K, V>
    where K: Clone + std::hash::Hash + std::cmp::Eq,
          V: Clone {
    map: HashMap<K, (V, usize)>
}

impl<K, V> SortedMap<K, V>
    where K: Clone + std::hash::Hash + std::cmp::Eq,
          V: Clone {
    pub fn new() -> Self
    {
        Self {
            map: HashMap::new()
        }
    }

    pub fn add<I>(&mut self, k: I, v: V)
        where I: Into<K>
    {
        let n = self.map.len();
        // TODO: check if param already exists
        self.map.insert(k.into(), (v, n));
    }

    pub fn into_vec(&self) -> Vec<(K, V)>
    {
        let mut v: Vec<_> = self.map.iter().collect();
        v.sort_by(|a, b| (a.1).1.cmp(&(b.1).1));
        v.iter().map(|a| (a.0.clone(), (a.1).0.clone())).collect()
    }
}

/*impl<K, V> From<&SortedMap<K, V>> for Vec<(K, V)> {
    fn from(s: &SortedMap<K, V>) -> Self
    {
        let mut v: Vec<_> = s.map.iter().collect();
        v.sort_by(|a, b| (a.1).1.cmp(&(b.1).1));
        v.iter().map(|a| (a.0.clone(), (a.1).0.clone())).collect()
    }
}*/

type Param = (String, DataType);

// An ordered set of parameters
#[derive(Debug, Clone, PartialEq)]
pub struct ParamList {
    map: HashMap<String, (DataType, usize)>
}

impl ParamList {
    pub fn new() -> Self
    {
        Self {
            map: HashMap::new()
        }
    }

    // Returns false if a parameter with the same name
    // already exists. Otherwise it returns true.
    pub fn add(&mut self, id: &str, dtype: DataType) -> bool
    {
        let n = self.map.len();
        if self.map.contains_key(id) { return false; }
        self.map.insert(id.into(), (dtype, n));
        true
    }
}

impl From<&ParamList> for Vec<(String, DataType)> {
    fn from(p: &ParamList) -> Self
    {
        let mut v: Vec<_> = p.map.iter().collect();
        v.sort_by(|a, b| (a.1).1.cmp(&(b.1).1));
        v.iter().map(|a| (a.0.clone(), (a.1).0.clone())).collect()
    }
}

// An abstract representation of a program function
#[derive(Debug)]
pub struct Function {
    // the name of the function
    pub name: String,
    // the sequence of parameters accepted by the function
    pub param: ParamList,
    // the return type of the function
    pub ret: DataType,
    // the sequence of expressions within the function
    pub expr: ExprList
}

impl Function {
    // Create a new function
    pub fn new(name: &str, param: ParamList,
               ret: DataType, expr: ExprList) -> Self
    {
        Self {
            name: name.into(),
            param,
            ret,
            expr
        }
    }
}

// An abstract representation of program,
// structured by its syntax
#[derive(Debug)]
pub struct SyntaxTree {
    //pub records: Vec<Rc<DataRecord>>,

    // Abstract representation of all functions within
    // the program root
    pub functions: Vec<Function>
}

impl SyntaxTree {
    // Create a new empty tree
    pub fn new() -> Self
    {
        Self {
            functions: vec![]
        }
    }

    // Append a function to the root of the tree
    pub fn append(&mut self, f: Function)
    {
        self.functions.push(f);
    }
}
