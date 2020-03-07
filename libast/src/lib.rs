
extern crate libtoken;

use std::fmt;
use std::rc::Rc;
use std::collections::HashMap;
pub use libtoken::Literal;
pub use libtoken::TokenStream;

#[derive(Debug, Copy, Clone, PartialEq)]
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

impl From<&IntType> for &str {
    fn from(t: &IntType) -> Self
    {
        use IntType::*;
        match t {
            U8 => "u8",
            U16 => "u16",
            U32 => "u32",
            U64 => "u64",
            S8 => "i8",
            S16 => "i16",
            S32 => "i32",
            S64 => "i64"
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FloatType {
    F32,
    F64
}

impl From<&FloatType> for &str {
    fn from(t: &FloatType) -> Self
    {
        use FloatType::*;
        match t {
            F32 => "f32",
            F64 => "f64"
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    types: Vec<DataType>
}

// A data record or structure, data type
#[derive(Debug, Clone, PartialEq)]
pub struct DataRecord {
    pub name: String,
    pub attr: Vec<(String, DataType)>
}

impl DataRecord {
    pub fn new(name: String, attr: Vec<(String, DataType)>) -> Self
    {
        Self {
            name,
            attr
        }
    }
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
    Unset,
    Unit,
    Integer(IntType),
    Float(FloatType),
    Boolean,
    String,
    Tuple(Tuple),
    Array(Box<DataType>),
    Record(Rc<DataRecord>),
    //Record(String),
    Function(Tuple, Box<DataType>),
    Pointer(Rc<DataType>)
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", match self {
            DataType::Unset => "unset",
            DataType::Unit => "unit",
            DataType::Integer(i) => i.into(),
            DataType::Float(f) => f.into(),
            DataType::Boolean => "bool",
            DataType::String => "string",
            _ => unimplemented!()
        })
    }
}

impl DataType {
    // Return whether of not data types are compatible
    fn compat() -> bool
    {
        unimplemented!()
    }
}

pub trait Typed {
    fn get_type(&self) -> &DataType;
}

// A variable identifier
#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: String,
    pub dtype: DataType
}

impl Variable {
    pub fn new(name: &str) -> Self
    {
        Self {
            name: name.into(),
            dtype: DataType::Unset
        }
    }

    fn get_type(&self) -> &DataType
    {
        &self.dtype
    }
}

// A value that can be either a
// literal or a value denoted by a variable
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// returns an empty value `()`
    Unit,
    Literal(Literal, DataType),
    Variable(Variable)
}

impl Typed for Value {
    fn get_type(&self) -> &DataType
    {
        match self {
            Value::Unit => unimplemented!(),
            Value::Literal(_, t) => t,
            Value::Variable(v) => v.get_type()
        }
    }
}

/// An expression that evaluates to a value
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Value(Value),
    Binary(BinaryExpr),
    Logical(LogicalExpr),
    If(IfExpr),
    Return(Return),
    Assign(Box<Assign>),
    Call(CallExpr)
}

impl Typed for Expr {
    fn get_type(&self) -> &DataType
    {
        match self {
            Expr::Value(v) => v.get_type(),
            Expr::Binary(b) => b.get_type(),
            Expr::Return(r) => r.get_type(),
            Expr::Call(c) => c.get_type(),
            _ => unimplemented!()
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub id: String,
    pub dtype: DataType,
    pub expr: Expr
}

impl Assign {
    pub fn new(id: &str, dtype: DataType, expr: Expr) -> Self
    {
        Self {
            id: id.into(),
            dtype,
            expr
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Assign(Assign),
    Expr(Expr)
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryExpr {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>)
}

impl Typed for BinaryExpr {
    fn get_type(&self) -> &DataType
    {
        use BinaryExpr::*;
        match self {
            Add(a, _) |
            Sub(a, _) |
            Mul(a, _) |
            Div(a, _) |
            Mod(a, _) => a.get_type()
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalExpr {
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Not(Box<Expr>, Box<Expr>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompExpr {
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum BoolExpr {
    Bool(bool),
    Comp(CompExpr)
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    pub cond: BoolExpr,
    pub expr: ExprList,
    //pub elif: Vec<(BoolExpr, ExprList)>,
    pub other: Option<ExprList>,
}

impl IfExpr {
    pub fn new(cond: BoolExpr, expr: ExprList, other: Option<ExprList>) -> Self
    {
        Self {
            cond,
            expr,
            other
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileExpr {
    pub cond: BoolExpr,
    pub expr: ExprList
}

impl WhileExpr {
    pub fn new(cond: BoolExpr, expr: ExprList) -> Self
    {
        Self {
            cond,
            expr
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub dtype: DataType,
    pub expr: Option<Box<Expr>>
}

impl Return {
    pub fn new(expr: Option<Expr>) -> Self
    {
        Self {
            dtype: DataType::Unset,
            expr: match expr {
                None => None,
                Some(e) => Some(Box::new(e))
            }
        }
    }
}

impl Typed for Return {
    fn get_type(&self) -> &DataType
    {
        &self.dtype
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub name: String,
    pub rtype: DataType,
    pub args: ExprList
}

impl CallExpr {
    pub fn new(name: &str, args: ExprList) -> Self
    {
        Self {
            name: name.into(),
            rtype: DataType::Unset,
            args
        }
    }
}

impl Typed for CallExpr {
    fn get_type(&self) -> &DataType
    {
        &self.rtype
    }
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

// Set of properties defined for a function
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionProp {
    // if the function is pure
    pub pure: bool
}

impl FunctionProp {
    pub fn default() -> Self
    {
        Self {
            pure: false
        }
    }

    pub fn pure(&mut self)
    {
        self.pure = true;
    }
}

// An abstract representation of a program function
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    // the name of the function
    pub name: String,
    // the properties of the function
    pub prop: FunctionProp,
    // the sequence of parameters accepted by the function
    pub param: ParamList,
    // the return type of the function
    pub ret: DataType,
    // the sequence of expressions within the function
    pub expr: ExprList
}

impl Function {
    // Create a new function
    pub fn new(name: &str, prop: FunctionProp, param: ParamList,
               ret: DataType, expr: ExprList) -> Self
    {
        Self {
            name: name.into(),
            prop,
            param,
            ret,
            expr
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDec {
    pub name: String,
    pub param: Vec<DataType>,
    pub ret: DataType
}

impl FunctionDec {
    pub fn new(name: &str, param: Vec<DataType>, ret: DataType) -> Self
    {
        Self {
            name: name.into(),
            param,
            ret
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
    pub functions: Vec<Function>,
    pub declarations: Vec<FunctionDec>,
    pub records: HashMap<String, DataRecord>,
}

impl SyntaxTree {
    // Create a new empty tree
    pub fn new() -> Self
    {
        Self {
            functions: Vec::new(),
            declarations: Vec::new(),
            records: HashMap::new()
        }
    }

    // Append a function to the root of the tree
    pub fn append(&mut self, f: Function)
    {
        self.functions.push(f);
    }

    pub fn append_dec(&mut self, f: FunctionDec)
    {
        self.declarations.push(f);
    }

    pub fn append_rec(&mut self, r: DataRecord)
    {
        self.records.insert(r.name.clone(), r);
    }
}
