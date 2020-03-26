
extern crate libtoken;

use std::fmt;
use std::rc::Rc;
use std::collections::HashMap;
use std::collections::HashSet;
pub use libtoken::Literal;
pub use libtoken::TokenStream;

// used to store the size of primitive integer types
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

impl IntType {
    pub fn signed(&self) -> bool
    {
        use IntType::*;
        match self {
            U8 | U16 | U32 | U64 => false,
            S8 | S16 | S32 | S64 => true
        }
    }
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

#[derive(Debug, Clone, PartialEq)]
pub struct Types {
    pub name: String,
    pub dtype: Box<DataType>
}

impl Types {
    pub fn new(name: &str, dtype: DataType) -> Self
    {
        Self {
            name: name.into(),
            dtype: Box::new(dtype)
        }
    }
}

impl fmt::Display for Types {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", &self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unique {
    pub name: String,
    pub dtype: Box<DataType>
}

impl Unique {
    pub fn new(name: &str, dtype: DataType) -> Self
    {
        Self {
            name: name.into(),
            dtype: Box::new(dtype)
        }
    }
}

impl fmt::Display for Unique {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", &self.name)
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
    Char,
    String,
    Tuple(Tuple),
    Array(Rc<Array>),
    Slice(Rc<Slice>),
    Record(Rc<DataRecord>),
    //Record(String),
    Function(Vec<DataType>, Box<DataType>),
    Pointer(Rc<DataType>),
    Types(Types),
    Unique(Unique),
    Named(String)
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", match self {
            DataType::Unset => "unset",
            DataType::Unit => "()",
            DataType::Integer(i) => i.into(),
            DataType::Float(f) => f.into(),
            DataType::Char => "char",
            DataType::Boolean => "bool",
            DataType::String => "string",
            DataType::Array(a) => {
                return write!(f, "{}", *a);
            },
            DataType::Slice(s) => {
                return write!(f, "{}", *s);
            },
            DataType::Record(r) => {
                &r.name
            },
            DataType::Function(a, r) => {
                if a.len() == 0 {
                    write!(f, "{} -> ", DataType::Unit)?;
                } else {
                    for v in a {
                        write!(f, "{} -> ", v)?;
                    }
                }
                return write!(f, "{}", *r);
            },
            DataType::Pointer(d) => {
                if let DataType::Function(_, _) = **d {
                    return write!(f, "^({})", *d);
                } else {
                    return write!(f, "^{}", *d);
                }
            },
            DataType::Types(t) => {
                return write!(f, "{}", t);
            },
            DataType::Unique(u) => {
                return write!(f, "{}", u);
            },
            DataType::Named(n) => &n,
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

    #[inline]
    pub fn derived(&self) -> &DataType
    {
        match self {
            DataType::Types(t) => &*t.dtype,
            DataType::Unique(u) => &*u.dtype,
            _ => &self,
        }
    }

    pub fn complex(&self) -> bool
    {
        match self {
            //DataType::String |
            DataType::Tuple(_) |
            DataType::Array(_) |
            DataType::Slice(_) |
            DataType::Record(_) => true,
            _ => false
        }
    }

    pub fn array(&self) -> bool
    {
        match self {
            DataType::Array(_) => true,
            _ => false
        }
    }

    pub fn unique(&self) -> bool
    {
        match self {
            DataType::Unique(_) => true,
            _ => false
        }
    }

    pub fn function(&self) -> bool
    {
        match self {
            DataType::Function(_, _) => true,
            _ => false
        }
    }

    pub fn get_record(&self) -> Option<&DataRecord>
    {
        match self {
            DataType::Record(r) => Some(r),
            _ => None
        }
    }

    pub fn get_named(&self, names: &mut HashSet<String>)
    {
        use DataType::*;
        match self {
            Pointer(d) => d.get_named(names),
            Named(s) => { names.insert(s.clone()); },
            _ => ()
        }
    }

    pub fn set_named(&mut self, names: &mut HashMap<String, DataType>)
    {
        use DataType::*;
        match self {
            Pointer(d) => {
                let mut n = (**d).clone();
                n.set_named(names);
                *d = Rc::new(n);
            },
            Named(s) => { *self = names.get(s).unwrap().clone(); },
            _ => ()
        }
    }
}

impl From<Unique> for DataType {
    fn from(u: Unique) -> Self
    {
        DataType::Unique(u)
    }
}

impl From<DataRecord> for DataType {
    fn from(r: DataRecord) -> Self
    {
        DataType::Record(Rc::new(r))
    }
}

impl From<(Vec<DataType>, DataType)> for DataType {
    fn from(f: (Vec<DataType>, DataType)) -> Self
    {
        DataType::Function(f.0, Box::new(f.1))
    }
}

pub trait Typed {
    fn get_type(&self) -> &DataType;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Array {
    pub sizes: Vec<usize>,
    pub dtype: DataType
}

impl Array {
    pub fn new(sizes: Vec<usize>, dtype: DataType) -> Self
    {
        Self {
            sizes, dtype
        }
    }
}

impl From<Array> for DataType {
    fn from(s: Array) -> Self
    {
        DataType::Array(Rc::new(s))
    }
}

impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        for size in &self.sizes {
            write!(f, "[{}]", size)?
        }
        write!(f, " {}", &self.dtype)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Slice {
    pub ptr: DataType
}

impl Slice {
    pub fn new(dtype: DataType) -> Self
    {
        Self {
            ptr: dtype
        }
    }
}

impl From<Slice> for DataType {
    fn from(s: Slice) -> Self
    {
        DataType::Slice(Rc::new(s))
    }
}

impl fmt::Display for Slice {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "|{}|", &self.ptr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayLiteral {
    pub elements: Vec<Expr>,
    pub dtype: DataType
}

impl ArrayLiteral {
    pub fn new(elements: Vec<Expr>) -> Self
    {
        Self {
            elements,
            dtype: DataType::Unset
        }
    }
}

impl Typed for ArrayLiteral {
    fn get_type(&self) -> &DataType
    {
        &self.dtype
    }
}

impl From<ArrayLiteral> for ComplexLiteral {
    fn from(a: ArrayLiteral) -> Self
    {
        ComplexLiteral::Array(a)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral {
    pub name: String,
    pub fields: Vec<(String, Expr)>,
    pub dtype: DataType
}

impl StructLiteral {
    pub fn new(name: &str, fields: Vec<(String, Expr)>) -> Self
    {
        Self {
            name: name.into(),
            fields,
            dtype: DataType::Unset
        }
    }
}

impl Typed for StructLiteral {
    fn get_type(&self) -> &DataType
    {
        &self.dtype
    }
}

impl From<StructLiteral> for ComplexLiteral {
    fn from(s: StructLiteral) -> Self
    {
        ComplexLiteral::Struct(s)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComplexLiteral {
    Array(ArrayLiteral),
    Struct(StructLiteral)
}

impl Typed for ComplexLiteral {
    fn get_type(&self) -> &DataType
    {
        use ComplexLiteral::*;
        match self {
            Array(a) => a.get_type(),
            Struct(s) => s.get_type(),
        }
    }
}

impl From<ComplexLiteral> for Value {
    fn from(c: ComplexLiteral) -> Self
    {
        Value::Complex(c)
    }
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
}

impl Typed for Variable {
    fn get_type(&self) -> &DataType
    {
        &self.dtype
    }
}

impl From<Variable> for Expr {
    fn from(v: Variable) -> Self
    {
        Expr::Value(Value::Variable(v))
    }
}

// A value that can be either a
// literal or a value denoted by a variable
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// returns an empty value `()`
    Unit,
    Literal(Literal, DataType),
    Complex(ComplexLiteral),
    Variable(Variable)
}

impl Typed for Value {
    fn get_type(&self) -> &DataType
    {
        match self {
            Value::Unit => unimplemented!(),
            Value::Literal(_, t) => t,
            Value::Complex(c) => c.get_type(),
            Value::Variable(v) => v.get_type()
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cast {
    pub expr: Box<Expr>,
    pub dtype: DataType
}

impl Cast {
    pub fn new(expr: Expr, dtype: DataType) -> Self
    {
        Self {
            expr: Box::new(expr),
            dtype
        }
    }
}

impl Typed for Cast {
    fn get_type(&self) -> &DataType
    {
        &self.dtype
    }
}

/// An expression that evaluates to a value
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Value(Value),
    Binary(BinaryExpr),
    Comp(CompExpr),
    Logical(LogicalExpr),
    Bit(BitExpr),
    If(IfExpr),
    Loop(Loop),
    While(WhileExpr),
    Return(Return),
    Assign(Box<Assign>),
    Call(CallExpr),
    Cast(Cast),
    At(AtExpr),
    Field(FieldAccess),
    Method(MethodAccess),
    Slice(SliceExpr),
    Index(Index),
    Address(Address),
    Deref(Deref),
    DerefField(DerefField),
    Unsafe(UnsafeExpr)
}

impl Typed for Expr {
    fn get_type(&self) -> &DataType
    {
        match self {
            Expr::Value(v) => v.get_type(),
            Expr::Binary(b) => b.get_type(),
            Expr::Comp(c) => c.get_type(),
            Expr::Logical(l) => l.get_type(),
            Expr::Bit(b) => b.get_type(),
            Expr::Return(r) => r.get_type(),
            Expr::Call(c) => c.get_type(),
            Expr::Cast(c) => c.get_type(),
            Expr::At(a) => a.get_type(),
            Expr::Field(f) => f.get_type(),
            Expr::Method(m) => m.get_type(),
            Expr::Index(i) => i.get_type(),
            Expr::Address(a) => a.get_type(),
            Expr::Deref(d) => d.get_type(),
            Expr::DerefField(d) => d.get_type(),
            Expr::Unsafe(u) => u.get_type(),
            _ => unimplemented!()
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Address {
    pub id: String,
    pub dtype: DataType
}

impl Address {
    pub fn new(id: &str) -> Self
    {
        Self {
            id: id.into(),
            dtype: DataType::Unset
        }
    }
}

impl Typed for Address {
    fn get_type(&self) -> &DataType
    {
        &self.dtype
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Deref {
    pub expr: Box<Expr>,
    pub dtype: DataType
}

impl Deref {
    pub fn new(expr: Expr) -> Self
    {
        Self {
            expr: Box::new(expr),
            dtype: DataType::Unset
        }
    }
}

impl Typed for Deref {
    fn get_type(&self) -> &DataType
    {
        &self.dtype
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DerefField {
    pub field: String,
    pub expr: Box<Expr>,
    pub dtype: DataType
}

impl DerefField {
    pub fn new(field: &str, expr: Expr) -> Self
    {
        Self {
            field: field.into(),
            expr: Box::new(expr),
            dtype: DataType::Unset
        }
    }
}

impl Typed for DerefField {
    fn get_type(&self) -> &DataType
    {
        &self.dtype
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub id: String,
    pub dtype: DataType,
    pub expr: Expr,
    pub declare: bool
}

impl Assign {
    pub fn new(id: &str, dtype: DataType, expr: Expr, declare: bool) -> Self
    {
        Self {
            id: id.into(),
            dtype,
            expr,
            declare
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
    Not(Box<Expr>)
}

impl Typed for LogicalExpr {
    fn get_type(&self) -> &DataType
    {
        use LogicalExpr::*;
        match self {
            And(e1, _) |
            Or(e1, _) |
            Not(e1) => e1.get_type()
        }
    }
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

impl Typed for CompExpr {
    fn get_type(&self) -> &DataType
    {
        use CompExpr::*;
        match self {
            Eq(e1, _) |
            Ne(e1, _) |
            Gt(e1, _) |
            Lt(e1, _) |
            Ge(e1, _) |
            Le(e1, _) => e1.get_type()
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BitExpr {
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Xor(Box<Expr>, Box<Expr>),
    Comp(Box<Expr>),
    Lsh(Box<Expr>, Box<Expr>),
    Rsh(Box<Expr>, Box<Expr>)
}

impl Typed for BitExpr {
    fn get_type(&self) -> &DataType
    {
        use BitExpr::*;
        match self {
            And(e1, _) |
            Or(e1, _) |
            Xor(e1, _) |
            Comp(e1) |
            Lsh(e1, _) |
            Rsh(e1, _) => e1.get_type()
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BoolExpr {
    Expr(Box<Expr>),
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
pub struct Loop {
    pub expr: ExprList
}

impl Loop {
    pub fn new(expr: ExprList) -> Self
    {
        Self {
            expr
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
pub enum ControlFlow {
    Break,
    Continue,
    Goto(String)
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpr {
    aexpr: Box<Expr>,
    expr: Box<Expr>
}

impl AssignExpr {
    pub fn new(aexpr: Expr, expr: Expr) -> Self
    {
        Self {
            aexpr: Box::new(aexpr),
            expr: Box::new(expr)
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
    pub args: ExprList,
    pub var: Option<DataType>
}

impl CallExpr {
    pub fn new(name: &str, args: ExprList) -> Self
    {
        Self {
            name: name.into(),
            rtype: DataType::Unset,
            args,
            var: None
        }
    }
}

impl Typed for CallExpr {
    fn get_type(&self) -> &DataType
    {
        &self.rtype
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AtExpr {
    pub dtype: DataType
}

impl AtExpr {
    pub fn new() -> Self
    {
        Self {
            dtype: DataType::Unset
        }
    }
}

impl Typed for AtExpr {
    fn get_type(&self) -> &DataType
    {
        &self.dtype
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    pub field: String,
    pub expr: Box<Expr>,
    pub dtype: DataType
}

impl FieldAccess {
    pub fn new(field: &str, expr: Expr) -> Self
    {
        Self {
            field: field.into(),
            expr: Box::new(expr),
            dtype: DataType::Unset
        }
    }
}

impl Typed for FieldAccess {
    fn get_type(&self) -> &DataType
    {
        &self.dtype
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodAccess {
    pub expr: Box<Expr>,
    pub call: CallExpr
}

impl MethodAccess {
    pub fn new(call: CallExpr, expr: Expr) -> Self
    {
        Self {
            expr: Box::new(expr),
            call
        }
    }
}

impl Typed for MethodAccess {
    fn get_type(&self) -> &DataType
    {
        self.call.get_type()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnsafeExpr {
    pub expr: Box<Expr>
}

impl UnsafeExpr {
    pub fn new(expr: Expr) -> Self
    {
        Self {
            expr: Box::new(expr)
        }
    }
}

impl Typed for UnsafeExpr {
    fn get_type(&self) -> &DataType
    {
        self.expr.get_type()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SliceExpr {
    Ptr(Box<Expr>),
    Len(Box<Expr>)
}

impl Typed for SliceExpr {
    fn get_type(&self) -> &DataType
    {
        use SliceExpr::*;
        match self {
            Ptr(e) |
            Len(e) => e.get_type()
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Index {
    pub index: Box<Expr>,
    pub expr: Box<Expr>,
    pub dtype: DataType
}

impl Index {
    pub fn new(index: Expr, expr: Expr) -> Self
    {
        Self {
            index: Box::new(index),
            expr: Box::new(expr),
            dtype: DataType::Unset
        }
    }
}

impl Typed for Index {
    fn get_type(&self) -> &DataType
    {
        &self.dtype
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
    pub map: HashMap<String, (DataType, usize)>
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
    pub pure: bool,
    pub external: bool,
    pub r#unsafe: bool
}

impl FunctionProp {
    pub fn default() -> Self
    {
        Self {
            pure: false,
            external: false,
            r#unsafe: false
        }
    }

    pub fn pure(&mut self)
    {
        self.pure = true;
    }

    pub fn external(&mut self) -> bool
    {
        let external = self.external;
        self.external = true;
        external
    }

    pub fn r#unsafe(&mut self) -> bool
    {
        let r#unsafe = self.r#unsafe;
        self.r#unsafe = true;
        r#unsafe
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

#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    pub name: String,
    pub param: ParamList,
    pub ret: DataType,
    pub expr: ExprList
}

impl Method {
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

#[derive(Debug, Clone, PartialEq)]
pub struct Define {
    pub methods: Vec<Method>
}

impl Define {
    pub fn new(methods: Vec<Method>) -> Self
    {
        Self {
            methods
        }
    }
}

#[derive(Debug)]
pub enum Definition {
    Function(Function),
    Declare(FunctionDec),
    Record(String, DataRecord),
    Types(String, DataType),
    Define(String, Define)
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
    pub records: Vec<(String, DataRecord)>,
    pub types: Vec<(String, DataType)>,
    pub defines: Vec<(String, Define)>,
    pub definition: Vec<Definition>
}

impl SyntaxTree {
    // Create a new empty tree
    pub fn new() -> Self
    {
        Self {
            functions: Vec::new(),
            declarations: Vec::new(),
            records: Vec::new(),
            types: Vec::new(),
            defines: Vec::new(),
            definition: Vec::new()
        }
    }

    pub fn definition<I>(&mut self, d: I)
        where I: Into<Definition>
    {
        self.definition.push(d.into());
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
        self.records.push((r.name.clone(), r));
    }

    pub fn append_types(&mut self, name: &str, d: DataType)
    {
        self.types.push((name.into(), d));
    }

    pub fn append_define(&mut self, name: String, d: Define)
    {
        self.defines.push((name.into(), d));
    }
}
