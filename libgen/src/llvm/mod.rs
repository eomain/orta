
use std::fmt;
use std::rc::Rc;
use crate::Output;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Linkage {
    Private,
    External
}

impl From<Linkage> for &str {
    fn from(l: Linkage) -> &'static str
    {
        use Linkage::*;
        match l {
            Private => "private",
            External => ""
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Properties {
    linkage: Linkage
}

impl Properties {
    fn default() -> Self
    {
        Self {
            linkage: Linkage::Private
        }
    }
}

impl From<Properties> for String {
    fn from(p: Properties) -> String
    {
        let linkage: &str = p.linkage.into();
        format!("{}", linkage)
    }
}

impl fmt::Display for Properties {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        let prop: String = self.clone().into();
        write!(f, "{}", prop)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Register {
    id: String
}

impl Register {
    pub fn new(id: &str) -> Self
    {
        Self {
            id: lid(id)
        }
    }
}

impl AsRef<str> for Register {
    fn as_ref(&self) -> &str
    {
        &self.id
    }
}

// global identifier
#[inline]
fn gid(name: &str) -> String
{
    format!("@{}", name)
}

// local identifier
#[inline]
fn lid(name: &str) -> String
{
    format!("%{}", name)
}

fn param(p: &Vec<(Type, Register)>) -> String
{
    let mut s = String::new();
    if p.len() > 0 {
        let mut arg = &p[0];
        let mut t: String = arg.0.clone().into();
        let mut r = &arg.1.id;
        s.push_str(&format!("{} {}", t, r));
        for i in 1..p.len() {
            arg = &p[i];
            t = arg.0.clone().into();
            r = &arg.1.id;
            s.push_str(&format!(", {} {}", t, r));
        }
    }
    s
}

fn param_dec(p: &Vec<Type>) -> String
{
    let mut s = String::new();
    if p.len() > 0 {
        let mut arg = &p[0];
        let mut t: String = arg.clone().into();
        s.push_str(&format!("{}", t));
        for i in 1..p.len() {
            arg = &p[i];
            t = arg.clone().into();
            s.push_str(&format!(", {}", t));
        }
    }
    s
}

#[derive(Debug, Clone)]
pub struct Module {
    name: String,
    values: Vec<GlobalValue>
}

impl Module {
    pub fn new(name: &str) -> Self
    {
        Self {
            name: name.into(),
            values: vec![]
        }
    }

    pub fn append<I>(&mut self, i: I)
        where I: Into<GlobalValue>
    {
        self.values.push(i.into());
    }
}

impl Output for Module {
    fn output<W>(&self, w: &mut W)
        where W: std::io::Write
    {
        for v in &self.values {
            v.output(w);
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Void,
    Int(usize),
    Uint(usize),
    Float,
    Double,
    Array(usize, Box<Type>),
    Pointer(Box<Type>)
}

impl From<Type> for String {
    fn from(t: Type) -> String
    {
        use Type::*;
        match t {
            Void => "void".into(),
            Int(n) => format!("i{}", n),
            Uint(n) => format!("i{}", n),
            Float => "float".into(),
            Double => "double".into(),
            Array(s, t) => {
                 let t: String = (*t).into();
                 format!("[{} x {}]", s, t)
             },
            Pointer(t) => {
                let t: String = (*t).into();
                format!("{}*", t)
            }
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        let s: String = self.clone().into();
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(isize),
    Uint(usize),
    Reg(Register)
}

impl From<Value> for String
{
    fn from(v: Value) -> String
    {
        use Value::*;
        match v {
            Int(i) => format!("{}", i),
            Uint(u) => format!("{}", u),
            Reg(r) => r.id
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        let s: String = self.clone().into();
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone)]
pub enum Operation {
    Add(Register, Value, Value),
    Call,
    Mul(Register, Value, Value),
    Ret(Option<Value>),
    Sub(Register, Value, Value),
    Sdiv(Register, Value, Value),
    Srem(Register, Value, Value),
    Udiv(Register, Value, Value),
    Urem(Register, Value, Value),
}

impl Operation {
    fn name(&self) -> &str
    {
        use Operation::*;
        match self {
            Add(_, _, _) => "add",
            Call => "call",
            Mul(_, _, _) => "mul",
            Ret(_) => "ret",
            Sub(_, _, _) => "sub",
            Sdiv(_, _, _) => "sdiv",
            Srem(_, _, _) => "srem",
            Udiv(_, _, _) => "udiv",
            Urem(_, _, _) => "urem",
        }
    }

    fn args(&self) -> Option<String>
    {
        use Operation::*;
        match self {
            Add(_, a, b) |
            Mul(_, a, b) |
            Sub(_, a, b) |
            Sdiv(_, a, b) |
            Srem(_, a, b) |
            Udiv(_, a, b) |
            Urem(_, a, b) => Some(format!("{}, {}", a, b)),
            Ret(v) => match v {
                None => None,
                Some(v) => Some(format!("{}", v))
            },
            _ => None
        }
    }

    fn reg(&self) -> Option<String>
    {
        use Operation::*;
        match self {
            Add(r, _, _) |
            Mul(r, _, _) |
            Sub(r, _, _) |
            Sdiv(r, _, _) |
            Srem(r, _, _) |
            Udiv(r, _, _) |
            Urem(r, _, _) => Some(r.id.clone()),
            _ => None
        }
    }
}

#[derive(Debug, Clone)]
pub struct Inst {
    op: Operation,
    t: Type
}

impl Inst {
    pub fn new(op: Operation, t: Type) -> Self
    {
        Self {
            op, t
        }
    }
}

impl From<Inst> for String {
    fn from(i: Inst) -> String
    {
        let t: String = i.t.into();
        let name = i.op.name();
        let args = i.op.args();
        let reg = i.op.reg();

        if let Some(reg) = reg {
            if let Some(args) = args {
                format!("{} = {} {} {}", reg, name, t, args)
            } else {
                unreachable!()
            }
        } else {
            if let Some(args) = args {
                format!("{} {} {}", name, t, args)
            } else {
                format!("{} {}", name, t)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Global {
    name: String,
    pub prop: Properties
}

impl Global {
    fn new(name: &str) -> Self
    {
        Self {
            name: gid(name),
            prop: Properties::default()
        }
    }
}

impl From<Global> for GlobalValue {
    fn from(g: Global) -> Self
    {
        GlobalValue::Global(g)
    }
}

impl Output for Global {
    fn output<W>(&self, w: &mut W)
        where W: std::io::Write
    {
        writeln!(w, "{} {}", self.name, self.prop);
    }
}

#[derive(Debug, Clone)]
pub struct GlobalId {
    id: String
}

#[derive(Debug, Clone)]
pub enum Constant {
    String(Rc<GlobalId>, String)
}

#[derive(Debug, Clone)]
pub struct Function {
    name: String,
    param: Option<Vec<(Type, Register)>>,
    ret: Type,
    body: Vec<Inst>,
    prop: Properties
}

impl Function {
    pub fn new(name: &str, param: Option<Vec<(Type, Register)>>,
           ret: Type, prop: Option<Properties>) -> Self
    {
        Self {
            name: gid(name),
            param,
            ret,
            body: vec![],
            prop: if let Some(p) = prop { p } else { Properties::default() }
        }
    }

    pub fn append(&mut self, i: Inst)
    {
        self.body.push(i);
    }
}

impl From<Function> for GlobalValue {
    fn from(f: Function) -> Self
    {
        GlobalValue::Function(f)
    }
}

impl Output for Function {
    fn output<W>(&self, w: &mut W)
        where W: std::io::Write
    {
        let name = &self.name;
        let t: String = self.ret.clone().into();
        let mut i: String;

        let param = match &self.param {
            None => String::new(),
            Some(p) => param(&p)
        };
        let prop: String = self.prop.into();

        writeln!(w, "define {} {} {}({}) {{", prop, t, name, param);
        for inst in &self.body {
            i = inst.clone().into();
            writeln!(w, "\t{}", i);
        }

        if let Type::Void = self.ret {
            let ret = Inst::new(Operation::Ret(None), self.ret.clone());
            let ret: String = ret.into();
            writeln!(w, "\t{}", ret);
        }
        writeln!(w, "}}");
    }
}

#[test]
fn function()
{
    use Value::*;
    let param = vec![
        (Type::Int(8), Register::new("x")),
        (Type::Int(8), Register::new("y"))
    ];
    let mut f = Function::new("main", Some(param), Type::Void, None);
    let add = Operation::Add(Register::new("1"), Reg(Register::new("0")), Int(5));
    let mul = Operation::Mul(Register::new("2"), Reg(Register::new("1")), Int(2));
    f.append(Inst::new(add, Type::Int(8)));
    f.append(Inst::new(mul, Type::Int(8)));
    let mut w = std::io::stdout();
    f.output(&mut w);
}

#[derive(Debug, Clone)]
pub struct FunctionDec {
    name: String,
    param: Option<Vec<Type>>,
    ret: Type
}

impl FunctionDec {
    fn new(name: &str, param: Option<Vec<Type>>, ret: Type) -> Self
    {
        Self {
            name: gid(name),
            param,
            ret
        }
    }
}

impl From<FunctionDec> for GlobalValue {
    fn from(f: FunctionDec) -> Self
    {
        GlobalValue::FunctionDec(f)
    }
}

impl Output for FunctionDec {
    fn output<W>(&self, w: &mut W)
        where W: std::io::Write
    {
        let name = &self.name;
        let ret: String = self.ret.clone().into();

        let param = match &self.param {
            None => String::new(),
            Some(p) => param_dec(&p)
        };

        writeln!(w, "declare {} {}({})", ret, name, param);
    }
}

#[derive(Debug, Clone)]
enum GlobalValue {
    Global(Global),
    Function(Function),
    FunctionDec(FunctionDec)
}

impl Output for GlobalValue {
    fn output<W>(&self, w: &mut W)
        where W: std::io::Write
    {
        use GlobalValue::*;
        match self {
            Global(g) => g.output(w),
            Function(f) => f.output(w),
            FunctionDec(d) => d.output(w)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::llvm::*;

    #[test]
    fn module()
    {
        let g = Global::new("g");
        let f = Function::new("f", None, Type::Void, None);
        let mut m = Module::new("test");
        m.append(g);
        m.append(f);
        println!("{:?}", m);
        m.output(&mut std::io::stdout());
    }

    #[test]
    fn declare()
    {
        let ret = Type::Void;
        let param = Some(vec![Type::Int(32)]);
        let dec = FunctionDec::new("exit", param, ret);
        dec.output(&mut std::io::stdout());
    }
}