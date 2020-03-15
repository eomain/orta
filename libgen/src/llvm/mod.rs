
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
    pub fn new(linkage: Linkage) -> Self
    {
        Self {
            linkage
        }
    }

    pub fn default() -> Self
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

#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    id: String
}

impl Local {
    pub fn new(id: &str) -> Self
    {
        Self {
            id: lid(&format!("v.{}", id))
        }
    }
}

impl AsRef<str> for Local {
    fn as_ref(&self) -> &str
    {
        &self.id
    }
}

impl From<&Local> for Register {
    fn from(l: &Local) -> Register
    {
        Register {
            id: l.id.clone()
        }
    }
}

#[test]
fn local_test()
{
    let l = Local::new("a");
    assert_eq!("%v.a", l.id);
    let r = Register::from(&l);
    assert_eq!("%v.a", r.id);
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

fn phi_list(p: &Vec<(Value, Register)>) -> String
{
    let mut s = String::new();
    if p.len() > 0 {
        let mut arg = &p[0];
        let mut t: String = arg.0.clone().into();
        let mut r = &arg.1.id;
        s.push_str(&format!("[ {}, {} ]", t, r));
        for i in 1..p.len() {
            arg = &p[i];
            t = arg.0.clone().into();
            r = &arg.1.id;
            s.push_str(&format!(", [ {}, {} ]", t, r));
        }
    }
    s
}

fn param_val(p: &Vec<(Type, Value)>) -> String
{
    let mut s = String::new();
    if p.len() > 0 {
        let mut arg = &p[0];
        let mut t: String = arg.0.clone().into();
        let mut v = &arg.1;
        s.push_str(&format!("{} {}", t, v));
        for i in 1..p.len() {
            arg = &p[i];
            t = arg.0.clone().into();
            v = &arg.1;
            s.push_str(&format!(", {} {}", t, v));
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
            writeln!(w);
            v.output(w);
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Int(usize),
    Uint(usize),
    Float,
    Double,
    Array(usize, Box<Type>),
    Function(Box<Type>, Vec<Type>),
    Pointer(Box<Type>),
    Types(Rc<GlobalId>, Vec<Type>),
    Label,
    None
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
            Function(r, p) => {
                let r = String::from(*r);
                let p = param_dec(&p);
                format!("{} ({})", r, p)
            },
            Pointer(t) => {
                let t: String = (*t).into();
                format!("{}*", t)
            },
            Types(r, t) => {
                let p = param_dec(&t);
                format!("{} = type {{ {} }}", r.id, p)
            },
            Label | None => "".into()
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

#[test]
fn types()
{
    let gid = Rc::new(GlobalId::new("data"));
    let t = Type::Types(gid, vec![Type::Int(32), Type::Int(8)]);
    println!("{}", t);
}

#[test]
fn fun_type_test()
{
    use Type::*;
    let r = Int(32);
    let p = vec![
        Int(32), Int(32)
    ];
    let f = Function(Box::new(r), p);
    let p = Pointer(Box::new(f));
    println!("{}", p);
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Void,
    Int(isize),
    Uint(usize),
    Float(f64),
    Reg(Register),
    Local(Local),
    Global(Rc<GlobalId>)
}

impl From<Value> for String
{
    fn from(v: Value) -> String
    {
        use Value::*;
        match v {
            Void => "".into(),
            Int(i) => format!("{}", i),
            Uint(u) => format!("{}", u),
            Float(f) => format!("{:.32}", f),
            Local(l) => l.id,
            Reg(r) => r.id,
            Global(g) => g.id.clone()
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

pub fn label(s: &str) -> Operation
{
    Operation::Label(format!("{}:", s))
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operation {
    Add(Register, Value, Value),
    Fadd(Register, Value, Value),
    Alloca(Rc<Register>, Option<u32>),
    Br(Value, Register, Register),
    BrCond(Register),
    Call(Option<Register>, Value, Option<Vec<(Type, Value)>>),
    GetElPtr(Register, (Type, Rc<GlobalId>), Vec<(Type, Value)>),
    Load(Register, Type, Rc<Register>),
    Mul(Register, Value, Value),
    Fmul(Register, Value, Value),
    Ret(Option<Value>),
    Store(Value, Type, Rc<Register>),
    Sub(Register, Value, Value),
    Fsub(Register, Value, Value),
    Sdiv(Register, Value, Value),
    Fdiv(Register, Value, Value),
    Srem(Register, Value, Value),
    Frem(Register, Value, Value),
    Udiv(Register, Value, Value),
    Urem(Register, Value, Value),
    IntToPtr(Register, Value, Type),
    PtrToInt(Register, Value, Type),
    Trunc(Register, Value, Type),
    Zext(Register, Value, Type),
    Sext(Register, Value, Type),
    FpTrunc(Register, Value, Type),
    FpExt(Register, Value, Type),
    FpToUi(Register, Value, Type),
    FpToSi(Register, Value, Type),
    UiToFp(Register, Value, Type),
    SiToFp(Register, Value, Type),
    Bitcast(Register, Value, Type),
    Icmp(Register, CmpType, Type, Value, Value),
    Phi(Register, Vec<(Value, Register)>),
    Label(String)
}

impl Operation {
    fn name(&self) -> &str
    {
        use Operation::*;
        match self {
            Add(_, _, _) => "add",
            Fadd(_, _, _) => "fadd",
            Alloca(_, _) => "alloca",
            Br(_, _, _) => "br",
            BrCond(_) => "br",
            Call(_, _, _) => "call",
            GetElPtr(_, _, _) => "getelementptr",
            Load(_, _, _) => "load",
            Mul(_, _, _) => "mul",
            Fmul(_, _, _) => "fmul",
            Ret(_) => "ret",
            Store(_, _, _) => "store",
            Sub(_, _, _) => "sub",
            Fsub(_, _, _) => "fsub",
            Sdiv(_, _, _) => "sdiv",
            Fdiv(_, _, _) => "fdiv",
            Srem(_, _, _) => "srem",
            Frem(_, _, _) => "frem",
            Udiv(_, _, _) => "udiv",
            Urem(_, _, _) => "urem",
            IntToPtr(_, _, _) => "inttoptr",
            PtrToInt(_, _, _) => "ptrtoint",
            Trunc(_, _, _) => "trunc",
            Zext(_, _, _) => "zext",
            Sext(_, _, _) => "sext",
            FpTrunc(_, _, _) => "fptrunc",
            FpExt(_, _, _) => "fpext",
            FpToUi(_, _, _) => "fptoui",
            FpToSi(_, _, _) => "fptosi",
            UiToFp(_, _, _) => "uitofp",
            SiToFp(_, _, _) => "sitofp",
            Bitcast(_, _, _) => "bitcast",
            Icmp(_, _, _, _, _) => "icmp",
            Phi(_, _) => "phi",
            Label(s) => &s
        }
    }

    fn args(&self) -> Option<String>
    {
        use Operation::*;
        match self {
            Add(_, a, b) |
            Fadd(_, a, b) |
            Mul(_, a, b) |
            Fmul(_, a, b) |
            Sub(_, a, b) |
            Fsub(_, a, b) |
            Sdiv(_, a, b) |
            Fdiv(_, a, b) |
            Srem(_, a, b) |
            Frem(_, a, b) |
            Udiv(_, a, b) |
            Urem(_, a, b) => Some(format!("{}, {}", a, b)),
            Ret(v) => match v {
                None => None,
                Some(v) => Some(format!("{}", v))
            },
            Alloca(_, s) => {
                if let Some(i) = s {
                    Some(format!(", i32 {}", i))
                } else {
                    None
                }
            },
            Br(c, a, b) => {
                Some(format!("{}, label {}, label {}", c, a.id, b.id))
            },
            BrCond(r) => Some(format!("label {}", r.id)),
            Call(_, v, args) => {
                match args {
                    None => Some(format!("{}()", v)),
                    Some(args) => {
                        let p = param_val(args);
                        Some(format!("{}({})", v, p))
                    }
                }
            },
            Load(_, t, p) => {
                Some(format!(", {}* {}", t, p.id))
            },
            Store(v, t, r) => {
                Some(format!("{}, {}* {}", v, t, r.id))
            },
            GetElPtr(_, ptr, indexes) => {
                let p = param_val(indexes);
                Some(format!(", {} {}, {}", ptr.0, ptr.1.id, p))
            },
            IntToPtr(_, v, t) => {
                Some(format!("{} to {}*", v, t))
            },
            PtrToInt(_, v, t) | Trunc(_, v, t) | Zext(_, v, t) |
            Sext(_, v, t) | FpTrunc(_, v, t) | FpExt(_, v, t) |
            FpToUi(_, v, t) | FpToSi(_, v, t) | UiToFp(_, v, t) |
            SiToFp(_, v, t) | Bitcast(_, v, t) => {
                Some(format!("{} to {}", v, t))
            },
            Icmp(_, c, t, v1, v2) => {
                Some(format!("{} {} {}, {}", c, t, v1, v2))
            },
            Phi(_, v) => Some(format!("{}", phi_list(v))),
            _ => None
        }
    }

    fn reg(&self) -> Option<&str>
    {
        use Operation::*;
        match self {
            Add(r, _, _) |
            Fadd(r, _, _) |
            Mul(r, _, _) |
            Fmul(r, _, _) |
            Sub(r, _, _) |
            Fsub(r, _, _) |
            Sdiv(r, _, _) |
            Fdiv(r, _, _) |
            Srem(r, _, _) |
            Frem(r, _, _) |
            Udiv(r, _, _) |
            Urem(r, _, _) => Some(&r.id),
            Alloca(r, _) => Some(&r.id),
            Call(r, _, _) => if let Some(r) = r { Some(&r.id) } else { None },
            Load(r, _, _) => Some(&r.id),
            GetElPtr(r, _, _) => Some(&r.id),
            IntToPtr(r, _, _) | PtrToInt(r, _, _) |
            Trunc(r, _, _) | Zext(r, _, _) | Sext(r, _, _) |
            FpTrunc(r, _, _) | FpExt(r, _, _) | FpToUi(r, _, _) |
            FpToSi(r, _, _) | UiToFp(r, _, _) | SiToFp(r, _, _) |
            Bitcast(r, _, _) => Some(&r.id),
            Icmp(r, _, _, _, _) => Some(&r.id),
            Phi(r, _) => Some(&r.id),
            _ => None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CmpType {
    Eq,
    Ne,
    Ugt,
    Uge,
    Ult,
    Ule,
    Sgt,
    Sge,
    Slt,
    Sle
}

impl fmt::Display for CmpType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        use CmpType::*;
        write!(f, "{}", match self {
            Eq => "eq",
            Ne => "ne",
            Ugt => "ugt",
            Uge => "uge",
            Ult => "ult",
            Ule => "ule",
            Sgt => "sgt",
            Sge => "sge",
            Slt => "slt",
            Sle => "sle"
        })
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

    pub fn get_op(&self) -> &Operation
    {
        &self.op
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
                format!("{} = {} {}", reg, name, t)
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

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalId {
    id: String
}

impl GlobalId {
    pub fn new(id: &str) -> Self
    {
        Self {
            id: gid(id)
        }
    }
}

impl AsRef<str> for GlobalId {
    fn as_ref(&self) -> &str
    {
        &self.id
    }
}

impl From<&GlobalId> for Register {
    fn from(g: &GlobalId) -> Register
    {
        Register {
            id: g.id.clone()
        }
    }
}

#[derive(Debug, Clone)]
pub enum Constant {
    String(Rc<GlobalId>, String)
}

impl Constant {
    pub fn get_type(&self) -> Type
    {
        use Constant::*;
        match self {
            String(_, s) => {
                let len = s.len() + 1;
                Type::Array(len, Box::new(Type::Int(8)))
            }
        }
    }
}

impl Output for Constant {
    fn output<W>(&self, w: &mut W)
        where W: std::io::Write
    {
        use Constant::*;
        match self {
            String(g, s) => {
                let len = s.len() + 1;
                let s = format!("{}\\00", s);
                writeln!(w, "{} = constant [{} x i8] c\"{}\"", g.id, len, s);
            }
        }
    }
}

impl From<Constant> for GlobalValue {
    fn from(c: Constant) -> Self
    {
        GlobalValue::Constant(c)
    }
}

#[test]
fn constant()
{
    let id = Rc::new(GlobalId::new("str"));
    let c = Constant::String(id, "hello world".into());
    c.output(&mut std::io::stdout());
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
        let ret: String = self.ret.clone().into();
        let mut i: String;

        let param = match &self.param {
            None => String::new(),
            Some(p) => param(&p)
        };
        let prop: String = self.prop.into();

        if prop.len() > 0 {
            writeln!(w, "define {} {} {}({}) {{", prop, ret, name, param);
        } else {
            writeln!(w, "define {} {}({}) {{", ret, name, param);
        }
        for inst in &self.body {
            i = inst.clone().into();
            writeln!(w, "\t{}", i);
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
    pub fn new(name: &str, param: Option<Vec<Type>>, ret: Type) -> Self
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
    Constant(Constant),
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
            Constant(c) => c.output(w),
            Global(g) => g.output(w),
            Function(f) => f.output(w),
            FunctionDec(d) => d.output(w)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn var()
    {
        let r = Rc::new(Register::new("a"));
        let t = Type::Int(32);
        let insts = vec![
            Inst::new(Operation::Alloca(r.clone(), None), t.clone()),
            Inst::new(Operation::Store(Value::Int(100), t.clone(), r.clone()), t.clone()),
            Inst::new(Operation::Load(Register::new("0"), t.clone(), r.clone()), t)
        ];
        for inst in &insts {
            let s = String::from(inst.clone());
            println!("{}", s);
        }
    }
}
