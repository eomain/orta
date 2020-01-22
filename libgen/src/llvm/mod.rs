
use std::fmt;
use crate::Output;

#[derive(Debug, Clone)]
pub struct Register {
    id: String
}

impl Register {
    fn new(id: &str) -> Self
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

#[derive(Debug, Clone)]
pub struct Module {
    name: String,
    functions: Vec<Function>
}

impl Module {
    fn new(name: &str) -> Self
    {
        Self {
            name: name.into(),
            functions: vec![]
        }
    }
}

impl Output for Module {
    fn output<W>(&self, w: &mut W)
        where W: std::io::Write
    {
        for f in &self.functions {
            f.output(w);
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Type {
    Void,
    Int(usize),
    Uint(usize),
    Float,
    Double
}

impl From<Type> for String {
    fn from(t: Type) -> String
    {
        use Type::*;
        match t {
            Void => "void".into(),
            Int(n) => format!("i{}", n),
            Uint(n) => format!("u{}", n),
            Float => "float".into(),
            Double => "double".into()
        }
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
    Ret
}

impl Operation {
    fn name(&self) -> &str
    {
        use Operation::*;
        match self {
            Add(_, _, _) => "add",
            Call => "call",
            Ret => "ret"
        }
    }

    fn args(&self) -> Option<String>
    {
        use Operation::*;
        match self {
            Add(_, a, b) => Some(format!("{}, {}", a, b)),
            _ => None
        }
    }

    fn reg(&self) -> Option<String>
    {
        use Operation::*;
        match self {
            Add(r, _, _) => Some(r.id.clone()),
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
    fn new(op: Operation, t: Type) -> Self
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

pub struct Global {
    name: String
}

pub struct Local {
    name: String
}

#[derive(Debug, Clone)]
pub struct Function {
    name: String,
    param: Option<Vec<Type>>,
    ret: Type,
    body: Vec<Inst>
}

impl Function {
    fn new(name: &str, param: Option<Vec<Type>>, ret: Type) -> Self
    {
        Self {
            name: name.into(),
            param,
            ret,
            body: vec![]
        }
    }

    fn append(&mut self, i: Inst)
    {
        self.body.push(i);
    }
}

impl Output for Function {
    fn output<W>(&self, w: &mut W)
        where W: std::io::Write
    {
        let name = gid(&self.name);
        let t: String = self.ret.into();
        let ret = Inst::new(Operation::Ret, self.ret);
        let ret: String = ret.into();
        let mut i: String;

        writeln!(w, "define {} {}() {{", t, name);
        for inst in &self.body {
            i = inst.clone().into();
            writeln!(w, "\t{}", i);
        }
        writeln!(w, "\t{}", ret);
        writeln!(w, "}}");
    }
}

#[test]
fn function()
{
    use Value::*;
    let mut f = Function::new("main", None, Type::Void);
    let add = Operation::Add(Register::new("1"), Reg(Register::new("0")), Int(5));
    f.append(Inst::new(add, Type::Int(8)));
    let mut w = std::io::stdout();
    f.output(&mut w);
}

#[cfg(test)]
mod tests {
    use crate::llvm::*;

    fn module()
    {

    }
}
