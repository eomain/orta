
mod cast;
mod complex;
mod method;
mod ptr;
mod slice;

use std::rc::Rc;
use std::collections::HashMap;
use std::collections::HashSet;
use super::llvm;
use super::llvm::*;
use super::libast as ast;
use ast::Typed;

type Set<T> = HashSet<T>;

#[derive(Debug, Copy, Clone, PartialEq)]
enum VarType {
    Val,
    Ref
}

impl From<&Type> for VarType {
    fn from(t: &Type) -> Self
    {
        use VarType::*;
        match t {
            /*Type::Void => unreachable!(),
            Type::Int(_) |
            Type::Uint(_) |
            Type::Float |
            Type::Double |*/
            Type::Pointer(_) => Ref,
            _ => Val
        }
    }
}

pub struct Id {
    lindex: usize,
    gindex: usize,
    llabel: usize,
    locals: HashMap<String, VarType>,
    types: HashMap<String, Types>
}

impl Id {
    fn new() -> Self
    {
        Self {
            lindex: 0,
            gindex: 0,
            llabel: 0,
            locals: HashMap::new(),
            types: HashMap::new()
        }
    }

    fn insert(&mut self, id: &str, v: VarType)
    {
        self.locals.insert(id.into(), v);
    }

    fn get(&mut self, id: &str) -> Option<&VarType>
    {
        self.locals.get(id)
    }

    fn register(&mut self) -> Register
    {
        let id = format!("r.{}", self.lindex);
        self.lindex += 1;
        Register::new(&id)
    }

    fn local(&self, id: &str) -> Local
    {
        let mut i = 0;
        let mut s = String::from(id);
        let mut l = Local::new(&s);
        /*while self.locals.contains_key(l.as_ref()) {
            s = format!("{}.{}", s, i);
            l = Local::new(&s);
        }*/
        l
    }

    fn global(&mut self) -> GlobalId
    {
        let id = format!("cst{}", self.gindex);
        self.gindex += 1;
        GlobalId::new(&id)
    }

    fn label(&mut self) -> (String, Operation)
    {
        let lbl = format!("Lb{}", self.llabel);
        let op = label(&lbl);
        self.llabel += 1;
        (lbl, op)
    }

    fn reset(&mut self)
    {
        self.lindex = 0;
        self.llabel = 0;
    }
}

pub struct FunInfo {
    entry: Option<Inst>,
    retval: Option<Register>,
    rtype: bool,
    count: usize,
    total: usize,
    temp: Option<Register>
}

impl FunInfo {
    fn new(d: &ast::DataType) -> Self
    {
        Self {
            entry: None,
            retval: None,
            rtype: Type::Void != type_cast(d),
            count: 0,
            total: 0,
            temp: None
        }
    }

    fn retval(&mut self)
    {
        if let None = self.retval {
            self.retval = Some(Register::new("retval"));
        }
    }
}

pub struct Context<'a> {
    m: &'a mut Module,
    id: &'a mut Id,
    info: FunInfo
}

impl<'a> Context<'a> {
    fn new(m: &'a mut Module, id: &'a mut Id, info: FunInfo) -> Self
    {
        Self {
            m, id, info
        }
    }

    fn get_reg(&mut self) -> Register
    {
        if let Some(temp) = &self.info.temp {
            let r = temp.clone();
            self.info.temp = None;
            r
        } else {
            self.id.register()
        }
    }
}

// Convert an AST datatype into a LLVM type
fn type_cast(dtype: &ast::DataType) -> Type
{
    use ast::DataType::*;
    match dtype {
        Unit => Type::Void,
        Integer(i) => {
            use ast::IntType::*;
            match i {
                U8 => Type::Uint(8),
                U16 => Type::Uint(16),
                U32 => Type::Uint(32),
                U64 => Type::Uint(64),
                S8 => Type::Int(8),
                S16 => Type::Int(16),
                S32 => Type::Int(32),
                S64 => Type::Int(64)
            }
        },
        Float(f) => {
            use ast::FloatType::*;
            match f {
                F32 => Type::Float,
                F64 => Type::Double
            }
        },
        Boolean => Type::Int(1),
        Array(a) => {
            let sizes: Vec<_> = a.sizes.iter().rev().collect();
            let mut dtype = type_cast(&a.dtype);
            for s in sizes {
                dtype = Type::Array(*s, Box::new(dtype));
            }
            dtype
        },
        String => Type::Pointer(Box::new(Type::Int(8))),
        Slice(s) => {
            Type::from(&slice::slice_type(&*s))
        },
        Record(r) => {
            Type::from(&types_cast(&**r))
        },
        Function(v, r) => {
            let r = Box::new(type_cast(r));
            let v = v.iter().map(|a| type_cast(a)).collect();
            let f = Type::Function(r, v);
            // the type of a function is always a pointer
            // to the function
            Type::Pointer(Box::new(f))
        },
        Pointer(p) => {
            let t = Box::new(type_cast(p));
            Type::Pointer(t)
        },
        Unique(u) => {
            type_cast(&u.dtype)
        },
        Unset => unreachable!(),
        _ => unimplemented!()
    }
}

fn types_cast(d: &ast::DataRecord) -> Types
{
    let name = &d.name;
    let mut v = Vec::new();
    for attr in &d.attr {
        v.push(type_cast(&attr.1));
    }
    Types::new(Rc::new(Register::new(&format!("st.{}", name))), v)
}

fn constant(c: &mut Context, l: &ast::Literal) -> (Constant, Rc<GlobalId>)
{
    use ast::Literal::*;
    match l {
        String(s) => {
            let id = c.id.global();
            let rc = Rc::new(id);
            (Constant::String(rc.clone(), s.clone()), rc)
        }
        _ => unreachable!()
    }
}

fn literal(c: &mut Context, l: &ast::Literal,
           t: &ast::DataType, v: &mut Vec<Inst>) -> (Type, Value)
{
    use ast::Literal::*;
    match l {
        Signed(i) => (type_cast(t), Value::Int(*i)),
        Unsigned(u) => {
            if let ast::DataType::Pointer(t) = t.derived() {
                let id = c.id.register();
                let t = type_cast(t);
                let op = Operation::IntToPtr(id.clone(), Value::Uint(*u), t.clone());
                v.push(Inst::new(op, Type::Int(64)));
                (Type::Pointer(Box::new(t)), Value::Reg(id))
            } else {
                (type_cast(t), Value::Uint(*u))
            }
        },
        Float(f) => (type_cast(t), Value::Float(*f)),
        String(s) => {
            use ast::DataType::Slice;

            let id = c.id.register();
            let (constant, gid) = constant(c, l);
            let stype = constant.get_type();
            let ptr = (Type::Pointer(Box::new(stype.clone())), Value::Global(gid));
            let indexes = vec![
                (Type::Int(32), Value::Int(0)),
                (Type::Int(32), Value::Int(0))
            ];

            let op = Operation::GetElPtr(id.clone(), ptr, indexes);
            v.push(Inst::new(op, stype));

            c.m.append(constant);
            let t = type_cast(t);
            (t, Value::Reg(id))
        },
        Boolean(b) => (type_cast(t), if *b { Value::Int(1) } else { Value::Int(0) }),
        _ => unimplemented!()
    }
}

fn variable(c: &mut Context, var: &ast::Variable, v: &mut Vec<Inst>) -> (Type, Value)
{
    let l = Local::new(&var.name);
    let t = type_cast(&var.dtype);

    if let Some(vt) = c.id.get(l.as_ref()) {
        match vt {
            VarType::Ref => {
                let reg = c.id.register();
                let rc = Rc::new(Register::from(&l));
                let op = Operation::Load(reg.clone(), t.clone(), rc);
                v.push(Inst::new(op, t.clone()));
                (t, Value::Reg(reg))
            },
            VarType::Val => {
                (t, Value::Reg(Register::from(&Local::new(&var.name))))
            }
        }
    } else {
        // Assumption! function pointer
        (t, Value::Reg(Register::from(&GlobalId::new(&var.name))))
    }
}

// Convert an AST value into a LLVM value
fn value(c: &mut Context, val: &ast::Value,
         v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    use ast::Value::*;
    Some(vec![match val {
        Unit => unimplemented!(),
        Literal(l, t) => literal(c, l, &t, v),
        Complex(e) => complex::complex(c, e, v),
        Variable(var) => variable(c, var, v)
    }])
}

#[test]
fn constant_test()
{
    use crate::Output;

    let mut m = Module::new("test");
    let mut id = Id::new();
    let mut c = Context::new(&mut m, &mut id, FunInfo::new(&ast::DataType::Unit));
    let l = ast::Literal::String("hello world".into());
    let (c, _) = constant(&mut c, &l);
    c.output(&mut std::io::stdout());
}

#[inline]
fn unary_expr(c: &mut Context, e: &ast::Expr,
              v: &mut Vec<Inst>) -> (Type, Value)
{
    let e1 = expr(c, e, v).unwrap();
    let t = e1[0].0.clone();
    (t, e1[0].1.clone())
}

#[inline]
fn bin_expr(c: &mut Context, e: (&ast::Expr, &ast::Expr),
            v: &mut Vec<Inst>) -> (Type, Value, Value)
{
    let e1 = expr(c, e.0, v).unwrap();
    let e2 = expr(c, e.1, v).unwrap();
    let t = e1[0].0.clone();
    (t, e1[0].1.clone(), e2[0].1.clone())
}

#[inline]
fn signed(t: &Type) -> bool
{
    match t {
        Type::Int(_) => true,
        Type::Uint(_) => false,
        _ => unreachable!()
    }
}

fn fbin(c: &mut Context, b: &ast::BinaryExpr,
        v: &mut Vec<Inst>) -> (Operation, Type, Register)
{
    let id = c.id.register();
    use ast::BinaryExpr::*;
    match b {
        Add(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            (Operation::Fadd(id.clone(), e1, e2), t, id)
        },
        Sub(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            (Operation::Fsub(id.clone(), e1, e2), t, id)
        },
        Mul(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            (Operation::Fmul(id.clone(), e1, e2), t, id)
        },
        Div(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            (Operation::Fdiv(id.clone(), e1, e2), t, id)
        },
        Mod(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            (Operation::Frem(id.clone(), e1, e2), t, id)
        }
    }
}

fn ibin(c: &mut Context, b: &ast::BinaryExpr,
        v: &mut Vec<Inst>) -> (Operation, Type, Register)
{
    let id = c.id.register();
    use ast::BinaryExpr::*;
    match b {
        Add(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            (Operation::Add(id.clone(), e1, e2), t, id)
        },
        Sub(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            (Operation::Sub(id.clone(), e1, e2), t, id)
        },
        Mul(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            (Operation::Mul(id.clone(), e1, e2), t, id)
        },
        Div(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            if signed(&t) {
                (Operation::Sdiv(id.clone(), e1, e2), t, id)
            } else {
                (Operation::Udiv(id.clone(), e1, e2), t, id)
            }
        },
        Mod(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            if signed(&t) {
                (Operation::Srem(id.clone(), e1, e2), t, id)
            } else {
                (Operation::Urem(id.clone(), e1, e2), t, id)
            }
        }
    }
}

fn bin(c: &mut Context, b: &ast::BinaryExpr,
       v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    use ast::BinaryExpr::*;
    let (op, t, id) = match b.get_type() {
        ast::DataType::Integer(_) => ibin(c, b, v),
        ast::DataType::Float(_) => fbin(c, b, v),
        _ => unreachable!()
    };

    v.push(Inst::new(op, t.clone()));
    Some(vec![(t, Value::Reg(id))])
}

fn icmp(c: &mut Context, e: &ast::CompExpr,
        v: &mut Vec<Inst>) -> (Operation, Register)
{
    let id = c.id.register();

    use ast::CompExpr::*;
    let op = match e {
        Eq(a, b) => {
            let (t, v1, v2) = bin_expr(c, (a, b), v);
            Operation::Icmp(id.clone(), CmpType::Eq, t, v1, v2)
        },
        Ne(a, b) => {
            let (t, v1, v2) = bin_expr(c, (a, b), v);
            Operation::Icmp(id.clone(), CmpType::Ne, t, v1, v2)
        },
        Gt(a, b) => {
            let (t, v1, v2) = bin_expr(c, (a, b), v);
            if signed(&t) {
                Operation::Icmp(id.clone(), CmpType::Sgt, t, v1, v2)
            } else {
                Operation::Icmp(id.clone(), CmpType::Ugt, t, v1, v2)
            }
        },
        Lt(a, b) => {
            let (t, v1, v2) = bin_expr(c, (a, b), v);
            if signed(&t) {
                Operation::Icmp(id.clone(), CmpType::Slt, t, v1, v2)
            } else {
                Operation::Icmp(id.clone(), CmpType::Ult, t, v1, v2)
            }
        },
        Ge(a, b) => {
            let (t, v1, v2) = bin_expr(c, (a, b), v);
            if signed(&t) {
                Operation::Icmp(id.clone(), CmpType::Sge, t, v1, v2)
            } else {
                Operation::Icmp(id.clone(), CmpType::Uge, t, v1, v2)
            }
        },
        Le(a, b) => {
            let (t, v1, v2) = bin_expr(c, (a, b), v);
            if signed(&t) {
                Operation::Icmp(id.clone(), CmpType::Sle, t, v1, v2)
            } else {
                Operation::Icmp(id.clone(), CmpType::Ule, t, v1, v2)
            }
        }
    };
    (op, id)
}

fn cmp(c: &mut Context, e: &ast::CompExpr,
       v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    use ast::CompExpr::*;
    use ast::DataType::*;
    let (op, id) = match e.get_type().derived() {
        Integer(_) | Pointer(_) => icmp(c, e, v),
        _ => unreachable!()
    };

    v.push(Inst::new(op, Type::None));
    Some(vec![(Type::Int(1), Value::Reg(id))])
}

fn phi(c: &mut Context, e: (Type, Vec<(Value, Register)>),
       v: &mut Vec<Inst>) -> Register
{
    let id = c.id.register();
    let op = Operation::Phi(id.clone(), e.1);
    v.push(Inst::new(op , e.0));
    id
}

fn log_cmp(c: &mut Context, e: &ast::Expr,
           v: &mut Vec<Inst>) -> Register
{
    let (t, val) = unary_expr(c, e, v);
    let r = c.id.register();
    let op = Operation::Icmp(r.clone(), CmpType::Ne, t, val, Value::Int(0));
    v.push(Inst::new(op, Type::None));
    r
}

fn log(c: &mut Context, l: &ast::LogicalExpr,
       v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    use ast::LogicalExpr::*;
    use ast::DataType::*;

     let id = match l {
        And(a, b) => {
            let (l0s, l0op) = c.id.label();
            v.push(Inst::new(Operation::BrCond(Register::new(&l0s)), Type::None));
            v.push(Inst::new(l0op, Type::Label));
            let r1 = log_cmp(c, a, v);

            let (l1s, l1op) = c.id.label();
            let (l2s, l2op) = c.id.label();

            let op = Operation::Br(
                Value::Reg(r1.clone()), Register::new(&l1s), Register::new(&l2s)
            );
            v.push(Inst::new(op, Type::Int(1)));
            v.push(Inst::new(l1op, Type::Label));

            let r2 = log_cmp(c, b, v);
            v.push(Inst::new(Operation::BrCond(Register::new(&l2s)), Type::None));
            v.push(Inst::new(l2op, Type::Label));

            let vals = vec![
                (Value::Reg(r1), Register::new(&l0s)),
                (Value::Reg(r2), Register::new(&l1s))
            ];
            phi(c, (Type::Int(1), vals), v)
        },
        Or(a, b) => {
            let (l0s, l0op) = c.id.label();
            v.push(Inst::new(Operation::BrCond(Register::new(&l0s)), Type::None));
            v.push(Inst::new(l0op, Type::Label));
            let r1 = log_cmp(c, a, v);

            let (l1s, l1op) = c.id.label();
            let (l2s, l2op) = c.id.label();

            let op = Operation::Br(
                Value::Reg(r1.clone()), Register::new(&l2s), Register::new(&l1s)
            );
            v.push(Inst::new(op, Type::Int(1)));
            v.push(Inst::new(l1op, Type::Label));

            let r2 = log_cmp(c, b, v);
            v.push(Inst::new(Operation::BrCond(Register::new(&l2s)), Type::None));
            v.push(Inst::new(l2op, Type::Label));

            let vals = vec![
                (Value::Reg(r1), Register::new(&l0s)),
                (Value::Reg(r2), Register::new(&l1s))
            ];
            phi(c, (Type::Int(1), vals), v)
        },
        Not(a) => {
            let id = c.id.register();
            id
        }
    };

    Some(vec![(Type::Int(1), Value::Reg(id))])
}

fn ret(c: &mut Context, r: &ast::Return,
       v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    let t = type_cast(&r.dtype);
    let value = match &r.expr {
        None => None,
        Some(e) => Some(expr(c, e, v).unwrap()[0].1.clone())
    };

    if let Some(r) = &c.info.retval {
        let id = c.id.register();
        let rc = Rc::new(r.clone());
        assert_eq!(value.is_some(), true);
        let op = Operation::Store(value.unwrap(), t.clone(), rc);
        v.push(Inst::new(op, t.clone()));
    } else {
        let op = Operation::Ret(value);
        v.push(Inst::new(op, t.clone()));
    }
    None
}

fn call_args(c: &mut Context, list: &ast::ExprList,
             v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    if list.len() > 0 {
        let mut exprs = Vec::new();
        for exp in list {
            if let Some(e) = expr(c, exp, v) {
                let mut e = e.iter().map(|a| (a.0.clone(), a.1.clone())).collect();
                exprs.append(&mut e);
            }
        }
        Some(exprs)
    } else {
        None
    }
}

fn call_id(c: &mut Context, e: &ast::CallExpr, v: &mut Vec<Inst>) -> Value
{
    // The id of the function usually a Global,
    // can be a Register if a function pointer
    // If Some(_) then a function pointer
    if let Some(t) = &e.var {
        // Here we know its a function pointer
        let l = Local::new(&e.name);
        let r = Register::from(&Local::new(&e.name));
        if let Some(VarType::Val) = c.id.get(l.as_ref()) {
            Value::Reg(r)
        } else {
            let t = type_cast(t);
            let id = c.id.register();
            let op = Operation::Load(id.clone(), t.clone(), Rc::new(r));
            v.push(Inst::new(op, t));
            Value::Reg(id)
        }
    } else {
        Value::Global(Rc::new(GlobalId::new(&e.name)))
    }
}

fn call(c: &mut Context, e: &ast::CallExpr,
        v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    // Return type of function call
    let rtype = type_cast(&e.rtype);

    // Arguments to function (Type, Value)
    let args = call_args(c, &e.args, v);

    // The id of the function
    let id = call_id(c, e, v);

    // Register for the return value
    let reg: Option<Register> = if rtype == Type::Void {
        None
    } else {
        Some(c.id.register())
    };

    // Add the call instuction
    let op = Operation::Call(reg.clone(), id, args);
    v.push(Inst::new(op, rtype.clone()));

    // Return the return Register
    if let Some(r) = reg {
        Some(vec![(rtype, Value::Reg(r))])
    } else {
        None
    }
}

fn expr(c: &mut Context, e: &ast::Expr,
        v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    use ast::Expr::*;
    match e {
        Value(e) => value(c, e, v),
        Binary(b) => bin(c, b, v),
        Comp(e) => cmp(c, e, v),
        Logical(l) => log(c, l, v),
        If(f) => { conditional(c, f, v); None },
        Loop(l) => { loops(c, l, v); None },
        While(w) => { loop_while(c, w, v); None },
        Return(r) => ret(c, r, v),
        Assign(a) => { assign(c, a, v); None },
        Call(e) => call(c, e, v),
        Cast(e) => cast::cast(c, e, v),
        At(a) => method::at(c, a, v),
        Field(f) => method::field(c, f, v),
        Index(i) => Some(vec![complex::index(c, i, v)]),
        Address(a) => Some(vec![ptr::address(c, a, v)]),
        Deref(d) => Some(vec![ptr::deref(c, d, v)]),
        _ => unimplemented!()
    }
}

fn assign_ptr(c: &mut Context, t: Type, val: Value, v: &mut Vec<Inst>) -> Value
{
    let reg = c.id.register();
    let ptr = Operation::IntToPtr(reg.clone(), val, t.clone());
    v.push(Inst::new(ptr, Type::Int(64)));
    Value::Reg(reg)
}

fn assign(c: &mut Context, a: &ast::Assign, v: &mut Vec<Inst>)
{
    let l = c.id.local(&a.id);
    let complex = a.dtype.complex();

    if complex {
        let reg = Register::from(&l);
        c.id.insert(reg.as_ref(), VarType::Val);
        c.info.temp = Some(reg.clone());
    }

    let (t, mut val) = unary_expr(c, &a.expr, v);
    if !complex {
        let r = Rc::new(Register::from(&l));
        let alloc = Operation::Alloca(r.clone(), None);
        v.push(Inst::new(alloc, t.clone()));
        if let Type::Pointer(t) = &t {
            match val {
                Value::Int(_) |
                Value::Uint(_) => { val = assign_ptr(c, *t.clone(), val.clone(), v); },
                _ => ()
            }
        }
        let store = Operation::Store(val.clone(), t.clone(), r.clone());
        v.push(Inst::new(store, t.clone()));
        c.id.insert((*r).as_ref(), VarType::Ref);
    } else {
        c.info.temp = None;
    }
}

fn bexpr(c: &mut Context, b: &ast::BoolExpr, v: &mut Vec<Inst>) -> (Type, Value)
{
    use ast::Literal;
    use ast::DataType;
    use ast::BoolExpr;
    match b {
        BoolExpr::Expr(e) => expr(c, e, v).unwrap()[0].clone(),
        _ => unimplemented!()
    }
}

fn ret_end(c: &mut Context) -> bool
{
    if c.info.rtype && c.info.count == c.info.total {
        c.info.retval();
        true
    } else {
        false
    }
}

static REND: &str = "exit";

// Convert an if-else into LLVM
fn conditional_else(c: &mut Context, br: &ast::IfExpr, v: &mut Vec<Inst>)
{
    ret_end(c);
    let rend = c.info.retval.is_some();

    let (_, val) = bexpr(c, &br.cond, v);
    let (start, sop) = c.id.label();
    let (els, elop) = c.id.label();
    let (ends, endop) = c.id.label();

    let op = Operation::Br(val, Register::new(&start), Register::new(&els));
    let jmp = if rend {
        Operation::BrCond(Register::new(REND))
    } else {
        Operation::BrCond(Register::new(&ends))
    };
    v.push(Inst::new(op, Type::Int(1)));
    v.push(Inst::new(sop, Type::Label));

    for e in &br.expr {
        expr(c, e, v);
    }

    v.push(Inst::new(jmp.clone(), Type::Label));

    if let Some(exprs) = &br.other {
        v.push(Inst::new(elop, Type::Label));
        for e in exprs {
            expr(c, e, v);
        }
        v.push(Inst::new(jmp, Type::Label));
    }

    if !rend {
        v.push(Inst::new(endop, Type::Label));
    }
}

// Convert an if or possible if-else into LLVM
fn conditional(c: &mut Context, br: &ast::IfExpr, v: &mut Vec<Inst>)
{
    if br.other.is_some() {
        return conditional_else(c, br, v);
    }

    let (_, val) = bexpr(c, &br.cond, v);
    let (start, sop) = c.id.label();
    let (ends, endop) = c.id.label();
    let op = Operation::Br(val, Register::new(&start), Register::new(&ends));
    let jmp = Operation::BrCond(Register::new(&ends));
    v.push(Inst::new(op, Type::Int(1)));
    v.push(Inst::new(sop, Type::Label));

    for e in &br.expr {
        expr(c, e, v);
    }

    v.push(Inst::new(jmp, Type::Label));
    v.push(Inst::new(endop, Type::Label));
}

// Convert an unconditional loop into LLVM
fn loops(c: &mut Context, w: &ast::Loop, v: &mut Vec<Inst>)
{
    let (br, brop) = c.id.label();

    let op = Operation::BrCond(Register::new(&br));
    v.push(Inst::new(op, Type::None));
    v.push(Inst::new(brop, Type::Label));

    for e in &w.expr {
        expr(c, e, v);
    }

    let back = Operation::BrCond(Register::new(&br));
    v.push(Inst::new(back, Type::None));
}

// Convert a while loop into LLVM
fn loop_while(c: &mut Context, w: &ast::WhileExpr, v: &mut Vec<Inst>)
{
    let (_, val) = bexpr(c, &w.cond, v);
    let (br, brop) = c.id.label();
    let (start, sop) = c.id.label();
    let (ends, endop) = c.id.label();

    let op = Operation::BrCond(Register::new(&br));
    v.push(Inst::new(op, Type::Label));
    let op = Operation::Br(val, Register::new(&start), Register::new(&ends));
    v.push(Inst::new(brop, Type::Label));
    v.push(Inst::new(op, Type::Int(1)));
    v.push(Inst::new(sop, Type::Label));

    let mut vec = Vec::new();
    for e in &w.expr {
        expr(c, e, &mut vec);
    }
    v.append(&mut vec);

    let back = Operation::BrCond(Register::new(&br));
    v.push(Inst::new(back, Type::Label));
    v.push(Inst::new(endop, Type::Label));
}

// Search for use of a Ret (return) instruction
fn contains_ret(v: &Vec<Inst>) -> bool
{
    v.iter().find(|i| match i.get_op() {
        Operation::Ret(_) => true,
        _ => false
    }).is_some()
}

// Check if LLVM function needs an entry label
fn entry(v: &mut Vec<Inst>, o: Operation)
{
    if let Operation::Label(s) = v[0].get_op().clone() {
        v.insert(0, Inst::new(o, Type::Label));
        // TODO: label
        let mut s = s.clone();
        s.pop();
        let op = Operation::BrCond(Register::new(&s));
        v.insert(1, Inst::new(op, Type::Label));
    }
}

// Convert function properties into LLVM properties
fn property(p: &ast::FunctionProp) -> Properties
{
    let linkage = match p.external {
        true => Linkage::External,
        _ => Linkage::Private
    };

    Properties::new(linkage)
}

// Convert an AST function into an LLVM function
fn function(c: &mut Context, func: &ast::Function) -> Function
{
    let name = &func.name;
    let ret = type_cast(&func.ret);
    let paramlist: Vec<_> = (&func.param).into();
    let param: Option<Vec<(Type, Register)>> = if paramlist.len() == 0 {
        None
    } else {
        Some(paramlist.iter()
                      .map(|a| (type_cast(&a.1), Register::from(&Local::new(&a.0))))
                      .collect())
    };

    if let Some(param) = &param {
        for (t, r) in param {
            c.id.insert(r.as_ref(), VarType::Val);
        }
    }

    let prop = property(&func.prop);
    let mut f = Function::new(name, param, ret.clone(), Some(prop));
    let mut v = Vec::new();

    let (_, eop) = c.id.label();
    c.info.count = 1;
    c.info.total = func.expr.len();
    for e in &func.expr {
        expr(c, e, &mut v);
        c.info.count += 1;
    }

    if let Some(r) = &c.info.retval {
        let rc = Rc::new(r.clone());
        let op = Operation::Alloca(rc.clone(), None);
        v.insert(0, Inst::new(op, ret.clone()));

        v.push(Inst::new(label(REND), Type::Label));
        let id = c.id.register();
        let op = Operation::Load(id.clone(), ret.clone(), rc);
        v.push(Inst::new(op, ret.clone()));
        let op = Operation::Ret(Some(Value::Reg(id)));
        v.push(Inst::new(op, ret));
    } else if ret == Type::Void {
        let i = Inst::new(Operation::Ret(None), ret);
        v.push(i);
    }

    if v.len() > 1 {
        entry(&mut v, eop);
    }

    while v.len() > 0 {
        f.append(v.remove(0));
    }

    c.id.reset();
    f
}

fn declare(f: &ast::FunctionDec) -> FunctionDec
{
    let name = &f.name;
    let paramlist: &Vec<_> = &f.param;
    let param: Option<Vec<Type>> = if paramlist.len() == 0 {
        None
    } else {
        Some(paramlist.iter()
                      .map(|a| type_cast(&a)).collect())
    };
    let ret = type_cast(&f.ret);
    FunctionDec::new(name, param, ret)
}

// Create a new LLVM module from an AST
pub fn main(name: &str, tree: &ast::SyntaxTree) -> Module
{
    let mut module = Module::new(name);
    let mut id = Id::new();

    for d in &tree.declarations {
        module.append(declare(d));
    }

    for (n, r) in &tree.records {
        let t = types_cast(r);
        id.types.insert(n.into(), t.clone());
        module.append(t);
    }

    for (n, d) in &tree.defines {
        let mut fun = method::define(&mut id, &mut module, n, d);
        while fun.len() > 0 {
            module.append(fun.remove(0));
        }
    }

    for f in &tree.functions {
        let function = {
            let mut context = Context::new(
                &mut module, &mut id, FunInfo::new(&f.ret)
            );
            function(&mut context, f)
        };
        module.append(function);
    }
    module
}
