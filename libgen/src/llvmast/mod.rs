
use std::rc::Rc;
use std::collections::HashMap;
use super::llvm::*;
use super::libast as ast;

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

struct Id {
    lindex: usize,
    gindex: usize,
    llabel: usize,
    locals: HashMap<String, VarType>
}

impl Id {
    fn new() -> Self
    {
        Self {
            lindex: 0,
            gindex: 0,
            llabel: 0,
            locals: HashMap::new()
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

struct FunInfo {
    entry: Option<Inst>
}

impl FunInfo {
    fn new() -> Self
    {
        Self {
            entry: None
        }
    }
}

struct Context<'a> {
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
        String => Type::Pointer(Box::new(Type::Int(8))),
        Function(v, r) => {
            let r = Box::new(type_cast(r));
            let v = v.iter().map(|a| type_cast(a)).collect();
            let f = Type::Function(r, v);
            // the type of a function is always a pointer
            // to the function
            Type::Pointer(Box::new(f))
        },
        Unset => unreachable!(),
        _ => unimplemented!()
    }
}

fn types_cast(d: &ast::DataRecord) -> Type
{
    let name = &d.name;
    let mut v = Vec::new();
    for attr in &d.attr {
        v.push(type_cast(&attr.1));
    }
    Type::Types(Rc::new(GlobalId::new(name)), v)
}

fn constant(c: &mut Context, l: &ast::Literal) -> (Constant, Rc<GlobalId>)
{
    use ast::Literal::*;
    match l {
        String(s) => {
            let id = c.id.global();
            let rc = Rc::new(id);
            (Constant::String(rc.clone(), s.clone()), rc.clone())
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
        Unsigned(u) => (type_cast(t), Value::Uint(*u)),
        Float(f) => (type_cast(t), Value::Float(*f)),
        String(s) => {
            let id = c.id.register();
            let (constant, gid) = constant(c, l);
            let stype = constant.get_type();
            let ptr = (Type::Pointer(Box::new(stype.clone())), gid);
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
fn value(c: &mut Context, val: &ast::Value, v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    use ast::Value::*;
    Some(vec![match val {
        Unit => unimplemented!(),
        Literal(l, t) => literal(c, l, &t, v),
        Variable(var) => variable(c, var, v)
    }])
}

#[test]
fn constant_test()
{
    use crate::Output;

    let mut m = Module::new("test");
    let mut id = Id::new();
    let mut c = Context {
        m: &mut m,
        id: &mut id
    };
    let l = ast::Literal::String("hello world".into());
    let (c, _) = constant(&mut c, &l);
    c.output(&mut std::io::stdout());
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

fn bin(c: &mut Context, b: &ast::BinaryExpr, v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    use ast::BinaryExpr::*;
    let (op, t, id) = match b {
        Add(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            let id = c.id.register();
            (Operation::Add(id.clone(), e1, e2), t, id)
        },
        Sub(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            let id = c.id.register();
            (Operation::Sub(id.clone(), e1, e2), t, id)
        },
        Mul(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            let id = c.id.register();
            (Operation::Mul(id.clone(), e1, e2), t, id)
        },
        Div(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            let id = c.id.register();
            if signed(&t) {
                (Operation::Sdiv(id.clone(), e1, e2), t, id)
            } else {
                (Operation::Udiv(id.clone(), e1, e2), t, id)
            }
        },
        Mod(a, b) => {
            let (t, e1, e2) = bin_expr(c, (a, b), v);
            let id = c.id.register();
            if signed(&t) {
                (Operation::Srem(id.clone(), e1, e2), t, id)
            } else {
                (Operation::Urem(id.clone(), e1, e2), t, id)
            }
        }
    };

    v.push(Inst::new(op, t.clone()));
    Some(vec![(t, Value::Reg(id))])
}

fn ret(c: &mut Context, r: &ast::Return, v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    let value = match &r.expr {
        None => return None,
        Some(e) => Some({
            expr(c, e, v).unwrap()[0].1.clone()
        })
    };

    let t = type_cast(&r.dtype);
    let op = Operation::Ret(value);
    v.push(Inst::new(op, t.clone()));
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

fn call(c: &mut Context, e: &ast::CallExpr, v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
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

fn expr(c: &mut Context, e: &ast::Expr, v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    use ast::Expr::*;
    match e {
        Value(e) => value(c, e, v),
        Binary(b) => bin(c, b, v),
        If(f) => { conditional(c, f, v); None },
        While(w) => { loop_while(c, w, v); None },
        Return(r) => ret(c, r, v),
        Assign(a) => { assign(c, a, v); None },
        Call(e) => call(c, e, v),
        _ => unimplemented!()
    }
}

fn assign(c: &mut Context, a: &ast::Assign, v: &mut Vec<Inst>)
{
    let l = Local::new(&a.id);
    let t = type_cast(&a.dtype);

    if let Some(expr) = expr(c, &a.expr, v) {
        let (t, val) = &expr[0];
        let r = Rc::new(Register::from(&l));
        let alloc = Operation::Alloca(r.clone(), None);
        let store = Operation::Store(val.clone(), t.clone(), r.clone());
        v.push(Inst::new(alloc, t.clone()));
        v.push(Inst::new(store, t.clone()));
        c.id.insert((*r).as_ref(), VarType::Ref);
    } else {
        unreachable!();
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

fn conditional_else(c: &mut Context, br: &ast::IfExpr, v: &mut Vec<Inst>)
{
    let (_, val) = bexpr(c, &br.cond, v);
    let (start, sop) = c.id.label();
    let (els, elop) = c.id.label();
    let (ends, endop) = c.id.label();
    let op = Operation::Br(val, Register::new(&start), Register::new(&els));
    let jmp = Operation::BrCond(Register::new(&ends));
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
    v.push(Inst::new(endop, Type::Label));
}

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
    for e in &w.expr {
        expr(c, e, v);
    }
    let back = Operation::BrCond(Register::new(&br));
    v.push(Inst::new(back, Type::Label));
    v.push(Inst::new(endop, Type::Label));
}

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
                      .map(|a| (type_cast(&a.1), Register::from(&Local::new(&a.0)))).collect())
    };

    if let Some(param) = &param {
        for (t, r) in param {
            c.id.insert(r.as_ref(), VarType::Val);
        }
    }

    let mut f = Function::new(name, param, ret, None);
    let mut v = Vec::new();

    let (_, eop) = c.id.label();
    for e in &func.expr {
        expr(c, e, &mut v);
    }
    if v.len() > 1 {
        entry(&mut v, eop);
    }
    for inst in &v {
        f.append(inst.clone());
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

    for r in &tree.records {
        types_cast(r.1);
    }

    for f in &tree.functions {
        let function = {
            let mut context = Context::new(
                &mut module, &mut id, FunInfo::new()
            );
            function(&mut context, f)
        };
        module.append(function);
    }
    module
}
