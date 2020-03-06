
use std::rc::Rc;
use super::llvm::*;
use super::libast as ast;

struct Id {
    lindex: usize,
    gindex: usize
}

impl Id {
    fn new() -> Self
    {
        Self {
            lindex: 1,
            gindex: 1
        }
    }

    fn register(&mut self) -> Register
    {
        let id = format!("{}", self.lindex);
        self.lindex += 1;
        Register::new(&id)
    }

    fn global(&mut self) -> GlobalId
    {
        let id = format!("cst{}", self.gindex);
        self.gindex += 1;
        GlobalId::new(&id)
    }

    fn reset(&mut self)
    {
        self.lindex = 1;
    }
}

struct Context<'a> {
    m: &'a mut Module,
    id: &'a mut Id
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
        Boolean => Type::Int(1),
        _ => {
            assert_ne!(dtype, &ast::DataType::Unset);
            unimplemented!()
        }
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
            let t = Type::Pointer(Box::new(Type::Int(8)));
            (t, Value::Reg(id))
        },
        _ => unimplemented!()
    }
}

fn variable(c: &mut Context, var: &ast::Variable, v: &mut Vec<Inst>) -> (Type, Value)
{
    let l = Local::new(&var.name);
    let t = type_cast(&var.dtype);

    let reg = c.id.register();
    let op = Operation::Load(reg.clone(), t.clone(), Rc::new(Register::from(&l)));
    v.push(Inst::new(op, t.clone()));

    (t, Value::Reg(reg))
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

// TODO:
/*fn value_to_type(v: &Value) -> Type
{
    match v {
        Value::Int(i) => Type::Int(64),
        Value::Uint(u) => Type::Uint(64),
        Value::Global(g) => Type::Pointer(Box::new(Type::Int(8))),
        _ => unimplemented!()
    }
}*/

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
    use ast::Expr;
    let value = match &r.expr {
        None => return None,
        Some(e) => Some(match &**e {
            Expr::Value(e) => value(c, e, v).unwrap()[0].1.clone(),
            _ => unimplemented!()
        })
    };
    let t = type_cast(&r.dtype);
    let op = Operation::Ret(value);
    v.push(Inst::new(op, t.clone()));
    Some(vec![(t, Value::Void)])
}

fn call(c: &mut Context, e: &ast::CallExpr, v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    let id = GlobalId::new(&e.name);
    let rtype = type_cast(&e.rtype);
    let reg: Option<Register> = if rtype == Type::Void {
        None
    } else {
        Some(c.id.register())
    };

    let mut args = if e.args.len() > 0 {
        let mut exprs = Vec::new();
        for exp in &e.args {
            if let Some(e) = expr(c, exp, v) {
                let mut e = e.iter().map(|a| (a.0.clone(), a.1.clone())).collect();
                exprs.append(&mut e);
            }
        }
        Some(exprs)
    } else {
        None
    };


    let op = Operation::Call(reg.clone(), id, args);
    v.push(Inst::new(op, rtype.clone()));

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
        Return(r) => ret(c, r, v),
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
        let store = Operation::Store(val.clone(), t.clone(), r);
        v.push(Inst::new(alloc, t.clone()));
        v.push(Inst::new(store, t.clone()));
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
                      .map(|a| (type_cast(&a.1), Register::new(&a.0))).collect())
    };

    let mut f = Function::new(name, param, ret, None);
    let mut v = Vec::new();

    for e in &func.expr {
        expr(c, e, &mut v);
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
            let mut context = Context {
                m: &mut module, id: &mut id
            };
            function(&mut context, f)
        };
        module.append(function);
    }
    module
}
