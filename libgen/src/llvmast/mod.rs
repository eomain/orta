
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
        _ => unimplemented!()
    }
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

fn literal(c: &mut Context, l: &ast::Literal, v: &mut Vec<Inst>) -> Value
{
    use ast::Literal::*;
    match l {
        Signed(i) => Value::Int(*i),
        Unsigned(u) => Value::Uint(*u),
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
            Value::Reg(id)
        },
        _ => unimplemented!()
    }
}

// Convert an AST value into a LLVM value
fn value(c: &mut Context, val: &ast::Value, v: &mut Vec<Inst>) -> Option<Vec<Value>>
{
    use ast::Value::*;
    Some(vec![match val {
        Unit => unimplemented!(),
        Literal(l) => literal(c, l, v),
        Variable(_) => unimplemented!()
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

fn bin(c: &mut Context, b: &ast::BinaryExpr, v: &mut Vec<Inst>) -> Option<Vec<Value>>
{
    use ast::BinaryExpr::*;
    match b {
        Add(a, b) => {

        },
        _ => unimplemented!()
    }

    None
}

fn ret(r: &ast::Return, v: &mut Vec<Inst>) -> Option<Vec<Value>>
{
    /*use ast::Expr;
    let value = match &r.expr {
        None => None,
        // TODO: value
        Some(e) => Some(match &**e {
            Expr::Value(v) => value(&v),
            _ => unimplemented!()
        })
    };
    let op = Operation::Ret(value);
    v.push(Inst::new(op, type_cast(&r.dtype)));*/

    None
}

fn call(c: &mut Context, e: &ast::CallExpr, v: &mut Vec<Inst>) -> Option<Vec<Value>>
{
    let id = GlobalId::new(&e.name);
    // TODO
    let rtype = Type::Void;
    let reg: Option<Register> = if rtype == Type::Void {
        None
    } else {
        Some(c.id.register())
    };

    let mut args = if e.args.len() > 0 {
        let mut exprs = Vec::new();
        let t = Type::Pointer(Box::new(Type::Int(8)));
        for exp in &e.args {
            if let Some(e) = expr(c, exp, v) {
                let mut e = e.iter().map(|a| (t.clone(), a.clone())).collect();
                exprs.append(&mut e);
            }
        }
        Some(exprs)
    } else {
        None
    };


    let op = Operation::Call(reg.clone(), id, args);
    v.push(Inst::new(op, rtype));

    if let Some(r) = reg {
        Some(vec![Value::Reg(r)])
    } else {
        None
    }
}

fn expr(c: &mut Context, e: &ast::Expr, v: &mut Vec<Inst>) -> Option<Vec<Value>>
{
    use ast::Expr::*;
    match e {
        Value(e) => value(c, e, v),
        Binary(b) => bin(c, b, v),
        Return(r) => ret(r, v),
        Call(e) => call(c, e, v),
        _ => unimplemented!()
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

// Create a new LLVM module from an AST
pub fn main(name: &str, tree: &ast::SyntaxTree) -> Module
{
    let mut module = Module::new(name);
    let mut id = Id::new();

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
