
use super::llvm::*;
use super::libast as ast;

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

// Convert an AST value into a LLVM value
fn value(val: &ast::Value) -> Value
{
    use ast::Value::*;
    match val {
        Literal(l) => {
            use ast::Literal::*;
            match l {
                Signed(i) => Value::Int(*i),
                Unsigned(u) => Value::Uint(*u),
                _ => unimplemented!()
            }
        },
        _ => unimplemented!()
    }
}

fn bin(b: &ast::BinaryExpr, v: &mut Vec<Inst>)
{
    use ast::BinaryExpr::*;
    match b {
        Add(a, b) => {

        },
        _ => unimplemented!()
    }
}

fn ret(r: &ast::Return, v: &mut Vec<Inst>)
{
    use ast::Expr;
    let value = match &r.expr {
        None => None,
        // TODO: value
        Some(e) => Some(match &**e {
            Expr::Value(v) => value(&v),
            _ => unimplemented!()
        })
    };
    let op = Operation::Ret(value);
    v.push(Inst::new(op, type_cast(&r.dtype)));
}

fn expr(e: &ast::Expr, v: &mut Vec<Inst>)
{
    use ast::Expr::*;
    match e {
        Binary(b) => bin(b, v),
        Return(r) => ret(r, v),
        _ => unimplemented!()
    }
}

// Convert an AST function into an LLVM function
fn function(func: &ast::Function) -> Function
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
        expr(e, &mut v);
    }
    for inst in &v {
        f.append(inst.clone());
    }
    f
}

// Create a new LLVM module from an AST
pub fn main(name: &str, tree: &ast::SyntaxTree) -> Module
{
    let mut module = Module::new(name);
    for f in &tree.functions {
        module.append(function(f));
    }
    module
}
