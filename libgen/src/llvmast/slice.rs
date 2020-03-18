
use super::*;
use libast::Expr;
use libast::Slice;
use libast::SliceExpr;
use libast::DataType;
use libast::IntType;

// Get the name of a slice
pub fn name(s: &Slice) -> String
{
    format!("sl.{}", &s.ptr)
}

// Convert a slice into an LLVM type
pub fn slice_type(s: &Slice) -> Types
{
    let id = name(s);
    let gid = Rc::new(Register::new(&id));

    Types::new(gid, vec![
        Type::Pointer(Box::new(type_cast(&s.ptr))),
        Type::Uint(64)
    ])
}

fn slice_into(c: &mut Context, t: Type, val: Value,
              index: usize, v: &mut Vec<Inst>) -> Register
{
    let ptr = (Type::Pointer(Box::new(t.clone())), val);
    let index = vec![
        (Type::Int(32), Value::Uint(0)),
        (Type::Int(32), Value::Uint(index))
    ];

    let id = c.id.register();
    let op = Operation::GetElPtr(id.clone(), ptr, index);
    v.push(Inst::new(op, t));
    id
}

// Index into a slice
fn slice_index(c: &mut Context, e: &Expr, index: usize,
               v: &mut Vec<Inst>) -> (Type, Value)
{
    let (dtype, val) = unary_expr(c, e, v);
    let id = slice_into(c, dtype.clone(), val, index, v);
    (dtype, Value::Reg(id))
}

// Get the pointer within the slice
fn ptr(c: &mut Context, e: &Expr,
       v: &mut Vec<Inst>) -> (Type, Value)
{
    slice_index(c, e, 0, v)
}

// Get the length of the slice
fn len(c: &mut Context, e: &Expr,
       v: &mut Vec<Inst>) -> (Type, Value)
{
    slice_index(c, e, 1, v)
}

// Evaluate slice expression
pub fn slice(c: &mut Context, s: &SliceExpr,
             v: &mut Vec<Inst>) -> (Type, Value)
{
    use SliceExpr::*;
    match s {
        Ptr(e) => ptr(c, &*e, v),
        Len(e) => len(c, &*e, v)
    }
}

pub fn string(c: &mut Context, s: &String, val: Value,
              v: &mut Vec<Inst>)
{
    use DataType::Integer;
    use IntType::S8;

    let s = Slice::new(Integer(S8));
    let t = slice_type(&s);
    let t = Type::from(&t);
    let r = Rc::new(c.id.register());

    let op = Operation::Alloca(r.clone(), None);
    v.push(Inst::new(op, t.clone()));

    let id = slice_into(c, t.clone(), Value::Reg((*r).clone()), 0, v);
    let op = Operation::Store(val, Type::Pointer(Box::new(Type::Int(8))), r);
    unimplemented!();
}

#[cfg(test)]
mod tests {
    use crate::Output;
    use super::*;
    use libast::*;
    use libast::Value;
    use libast::Slice;
    use libast::DataType;
    use libast::DataType::*;
    use libast::IntType::*;

    #[test]
    fn slice_test()
    {
        let s = Slice::new(Integer(S8));
        let t = slice_type(&s);
        t.output(&mut std::io::stdout());
    }

    #[test]
    fn slice_expr_test()
    {
        let mut m = Module::new("test");
        let mut id = Id::new();
        let mut c = Context::new(&mut m, &mut id, FunInfo::new(&ast::DataType::Unit));

        let s = Slice::new(Integer(S8));
        let v = Expr::Value(Value::Variable(
            Variable { name: "test".into(), dtype: DataType::from(s) }
        ));

        let mut vec = Vec::new();
        ptr(&mut c, &v, &mut vec);
        len(&mut c, &v, &mut vec);

        while vec.len() > 0 {
            println!("{}", std::string::String::from(vec.remove(0)));
        }
    }

}
