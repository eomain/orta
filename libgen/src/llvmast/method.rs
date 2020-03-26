
use super::*;
use ast::AtExpr;
use ast::FieldAccess;
use ast::MethodAccess;

static METHOD_OBJECT: &str = "m.o";

fn name(mo: &str, name: &str) -> String
{
    format!("st.{}.{}", mo, name)
}

pub fn at(c: &mut Context, a: &AtExpr,
          v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    let r = Register::new(METHOD_OBJECT);
    let dtype = type_cast(&a.dtype);
    Some(vec![(dtype, Value::Reg(r))])
}

fn field_index(c: &mut Context, f: &FieldAccess, r: &Register,
               t: &Type, val: Value, v: &mut Vec<Inst>)
{
    let rec = f.expr.get_type().get_record().unwrap();
    let index = rec.attr.iter()
                        .position(|a| &a.0 == &f.field).unwrap() as isize;

    let op = Operation::GetElPtr(
        r.clone(), (Type::Pointer(Box::new(t.clone())), val),
        vec![
            (Type::Int(64), Value::Int(0)),
            (Type::Int(32), Value::Int(index))
        ]
    );
    v.push(Inst::new(op, t.clone()));
}

pub fn field(c: &mut Context, f: &FieldAccess,
             v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    let (t, val) =  unary_expr(c, &f.expr, v);
    let r = c.id.register();
    field_index(c, f, &r, &t, val, v);

    let dtype = type_cast(&f.dtype);
    let reg = complex::get(c, &f.dtype, dtype.clone(), r, v);

    Some(vec![(dtype, Value::Reg(reg))])
}

pub fn call(c: &mut Context, m: &MethodAccess,
            v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    let (t, val) =  unary_expr(c, &m.expr, v);
    let obj = (Type::Pointer(Box::new(t)), val);
    let e = &m.call;

    // Return type of function call
    let rtype = type_cast(&e.rtype);

    // Arguments to function (Type, Value)
    let args = match call_args(c, &e.args, v) {
        None => Some(vec![obj]),
        Some(mut v) => {
            v.insert(0, obj);
            Some(v)
        }
    };

    let rec = m.expr.get_type().derived().get_record().unwrap();
    let mo = &rec.name;
    // The id of the function
    let id = name(mo, &e.name);
    let id = Value::Global(Rc::new(GlobalId::new(&id)));

    call_op(c, id, args, rtype, v)
}

// Convert an AST method into an LLVM function
fn method(c: &mut Context, m: &ast::Method,
          at: Type, tname: &str) -> Function
{
    let name = name(tname, &m.name);
    let ret = type_cast(&m.ret);
    let mut paramlist: Vec<_> = (&m.param).into();
    let param: Option<Vec<(Type, Register)>> = {
        let mut p: Vec<_> = paramlist.iter()
                             .map(|a| (type_cast(&a.1), Register::from(&Local::new(&a.0))))
                             .collect();
        p.insert(0, (Type::Pointer(Box::new(at)), Register::new(METHOD_OBJECT)));
        Some(p)
    };

    if let Some(param) = &param {
        for (t, r) in param {
            c.id.insert(r.as_ref(), VarType::Val);
        }
    }

    let mut f = Function::new(&name, param, ret.clone(), None);
    let mut v = function_body(c, ret, &m.expr);

    while v.len() > 0 {
        f.append(v.remove(0));
    }

    c.id.reset();
    f
}

pub fn define(id: &mut Id, module: &mut Module,
              name: &str, d: &ast::Define) -> Vec<Function>
{
    let rec = id.types.get(name).unwrap();
    let at = Type::from(rec);
    let mut fun = Vec::new();
    for f in &d.methods {
        let mut context = Context::new(
            module, id, FunInfo::new(&f.ret)
        );
        fun.push(method(&mut context, f, at.clone(), name));
    }
    fun
}
