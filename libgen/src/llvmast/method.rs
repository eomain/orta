
use super::*;

pub fn at(c: &mut Context, a: &ast::AtExpr,
          v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    let r = Register::new("mo");
    let dtype = type_cast(&a.dtype);
    Some(vec![(dtype, Value::Reg(r))])
}

pub fn field(c: &mut Context, f: &ast::FieldAccess,
             v: &mut Vec<Inst>) -> Option<Vec<(Type, Value)>>
{
    let (t, val) =  unary_expr(c, &f.expr, v);
    let r = c.id.register();
    let index = match &f.expr.get_type() {
        ast::DataType::Record(r) => {
            r.attr.iter().position(|a| &a.0 == &f.field).unwrap() as isize
        },
        _ => unreachable!()
    };
    let op = Operation::GetElPtr(
        r.clone(), (Type::Pointer(Box::new(t.clone())), val),
        vec![
            (Type::Int(64), Value::Int(0)),
            (Type::Int(32), Value::Int(index))
        ]
    );
    v.push(Inst::new(op, t.clone()));
    let reg = c.id.register();
    let dtype = type_cast(&f.dtype);
    let op = Operation::Load(reg.clone(), dtype.clone(), Rc::new(r));
    v.push(Inst::new(op, dtype));
    Some(vec![(t, Value::Reg(reg))])
}

// Convert an AST method into an LLVM function
fn method(c: &mut Context, m: &ast::Method,
          at: Type, tname: &str) -> Function
{
    let name = format!("st.{}.{}", tname, &m.name);
    let ret = type_cast(&m.ret);
    let mut paramlist: Vec<_> = (&m.param).into();
    let param: Option<Vec<(Type, Register)>> = {
        let mut p: Vec<_> = paramlist.iter()
                             .map(|a| (type_cast(&a.1), Register::from(&Local::new(&a.0))))
                             .collect();
        p.insert(0, (Type::Pointer(Box::new(at)), Register::new("mo")));
        Some(p)
    };

    if let Some(param) = &param {
        for (t, r) in param {
            c.id.insert(r.as_ref(), VarType::Val);
        }
    }

    let mut f = Function::new(&name, param, ret.clone(), None);
    let mut v = Vec::new();

    let (_, eop) = c.id.label();
    c.info.count = 1;
    c.info.total = m.expr.len();
    for e in &m.expr {
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
