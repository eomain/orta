
use std::rc::Rc;
use super::*;
use libast::DataRecord;
use libast::AtExpr;
use libast::Method;
use libast::FieldAccess;
use libast::MethodAccess;

pub fn at(i: &mut Info, s: &mut Scope,
          a: &mut AtExpr, expt: Option<DataType>) -> Result<(), Error>
{
    let dtype = match &i.at {
        None => return Err(error!("use of '@' outside of method")),
        Some(dtype) => dtype.clone()
    };
    a.dtype = DataType::Record(dtype);
    Ok(())
}

fn call(info: &mut Info, s: &mut Scope,
        c: &mut CallExpr, expt: Option<DataType>, name: &str) -> Result<(), Error>
{
    // get the argument types and the return type
    let (args, ret) = match s.find_struct_method(name, &c.name) {
        None => return Err(error!("undefined method '{}'", &c.name)),
        Some(info) => {
            if let DataType::Function(v, r) = &info.dtype.derived() {
                (v.clone(), r.clone())
            } else {
                return Err(error!("`{}` is not a method", &c.name));
            }
        }
    };

    let (a, b) = (args.len(), c.args.len());
    if a != b {
        return Err(error!(
            "incorrect number of positional arguments\n  found: {}\n  expected {}",
            b, a
        ));
    }

    let mut i = 0;
    c.rtype = (*ret).clone();
    for arg in &mut c.args {
        if let Err(e) = expr(info, s, arg, Some(args[i].clone())) {
            return Err(suberror!(e,
                "argument type error\n  invoked method: {}\n  positional argument: {}\n",
                &c.name, (i + 1)
            ));
        }
        let (found, expt) = (arg.get_type(), &args[i]);
        if found != expt {
            return Err(type_error(found, expt));
        }
        i += 1;
    }
    Ok(())
}

pub fn method(i: &mut Info, s: &mut Scope,
              m: &mut MethodAccess, expt: Option<DataType>) -> Result<(), Error>
{
    expr(i, s, &mut *m.expr, expt.clone())?;
    let rec = m.expr.get_type().derived().get_record();
    call(i, s, &mut m.call, expt, &rec.name)?;
    Ok(())
}

pub fn field(i: &mut Info, s: &mut Scope,
             f: &mut FieldAccess, expt: Option<DataType>) -> Result<(), Error>
{
    expr(i, s, &mut *f.expr, expt)?;
    let dtype = (f.expr.get_type());
    if let DataType::Record(r) = dtype {
        match r.attr.iter().find(|a| &a.0 == &f.field) {
            None => return Err(error!("field `{}` does not exist", &f.field)),
            Some(i) => { f.dtype = i.1.clone(); }
        }
    } else {
        return Err(error!("expected type struct found `{}`", dtype));
    }
    Ok(())
}

fn methods(i: &mut Info, s: &mut Scope, m: &mut Method) -> Result<(), Error>
{
    for e in &mut m.expr {
        expr(i, s, e, None)?;
    }

    Ok(())
}

pub fn mpass(s: &mut Scope, m: &mut Method, d: DataRecord) -> Result<(), Error>
{
    let mut info = Info::new(&m.name, m.ret.clone());
    info.at = Some(Rc::new(d));

    methods(&mut info, s, m)?;
    if info.second_pass() {
        info.pcount += 1;
        methods(&mut info, s, m)?;
    }

    if !ret::method_returns(&mut info, s, m) {
        return Err(error!("method: `{}`: expected return statement", &m.name));
    }

    Ok(())
}
