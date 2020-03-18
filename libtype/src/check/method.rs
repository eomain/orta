
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
    a.dtype = DataType::Record(i.at.clone().unwrap());
    Ok(())
}

pub fn method(i: &mut Info, s: &mut Scope,
              m: &mut MethodAccess, expt: Option<DataType>) -> Result<(), Error>
{
    expr(i, s, &mut *m.expr, expt.clone())?;
    call(i, s, &mut m.call, expt)?;
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
