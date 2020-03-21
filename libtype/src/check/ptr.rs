
use super::*;
use libast::Address;
use libast::Deref;

pub fn address(i: &mut Info, s: &mut Scope, a: &mut Address,
               expt: Option<DataType>) -> Result<(), Error>
{
    let dtype = match s.find_var_type(&a.id) {
        Err(_) => return Err(error!("undefined variable {}", &a.id)),
        Ok(t) => t
    };
    a.dtype = DataType::Pointer(Rc::new(dtype.clone()));
    Ok(())
}

pub fn deref(i: &mut Info, s: &mut Scope, d: &mut Deref,
             expt: Option<DataType>) -> Result<(), Error>
{
    expr(i, s, &mut d.expr, match expt {
        None => None,
        Some(t) => Some(DataType::Pointer(Rc::new(t)))
    })?;

    let dtype = &d.expr.get_type();
    match dtype {
        DataType::Pointer(t) => {
            d.dtype = (**t).clone();
        },
        _ => return Err(error!("expected pointer ^, found '{}'", dtype))
    }

    Ok(())
}
