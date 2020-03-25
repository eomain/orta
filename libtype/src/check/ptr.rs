
use super::*;
use libast::Address;
use libast::Deref;

// Check we can obtain the address of a variable.
pub fn address(i: &mut Info, s: &mut Scope, a: &mut Address,
               expt: Option<DataType>) -> Result<(), Error>
{
    let dtype = match s.find_var_type(&a.id) {
        None => return Err(error!("undefined variable '{}'", &a.id)),
        Some(t) => t
    };

    if let Some(expt) = expt {
        if expt != *dtype {
            return Err(error!("expected '{}', found '{}'", expt, dtype));
        }
    }

    a.dtype = DataType::Pointer(dtype.clone());
    Ok(())
}

// Check if an expression is a pointer that can be
// derefenced.
pub fn deref(i: &mut Info, s: &mut Scope, d: &mut Deref,
             expt: Option<DataType>) -> Result<(), Error>
{
    if !i.is_unsafe() {
        return Err(error!("cannot derefence ^ pointer, outside an 'unsafe'"));
    }

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
