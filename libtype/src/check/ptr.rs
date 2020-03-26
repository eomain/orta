
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

fn can_deref(dtype: &DataType) -> bool
{
    use DataType::*;
    match dtype {
        Integer(_) | Float(_) | Boolean | Char | Pointer(_) => true,
        Array(dtype) => can_deref(&(**dtype).dtype),
        _ => false
    }
}

// Check if an expression is a pointer that can be
// dereferenced.
pub fn deref(i: &mut Info, s: &mut Scope, d: &mut Deref,
             expt: Option<DataType>) -> Result<(), Error>
{
    if !i.is_unsafe() {
        return Err(error!("cannot dereference ^ pointer, outside an 'unsafe'"));
    }

    expr(i, s, &mut d.expr, match expt {
        None => None,
        Some(t) => Some(DataType::Pointer(Rc::new(t)))
    })?;

    let dtype = &d.expr.get_type();
    match dtype {
        DataType::Pointer(t) => {
            if can_deref(&**t) {
                d.dtype = (**t).clone();
            } else {
                return Err(error!("cannot dereference, type '{}'", t));
            }
        },
        _ => return Err(error!("expected pointer ^, found '{}'", dtype))
    }

    Ok(())
}
