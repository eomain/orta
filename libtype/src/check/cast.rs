
use super::*;
use libast::Cast;

fn castable(a: &DataType, b: &DataType) -> bool
{
    if (a == b) {
        return true;
    }

    use DataType::*;
    use IntType::*;
    match (a, b) {
        (Float(_), Float(_)) |
        (Integer(_), Integer(_)) |
        (Pointer(_), Pointer(_)) |
        (Float(_), Integer(_)) |
        (Integer(_), Float(_)) => true,
        (Integer(a), Pointer(_)) |
        (Pointer(_), Integer(a)) => !a.signed(),
        _ => false
    }
}

pub fn cast(i: &mut Info, s: &mut Scope,
            c: &mut Cast, expt: Option<DataType>) -> Result<(), Error>
{
    expr(i, s, &mut *c.expr, None)?;
    let (a, b) = (&c.dtype, c.expr.get_type());
    if !castable(a, b) {
        Err(error!("cannot cast type from `{}` into `{}`", b, a))
    } else {
        Ok(())
    }
}
