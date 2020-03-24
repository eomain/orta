
use super::*;
use libast::Cast;

fn castable(a: &DataType, b: &DataType) -> bool
{
    let (a, b) = (a.derived(), b.derived());
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
        (Integer(_), Float(_)) |
        (Integer(_), Char) |
        (Char, Integer(_)) |
        (Integer(_), Pointer(_)) |
        (Pointer(_), Integer(_)) => true,
        _ => false
    }
}

pub fn cast(i: &mut Info, s: &mut Scope,
            c: &mut Cast, expt: Option<DataType>) -> Result<(), Error>
{
    expr(i, s, &mut *c.expr, None)?;
    let (a, b) = (named(s, &c.dtype), c.expr.get_type());
    if !castable(a, b) {
        Err(error!("cannot cast type from `{}` into `{}`", b, a))
    } else {
        c.dtype = a.clone();
        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use std::rc::Rc;
    use super::*;
    use libast::Unique;
    use DataType::*;
    use IntType::*;

    #[test]
    fn cast_test()
    {
        let unique = Unique::new("Object", Integer(S32));
        assert_eq!(castable(&unique.into(), &Integer(S32)), true);
    }
}
