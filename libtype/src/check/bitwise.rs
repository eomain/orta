
use super::*;
use libast::BitExpr;

pub fn bit(i: &mut Info, s: &mut Scope,
           b: &mut BitExpr, expt: Option<DataType>) -> Result<(), Error>
{
    use DataType::*;
    use BitExpr::*;
    match b {
        And(a, b) |
        Or(a, b) |
        Xor(a, b) |
        Lsh(a, b) |
        Rsh(a, b) => {
            expr(i, s, a, expt.clone())?;
            expr(i, s, b, expt)?;
            let (a, b) = (a.get_type(), b.get_type());
            let (a, b) = (a.derived(), b.derived());

            match (a, b) {
                (Integer(_), Integer(_)) => (),
                _ => return Err(error!(
                    "cannot perform bitwise operation on types `{}` and `{}`", a, b
                ))
            }
        },
        Comp(a) => {
            expr(i, s, a, expt)?;
            let a = a.get_type().derived();
            match a {
                Integer(_) => (),
                _ => return Err(error!("cannot perform bitwise operation on type `{}`", a))
            }
        }
    }
    Ok(())
}
