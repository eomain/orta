
use super::*;
use libast::Address;

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
