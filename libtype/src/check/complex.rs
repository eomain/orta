
use std::collections::HashMap;
use super::*;
use libast::ComplexLiteral as Complex;
use libast::StructLiteral as Struct;

pub fn structs(i: &mut Info, s: &mut Scope, c: &mut Struct,
               expt: Option<DataType>) -> Result<(), Error>
{
    let comp = match s.find_record_type(&c.name) {
        Err(_) => return Err(error!("undefined type struct `{}`", &c.name)),
        Ok(s) => s
    };
    unimplemented!()
    Ok(())
}

pub fn complex(i: &mut Info, s: &mut Scope, c: &mut Complex,
               expt: Option<DataType>) -> Result<(), Error>
{
    use Complex::*;
    match c {
        Struct(c) => structs(i, s, c, expt),
        _ => unimplemented!()
    }
}
