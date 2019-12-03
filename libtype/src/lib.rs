
extern crate libsym;
extern crate libast;

use libsym::Scope;
use libsym::Table;
use libsym::TypeInfo;
use libast::Variable;

#[derive(Debug)]
pub enum Error {
    Id
}

#[derive(Debug)]
pub struct Env<'a> {
    table: &'a Table<'a>
}

mod check {
    use super::Error;
    use libsym::Scope;
    use libsym::TypeInfo;
    use libast::Expr;

    fn id(scope: &Scope, name: &str) -> Result<TypeInfo, Error>
    {
        if let Some(info) = scope.find(name) {
	    Ok(info)
        } else {
	    Err(Error::Id)
	}
    }

    fn expr(scope: &Scope, expr: Expr) -> Result<TypeInfo, Error>
    {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
   
}
