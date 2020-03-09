
extern crate libsym;
extern crate libast;

mod check;
mod error;

use libsym::Scope;
use libsym::Table;
use libsym::TypeInfo;
use libast::Variable;
use libast::DataType;
use libast::SyntaxTree;

pub use error::Error;

#[derive(Debug)]
struct Env<'a> {
    pub table: Table<'a>
}

impl<'a> Env<'a> {
    pub fn new() -> Self
    {
       Self {
           table: Table::new()
	   }
    }
}

/*mod efmt {
    use libast::DataType;

    pub fn args(args: &Vec<DataType>) -> String
    {
        let mut s = String::new();
        for i in 1..args.len() {
            s.push();
        }
        s
    }
}*/

pub fn init(ast: &mut SyntaxTree) -> Result<(), Error>
{
    let mut env = Env::new();

    for d in &ast.declarations {
        let args = d.param.clone();
        let ret = d.ret.clone();
        env.table.insert(&d.name, (TypeInfo::Function((args, ret)), true));
    }
    
    for f in &ast.functions {
        let args = Vec::from(&f.param).iter().map(|a| a.1.clone()).collect();
        let ret = f.ret.clone();
        env.table.insert(&f.name, (TypeInfo::Function((args, ret)), true));
    }

    for f in &mut ast.functions {
        let mut s = env.table.scope();
        check::fpass(&mut s, f)?;
    }

    if !env.table.has_main() {
        return Err(Error::Custom("function 'main' undefined".into()));
    }

    Ok(())
}

#[cfg(test)]
mod tests {

}
