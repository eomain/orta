
extern crate libsym;
extern crate libast;

#[macro_use]
mod error;
mod check;

use libsym::Scope;
use libsym::Table;
use libsym::TypeInfo;
use libast::Variable;
use libast::DataType;
use libast::Function;
use libast::FunctionDec;
use libast::SyntaxTree;

pub use error::Error;

// Meta data to be used by the
// type checker
pub struct TypeMeta {
    // if the main function is required
    main: bool
}

impl TypeMeta {
    pub fn new(main: bool) -> Self
    {
        Self {
            main
        }
    }
}

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

fn insert_functions(global: &mut Table, functions: &Vec<Function>)
{
    for f in functions {
        let args = Vec::from(&f.param).iter()
                                      .map(|a| a.1.clone())
                                      .collect();
        let ret = f.ret.clone();
        global.insert(&f.name, (TypeInfo::Function((args, ret)), true));
    }
}

fn insert_declarations(global: &mut Table, declarations: &Vec<FunctionDec>)
{
    for d in declarations {
        let args = d.param.clone();
        let ret = d.ret.clone();
        global.insert(&d.name, (TypeInfo::Function((args, ret)), true));
    }
}

fn require_main(meta: &TypeMeta, global: &Table) -> Result<(), Error>
{
    if meta.main && !global.has_main() {
        return Err(error!("function 'main' undefined"));
    }
    Ok(())
}

pub fn init(meta: TypeMeta, ast: &mut SyntaxTree) -> Result<(), Error>
{
    let mut env = Env::new();
    let global = &mut env.table;

    insert_declarations(global, &ast.declarations);
    insert_functions(global, &ast.functions);

    for f in &mut ast.functions {
        let mut s = global.scope();
        check::fpass(&mut s, f)?;
    }

    require_main(&meta, global)?;

    Ok(())
}

#[cfg(test)]
mod tests {

}
