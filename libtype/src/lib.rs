
extern crate libsym;
extern crate libast;

#[macro_use]
mod error;
mod check;

use std::collections::HashMap;
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

fn update_functions(table: &mut Table, functions: &mut Vec<Function>) -> Result<(), Error>
{
    for f in functions {
        for (_, v) in &mut f.param.map {
            if let DataType::Named(s) = &mut v.0 {
                match table.global.find_named_type(s) {
                    Err(e) => return Err(e.into()),
                    Ok(t) => v.0 = t.clone()
                }
            }
        }
    }
    Ok(())
}

fn insert_declarations(global: &mut Table, declarations: &Vec<FunctionDec>)
{
    for d in declarations {
        let args = d.param.clone();
        let ret = d.ret.clone();
        global.insert(&d.name, (TypeInfo::Function((args, ret)), true));
    }
}

fn update_declarations(table: &mut Table,
                       declarations: &mut Vec<FunctionDec>) -> Result<(), Error>
{
    for d in declarations {
        for p in &mut d.param {
            if let DataType::Named(s) = p {
                match table.global.find_named_type(s) {
                    Err(e) => return Err(e.into()),
                    Ok(t) => *p = t.clone()
                }
            }
        }

        if let DataType::Named(s) = &mut d.ret {
            match table.global.find_named_type(s) {
                Err(e) => return Err(e.into()),
                Ok(t) => d.ret = t.clone()
            }
        }
    }
    Ok(())
}

/*fn update_types(table: &mut Table,
                types: &mut HashMap<String, DataType>) -> Result<(), Error>
{
    for (_, d) in types {
        if let DataType::Unique(u) = d {
            match types.get(&u.name) {
                None => return Err(error!("undefined type {}", u.dtype)),
                Some(t) => *u.dtype = (*t).clone()
            }
        }
    }
    Ok(())
}*/

fn insert_types(global: &mut Table, types: &HashMap<String, DataType>)
{
    for (k, t) in types {
        global.insert(k, (TypeInfo::Named(t.clone()), true));
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

    //update_types(global, &mut ast.types)?;
    insert_types(global, &ast.types);

    update_declarations(global, &mut ast.declarations)?;
    insert_declarations(global, &ast.declarations);

    update_functions(global, &mut ast.functions)?;
    insert_functions(global, &ast.functions);

    for (n, d) in &mut ast.defines {
        let rec = ast.records.get(n).unwrap();
        for m in &mut d.methods {
            let mut s = global.scope();
            check::mpass(&mut s, m, rec.clone())?;
        }
    }

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
