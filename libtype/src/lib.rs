
extern crate libsym;
extern crate libast;

#[macro_use]
mod error;
mod check;

use std::rc::Rc;
use std::collections::HashMap;
use std::collections::HashSet;
use libsym::Scope;
use libsym::Table;
use libsym::Definition;
use libsym::Info;
use libsym::Function as Fun;
use libsym::Structure as Struct;
use libast::Variable;
use libast::Define;
use libast::DataType;
use libast::DataRecord;
use libast::Function;
use libast::FunctionDec;
use libast::SyntaxTree;

pub use error::Error;

// Meta data to be used by the
// type checker
pub struct TypeMeta {
    // if the main function is required
    main: bool,
    // include runtime library
    runtime: bool
}

impl TypeMeta {
    pub fn new(main: bool, runtime: bool) -> Self
    {
        Self {
            main,
            runtime
        }
    }
}

// Environment for a single module
#[derive(Debug)]
struct Env<'a> {
    // The symbol table
    pub table: Table<'a>,
    // Contains an entry point
    // within the module
    pub entry: bool
}

impl<'a> Env<'a> {
    fn new(meta: &TypeMeta) -> Self
    {
       Self {
           table: Table::new(meta.runtime),
           entry: meta.main
	   }
    }

    fn init(&mut self, ast: &mut SyntaxTree) -> Result<(), Error>
    {
        self.insert_types(&ast.types)?;
        self.insert_record(&ast.records, &ast.defines)?;
        self.update_declarations(&mut ast.declarations);
        self.insert_declarations(&ast.declarations)?;
        self.update_functions(&mut ast.functions);
        self.insert_functions(&ast.functions)?;
        self.require_main()?;
        Ok(())
    }

    fn update_named(&mut self, dtype: &mut DataType)
    {
        match dtype {
            DataType::Named(name) => {
                match self.table.global.get_type(name) {
                    None => (),
                    Some(t) => { *dtype = (*t).clone(); }
                }
            },
            DataType::Pointer(t) => {
                let mut n = (**t).clone();
                self.update_named(&mut n);
                *t = Rc::new(n);
            },
            _ => ()
        }
    }

    fn update_functions(&mut self, functions: &mut Vec<Function>)
    {
        for f in functions {
            for (_, v) in &mut f.param.map {
                self.update_named(&mut v.0);
            }
            self.update_named(&mut f.ret);
        }
    }

    fn update_declarations(&mut self, declarations: &mut Vec<FunctionDec>)
    {
        for d in declarations {
            for dtype in &mut d.param {
                self.update_named(dtype);
            }
            self.update_named(&mut d.ret);
        }
    }

    fn insert_functions(&mut self, functions: &Vec<Function>) -> Result<(), Error>
    {
        for f in functions {
            if self.table.contains(&f.name) {
                return Err(error!("redefinition of function '{}'", &f.name));
            }

            let args = Vec::from(&f.param).iter()
                                          .map(|a| a.1.clone())
                                          .collect();
            let ret = f.ret.clone();
            let dtype = DataType::from((args, ret));
            let d = Definition::Function(Fun::new(false, f.prop.external));
            let info = Info::new(d, Rc::new(dtype), true);

            self.table.insert(&f.name, info);
        }
        Ok(())
    }

    fn insert_declarations(&mut self, declarations: &Vec<FunctionDec>) -> Result<(), Error>
    {
        for dec in declarations {
            if self.table.contains(&dec.name) {
                return Err(error!("redefinition of function declaration '{}'", &dec.name));
            }

            let args = dec.param.clone();
            let ret = dec.ret.clone();

            let dtype = DataType::from((args, ret));
            let d = Definition::Function(Fun::new(false, true));
            let info = Info::new(d, Rc::new(dtype), true);

            self.table.insert(&dec.name, info);
        }
        Ok(())
    }

    fn insert_record(&mut self, types: &Vec<(String, DataRecord)>,
                     define: &Vec<(String, Define)>) -> Result<(), Error>
    {
        for (k, t) in types {
            if self.table.contains(k) {
                return Err(error!("redefinition of type struct '{}'", k));
            }

            let define: HashMap<_, _> = define.iter()
                                              .map(|a| (a.0.clone(), a.1.clone()))
                                              .collect();
            let map = match define.get(k) {
                Some(d) => {
                    let mut map = HashMap::new();
                    for m in &d.methods {
                        let args = Vec::from(&m.param).iter()
                                                      .map(|a| a.1.clone())
                                                      .collect();
                        let ret = m.ret.clone();

                        let dtype = DataType::from((args, ret));
                        let d = Definition::Function(Fun::new(false, false));
                        let info = Info::new(d, Rc::new(dtype), true);

                        map.insert(m.name.clone(), info);
                    }
                    map
                },
                _ => HashMap::new()
            };

            let s = Struct::from(map);
            let d = Definition::Structure(s);
            let info = Info::new(d, Rc::new(DataType::from(t.clone())), true);

            self.table.insert(k, info);
        }
        Ok(())
    }

    fn insert_types(&mut self, types: &Vec<(String, DataType)>) -> Result<(), Error>
    {
        for (k, t) in types {
            if self.table.contains(k) {
                return Err(error!("redefinition of type '{}'", k));
            }

            let d = Definition::TypeName;
            let info = Info::new(d, Rc::new(t.clone()), true);

            self.table.insert(k, info);
        }
        Ok(())
    }

    fn require_main(&self) -> Result<(), Error>
    {
        if !self.entry {
            return Ok(());
        }

        match self.table.global.find_fun_type("main") {
            None => Err(error!("function 'main' undefined")),
            Some(dtype) => match &*dtype {
                DataType::Function(param, ret) => {
                    if param.len() > 0 {
                        Err(error!("'main' function must not have parameters"))
                    } else if **ret != DataType::Unit {
                        Err(error!("'main' function must return unit `()`"))
                    } else {
                        Ok(())
                    }
                },
                _ => unreachable!()
            }
        }
    }
}

pub fn init(meta: TypeMeta, ast: &mut SyntaxTree) -> Result<(), Error>
{
    let mut env = Env::new(&meta);
    env.init(ast)?;

    let global = &mut env.table;

    let records: HashMap<_, _> = ast.records.iter()
                                            .map(|a| (a.0.clone(), a.1.clone()))
                                            .collect();
    for (n, d) in &mut ast.defines {
        match records.get(n) {
            Some(rec) => {
                for m in &mut d.methods {
                    let mut s = global.scope();
                    check::mpass(&mut s, m, rec.clone())?;
                }
            },
            _ => return Err(error!("found define block for undefined type struct '{}'", n))
        }
    }

    for f in &mut ast.functions {
        let mut s = global.scope();
        check::fpass(&mut s, f)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {

}
