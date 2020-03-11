
mod librt;

use std::collections::HashMap;
use libast::DataType;
use libast::ParamList;

// The symbol used to refernce the type information
pub type Id = String;

// Function signature (parameters, return type)
pub type Signature = (Vec<DataType>, DataType);

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    Redefined(String),
    Undefined(String),
    NotFunction(String),
    NotVariable(String),
    Custom(String)
}

impl From<&Error> for String {
    fn from(e: &Error) -> Self
    {
        use Error::*;
        match e {
            Redefined(s) => format!("redefined symbol '{}'", s),
            Undefined(s) => format!("undefined symbol '{}'", s),
            NotFunction(s) => format!("symbol {} is not a function", s),
            NotVariable(s) => format!("symbol {} is not a variable", s),
            Custom(s) => s.clone()
        }
    }
}

// The type information
#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    Var(DataType),
    Function(Signature)
}

impl TypeInfo {
    fn is_function(&self) -> bool
    {
        if let TypeInfo::Function(_) = self {
            true
        } else {
            false
        }
    }
}

// A map of names to type info
pub type Entry = HashMap<Id, (TypeInfo, bool)>;

// Represents a single scope within a program
#[derive(Debug, Clone, PartialEq)]
pub struct Scope<'a> {
    // An optional parent scope.
    // This should only be `None` for
    // the global scope.
    parent: Option<&'a Scope<'a>>,
    entry: Entry
}

impl<'a> Scope<'a> {
    pub fn new(parent: Option<&'a Scope<'a>>) -> Self
    {
        Self {
            parent,
            entry: Entry::new()
        }
    }

    pub fn insert(&mut self, id: &str, t: (TypeInfo, bool))
    {
        self.entry.insert(id.into(), t);
    }

    pub fn insert_var(&mut self, id: &str, v: DataType, f: bool)
    {
        self.entry.insert(id.into(), (TypeInfo::Var(v), f));
    }

    pub fn find(&self, id: &str) -> Option<&(TypeInfo, bool)>
    {
        match self.entry.get(id) {
            None => {
                if let Some(p) = self.parent {
                    p.find(id)
                } else {
                    None
                }
            },
            Some(t) => Some(&t)
        }
    }

    pub fn is_final(&self, id: &str) -> bool
    {
        match self.find(id) {
            None => false,
            Some(t) => t.1
        }
    }

    pub fn get_type(&self, id: &str) -> Result<DataType, Error>
    {
        match self.find(id) {
            None => Err(Error::Undefined(id.into())),
            Some(t) => {
                match &t.0 {
                    TypeInfo::Var(dt) => Ok(dt.clone()),
                    TypeInfo::Function((v, r)) => {
                        Ok(DataType::Function(v.clone(), Box::new(r.clone())))
                    }
                }
            }
        }
    }

    pub fn find_type(&self, id: &str) -> Result<(DataType, bool), Error>
    {
        match self.find(id) {
            None => Err(Error::Undefined(id.into())),
            Some(t) => {
                match &t.0 {
                    TypeInfo::Var(dt) => Ok((dt.clone(), t.1)),
                    TypeInfo::Function((v, r)) => {
                        Ok((DataType::Function(v.clone(), Box::new(r.clone())), t.1))
                    }
                }
            }
        }
    }

    pub fn find_var(&self, id: &str) -> Result<(DataType, bool), Error>
    {
        match self.find(id) {
            None => Err(Error::Undefined(id.into())),
            Some(t) => {
                if let TypeInfo::Var(dt) = &t.0 {
                    Ok((dt.clone(), t.1))
                } else {
                    Err(Error::NotVariable(id.into()))
                }
            }
        }
    }

    pub fn find_var_type(&self, id: &str) -> Result<&DataType, Error>
    {
        match self.find(id) {
            None => Err(Error::Undefined(id.into())),
            Some((t, _)) => {
                if let TypeInfo::Var(t) = t {
                    Ok(&t)
                } else {
                    Err(Error::NotVariable(id.into()))
                }
            }
        }
    }

    pub fn find_fun_type(&self, id: &str) -> Result<&Signature, Error>
    {
        match self.find(id) {
            None => Err(Error::Undefined(id.into())),
            Some((t, _)) => {
                if let TypeInfo::Function(t) = t {
                    Ok(&t)
                } else {
                    Err(Error::NotFunction(id.into()))
                }
            }
        }
    }

    // Checks if the identifier can be found
    // in the current scope.
    #[inline]
    pub fn contains(&self, id: &str) -> bool
    {
        self.entry.contains_key(id)
    }
}

// A table to bind symbols
#[derive(Debug, Clone, PartialEq)]
pub struct Table<'a> {
    // the global scope
    pub global: Scope<'a>
}

impl<'a> Table<'a> {
    pub fn new() -> Self
    {
        let global = {
            librt::insert(Scope::new(None))
        };
        Self {
            global
        }
    }

    pub fn insert(&mut self, id: &str, t: (TypeInfo, bool))
    {
        self.global.insert(id, t);
    }

    pub fn scope(&self) -> Scope
    {
        Scope::new(Some(&self.global))
    }

    pub fn has_main(&self) -> bool
    {
        if let Ok(_) = self.global.find_fun_type("main") {
            return true;
        }
        false
    }
}

#[cfg(test)]
mod tests {

}
