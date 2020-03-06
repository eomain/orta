
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
pub type Entry = HashMap<Id, TypeInfo>;

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

    pub fn insert(&mut self, id: &str, t: TypeInfo)
    {
        self.entry.insert(id.into(), t);
    }

    pub fn find(&self, id: &str) -> Option<&TypeInfo>
    {
        match self.entry.get(id) {
            None => {
                if let Some(p) = self.parent {
                    p.find(id)
                } else {
                    None
                }
            },
            Some(t) => Some(t)
        }
    }

    #[inline]
    pub fn contains(&self, id: &str) -> bool
    {
        if let Some(_) = self.find(id) {
            true
        } else {
            false
        }
    }
}

mod librt {
    use super::Signature;
    use super::TypeInfo;
    use super::Scope;
    use libast::DataType;
    use libast::IntType::*;

    pub fn insert(mut s: Scope) -> Scope
    {
        s.insert("print", TypeInfo::Function(
            (vec![DataType::String], DataType::Unit)
        ));
        s.insert("iprint", TypeInfo::Function(
            (vec![DataType::Integer(S64)], DataType::Unit)
        ));
        s.insert("exit", TypeInfo::Function(
            (vec![DataType::Integer(S32)], DataType::Unit)
        ));
        s
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

    pub fn insert(&mut self, id: &str, t: TypeInfo)
    {
        self.global.insert(id, t);
    }

    pub fn scope(&self) -> Scope
    {
        Scope::new(Some(&self.global))
    }

    pub fn has_main(&self) -> bool
    {
        if let Some(sym) = self.global.find("main") {
            if let TypeInfo::Function(_) = sym {
                return true;
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {

}
