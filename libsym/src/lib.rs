
mod librt;

use std::rc::Rc;
use std::collections::HashMap;
use libast::DataType;
use libast::DataRecord;
use libast::ParamList;

pub type Map<K, V> = HashMap<K, V>;

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
    NotNamed(String),
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
            NotNamed(s) => format!("symbol {} is not a named type", s),
            Custom(s) => s.clone()
        }
    }
}

// The defined kind of a variable
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum VarKind {
    // A static variable
    Static,
    // A temporary variable i.e.
    // allocated on the stack
    Temp
}

// A compile time constant
#[derive(Debug, Clone, PartialEq)]
pub struct Constant {

}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub kind: VarKind,
    //pub assigned: bool
}

impl Variable {
    pub fn new(kind: VarKind) -> Self
    {
        Self {
            kind
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub r#unsafe: bool,
    pub external: bool
}

impl Function {
    pub fn new(r#unsafe: bool, external: bool) -> Self
    {
        Self {
            r#unsafe,
            external
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Structure {
    pub methods: Map<String, Info>
}

impl Structure {
    pub fn from(methods: Map<String, Info>) -> Self
    {
        Self {
            methods
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition {
    Constant(Constant),
    Variable(Variable),
    Function(Function),
    Structure(Structure),
    TypeName
}

// Contains all the info about symbols
#[derive(Debug, Clone, PartialEq)]
pub struct Info {
    // The type of the symbol
    pub dtype: Rc<DataType>,
    // The type definition
    pub definition: Definition,
    // If the symbol type is final
    pub absolute: bool,
    // If the symbol is a global
    //pub global: bool,
    // Is a named type
    //pub named: bool,
    // Is a foregin symbol
    //pub foreign: bool
}

impl Info {
    pub fn new(definition: Definition, dtype: Rc<DataType>,
               absolute: bool) -> Self
    {
        Self {
            dtype,
            definition,
            absolute
        }
    }

    pub fn abs(&self) -> bool
    {
        self.absolute
    }
}

// A map of names to type info
pub type Entry = Map<Id, Info>;

// Represents a single scope within a program
#[derive(Debug, Clone, PartialEq)]
pub struct Scope<'a> {
    // An optional parent scope.
    // This should only be `None` for
    // the global scope.
    parent: Option<&'a Scope<'a>>,
    // The set of symbol entries within
    // the current scope
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

    pub fn root(&self) -> bool
    {
        match self.parent {
            None => true,
            _ => false
        }
    }

    pub fn insert(&mut self, id: Id, info: Info)
    {
        self.entry.insert(id, info);
    }

    pub fn find(&self, id: &str) -> Option<&Info>
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

    pub fn find_definition(&self, id: &str) -> Option<&Definition>
    {
        match self.entry.get(id) {
            None => {
                if let Some(p) = self.parent {
                    p.find_definition(id)
                } else {
                    None
                }
            },
            Some(t) => Some(&t.definition)
        }
    }

    pub fn find_struct_method(&self, name: &str, method: &str) -> Option<&Info>
    {
        match self.find_definition(name) {
            Some(Definition::Structure(s)) => {
                s.methods.get(method)
            },
            _ => None
        }
    }

    pub fn find_var_type(&self, id: &str) -> Option<Rc<DataType>>
    {
        match self.find(id) {
            None => None,
            Some(info) => {
                match info.definition {
                    Definition::Variable(_) => Some(info.dtype.clone()),
                    _ => None
                }
            }
        }
    }

    pub fn find_fun_type(&self, id: &str) -> Option<Rc<DataType>>
    {
        match self.find(id) {
            None => None,
            Some(info) => {
                match info.definition {
                    Definition::Function(_) => Some(info.dtype.clone()),
                    _ => None
                }
            }
        }
    }

    pub fn find_typename(&self, id: &str) -> Option<Rc<DataType>>
    {
        match self.find(id) {
            None => None,
            Some(info) => {
                match info.definition {
                    Definition::TypeName => Some(info.dtype.clone()),
                    _ => None
                }
            }
        }
    }

    pub fn get_type(&self, id: &str) -> Option<Rc<DataType>>
    {
        match self.find(id) {
            None => None,
            Some(e) => Some(e.dtype.clone())
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
    pub fn new(rtlib: bool) -> Self
    {
        let global = {
            if rtlib {
                librt::insert(Scope::new(None))
            } else {
                Scope::new(None)
            }
        };
        Self {
            global
        }
    }

    pub fn insert(&mut self, id: &str, info: Info)
    {
        self.global.insert(id.into(), info);
    }

    pub fn contains(&self, id: &str) -> bool
    {
        self.global.contains(id)
    }

    pub fn scope(&self) -> Scope
    {
        Scope::new(Some(&self.global))
    }
}

#[cfg(test)]
mod tests {

}
