
use std::collections::HashMap;

// The symbol used to refernce the type information
pub type Id = String;

// The type information
pub enum TypeInfo {
    
}

// A map of names to type info
pub type Entry = HashMap<Id, TypeInfo>;

// Represents a single scope within a program
pub struct Scope<'a> {
    // An optional parent scope.
    // This should only be `None` for
    // the global scope.
    parent: Option<&'a Scope<'a>>,
    entry: Entry
}

impl<'a> Scope<'a> {
    pub fn find(&self, id: &str) -> Option<TypeInfo>
    {
        None
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

// A table to bind symbols
pub struct Table<'a> {
    // the global scope
    pub global: Scope<'a>
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
