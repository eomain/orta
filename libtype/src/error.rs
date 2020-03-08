
use std::fmt;
use libsym::Error as SError;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    //Id,
    Symbol(SError),
    Custom(String),
    SubError(String, Box<Error>),
}

impl From<&Error> for String {
    fn from(e: &Error) -> Self
    {
        use Error::*;
        match e {
            //Id => "".into(),
            Symbol(s) => String::from(s),
            Custom(s) => s.clone(),
            SubError(s, e) => {
                let mut s = s.clone();
                let e = String::from(&(**e));
                for l in e.lines() {
                    s.push_str(&format!("    {}\n", l));
                }
                s
            }
        }
    }
}

impl From<SError> for Error {
    fn from(s: SError) -> Self
    {
        Error::Symbol(s)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", String::from(self))
    }
}
