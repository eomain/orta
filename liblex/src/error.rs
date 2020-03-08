
use std::fmt;
use super::Cursor;

// The type returned if there is an error
// found while lexing.
#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    Invalid(String),
    CommentEnd(Cursor),
    QuouteEnd(Cursor),
    Custom(&'static str)
}

impl From<&Error> for String {
    fn from(e: &Error) -> String
    {
        use Error::*;
        match e {
            Invalid(s) => format!("found invalid sequence: `{}`", s),
            CommentEnd(c) => format!("expected a closing `*\\`, following the opening on {}", c),
            QuouteEnd(c) => format!("expected a closing `\"`, following the opening on {}", c),
            Custom(s) => (*s).into()
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", String::from(self))
    }
}
