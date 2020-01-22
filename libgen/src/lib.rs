
extern crate libast;

pub use libast::*;

pub mod llvm;

pub trait Output {
    fn output<W>(&self, w: &mut W)
        where W: std::io::Write;
}
