
use std::path::Path;

pub fn asm(s: &str) -> Option<String>
{
    let p = Path::new(s);
    match p.file_stem() {
        None => None,
        Some(f) => Some(format!("{}.s", f.to_str()?))
    }
}

pub fn obj(s: &str) -> Option<String>
{
    let p = Path::new(s);
    match p.file_stem() {
        None => None,
        Some(f) => Some(format!("{}.o", f.to_str()?))
    }
}

pub fn llvm(s: &str) -> Option<String>
{
    let p = Path::new(s);
    match p.file_stem() {
        None => None,
        Some(f) => Some(format!("{}.ll", f.to_str()?))
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test()
    {
        assert_eq!(Some("main.ll".into()), llvm("tests/main"));
        assert_eq!(Some("test.s".into()), asm("test.ll"));
        assert_eq!(Some("input.s".into()), asm("tests/input.ll"));
        assert_eq!(Some("input.s".into()), asm("tests/input"));
        assert_eq!(Some("input.o".into()), obj("tests/input.s"));
    }
}
