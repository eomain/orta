
use crate::Error;
use crate::PResult;
use crate::ParseInfo;
use crate::token_or;
use super::*;
use libtoken::Token;
use libtoken::IntoToken;
use libtoken::Key;
use libast::Array;
use libast::ArrayLiteral;
use libast::Index;
use libast::ComplexLiteral as Complex;

// Get the size value of the array
fn size(info: &mut ParseInfo) -> PResult<usize>
{
    match info.next() {
        Some(&Token::Literal(Literal::Unsigned(u))) => Ok(u),
        _ => Err("expected size".into())
    }
}

// A single array subscript
fn subscript(info: &mut ParseInfo) -> PResult<usize>
{
    token!(Token::Lsqr, info.next())?;
    let size = size(info)?;
    token!(Token::Rsqr, info.next())?;
    Ok(size)
}

// A single array index
pub fn index(info: &mut ParseInfo, e: Expr) -> PResult<Expr>
{
    token!(Token::Lsqr, info.next())?;
    let index = expr(info)?;
    token!(Token::Rsqr, info.next())?;
    Ok(Expr::Index(Index::new(index, e)))
}

// Parse an array type definition
pub fn array(info: &mut ParseInfo) -> PResult<DataType>
{
    let mut sizes = Vec::new();
    sizes.push(subscript(info)?);
    while Some(&Token::Lsqr) == info.look() {
        sizes.push(subscript(info)?);
    }
    let dtype = types(info)?;
    Ok(DataType::from(Array::new(sizes, dtype)))
}

// Parse an array literal
pub fn literal(info: &mut ParseInfo) -> PResult<Complex>
{
    token!(Token::Lsqr, info.next())?;
    let mut exprs = Vec::new();
    exprs.push(expr(info)?);
    while Some(&Token::Comma) == info.look() {
        info.next();
        exprs.push(expr(info)?);
    }
    token!(Token::Rsqr, info.next())?;
    Ok(Complex::from(ArrayLiteral::new(exprs)))
}

#[cfg(test)]
mod tests {
    extern crate liblex;

    use super::*;

    #[test]
    fn array_test()
    {

        let tokens = liblex::scan(r#"
            [5] |[2] ^int|
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let a = array(&mut info).unwrap();
        println!("{}", a);
    }

    #[test]
    fn multi_array_test()
    {

        let tokens = liblex::scan(r#"
            [3][3] i8
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let a = array(&mut info).unwrap();
        println!("{}", a);
    }

    #[test]
    fn array_lit_test()
    {

        let tokens = liblex::scan(r#"
            [ 1, 3, 5 ]
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let l = literal(&mut info).unwrap();
        println!("{:?}", l);
    }

    #[test]
    fn index_test()
    {

        let tokens = liblex::scan(r#"
            data[i][2 * 2].run()
        "#.chars().collect()).unwrap();

        let mut info = ParseInfo::new(tokens);
        let e = expr(&mut info).unwrap();
        println!("{:?}", e);
    }
}
