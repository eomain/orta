
extern crate libast;
extern crate libtoken;

use libast::ParseTree;
use libast::TokenStream;

mod ll {

    use libast::Literal;
    use libtoken::Token;

    fn literal(token: &Token)
    {

    }

}

pub struct ParseInfo<'a> {
    tokens: TokenStream<'a>
}

#[derive(Debug)]
pub enum Error {

}

fn construct() -> Result<ParseTree, Error>
{
    let tree = ParseTree::new();

    Ok(tree)
}

#[cfg(test)]
mod tests {

}
