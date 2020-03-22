
use crate::pipe::Pipeline;

pub fn asm(output: &str) -> Vec<&str>
{
    vec![
        "as", "-o", output
    ]
}
