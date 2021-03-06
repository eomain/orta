
extern crate libast;

pub use libast::*;

pub mod llvm;

mod llvmast;

// A type used to specify the code
// generation format.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum CodeGen {
    LLVM
}

impl CodeGen {
    fn begin<W>(&self, u: &mut CodeUnit<W>, tree: &SyntaxTree)
        where W: std::io::Write
    {
        use CodeGen::*;
        match self {
            LLVM => {
                let m = llvmast::main(&u.name, tree);
                m.output(&mut u.output);
            }
        }
    }
}

// A code unit, used as for outputing the
// generated code.
pub struct CodeUnit<W>
    where W: std::io::Write {
    name: String, // name of the code unit or file
    output: W // destination to be written to
}

impl<W> CodeUnit<W>
    where W: std::io::Write {
    pub fn new(name: &str, output: W) -> Self
    {
        Self {
            name: name.into(),
            output
        }
    }
}

// Entry point into the module. It takes a `CodeGen` target format,
// that the `SyntaxTree` will be converted into and outputs this
// with the `CodeUnit`.
pub fn make<W>(cg: CodeGen, mut u: CodeUnit<W>, tree: &SyntaxTree) -> W
    where W: std::io::Write
{
    cg.begin(&mut u, tree);
    u.output
}

pub trait Output {
    fn output<W>(&self, w: &mut W)
        where W: std::io::Write;
}

#[cfg(test)]
mod tests {

    extern crate libast;
    use libast::IntType::*;
    use libast::DataType;
    use libast::ParamList;
    use libast::Function;
    use libast::FunctionProp;
    use libast::SyntaxTree;

    use crate::*;

    #[test]
    fn llvm_gen()
    {
        let cg = CodeGen::LLVM;
        let w = &mut std::io::stdout();
        let mut unit = CodeUnit::new("test", w);
        let dtype = DataType::Integer(U8);
        let bexpr = libast::Expr::Binary(
            BinaryExpr::Add(
                Box::new(
                    libast::Expr::Value(
                        libast::Value::Literal(
                            libast::Literal::Unsigned(1),
                            dtype.clone()
                        )
                    )
                ),
                Box::new(
                    libast::Expr::Value(
                        libast::Value::Literal(
                            libast::Literal::Unsigned(1),
                            dtype.clone()
                        )
                    )
                )
            )
        );

        let rtype = DataType::Integer(U8);
        let mut param = ParamList::new();
        param.add("a", DataType::Integer(U8));
        param.add("b", DataType::Integer(U8));
        let ret = libast::Expr::Return(
            libast::Return {
                dtype: DataType::Integer(U8),
                expr: Some(Box::new(
                        libast::Expr::Value(
                        libast::Value::Literal(
                            libast::Literal::Unsigned(0),
                            dtype.clone()
                        )
                    )
                )),
            }
        );

        let name = "main".into();
        let expr = vec![
            bexpr, ret
        ];

        let prop = FunctionProp::default();
        let function = Function::new(name, prop, param, rtype, expr);
        let mut ast = SyntaxTree::new();
        ast.append(function);

        make(cg, unit, &ast);
    }

}
