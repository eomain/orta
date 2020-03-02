
extern crate libtoken;
extern crate liblex;
extern crate libparse;
extern crate libgen;

mod pipe;

use std::io;
use std::fs;
use std::io::Read;
use std::io::Write;
use std::fmt;
use std::path::Path;
use std::path::PathBuf;
use libtoken::TokenStream;
use libast::SyntaxTree;
pub use libgen::CodeGen;

#[derive(Debug)]
pub enum InputStream {
    File(io::Result<fs::File>)
}

impl InputStream {
    fn ok(&self) -> bool
    {
        use InputStream::*;
        match self {
            File(f) => f.is_ok()
        }
    }
}

impl<P> From<P> for InputStream
    where P: AsRef<Path> {
    fn from(p: P) -> InputStream
    {
        InputStream::File(fs::File::open(p))
    }
}

impl Read for InputStream {
    fn read(&mut self, b: &mut [u8]) -> io::Result<usize>
    {
        use InputStream::*;
        match self {
            File(f) => if let Ok(f) = f {
                f.read(b)
            } else {
                unreachable!()
            }
        }
    }
}

struct OutputSteam<W> {
    output: W
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Error {
    Input,
    Scan,
    Parse,
    CodeGen,
    Write
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        use Error::*;
        let msg: &str = self.clone().into();
        write!(f, "{}", msg)
    }
}

impl From<Error> for &str {
    fn from(e: Error) -> &'static str
    {
        use Error::*;
        match e {
            Input => "unable to read input",
            _ => unimplemented!()
        }
    }
}

fn input(i: &mut InputStream) -> Result<String, Error>
{
    if !i.ok() {
        return Err(Error::Input);
    }

    let mut s = String::new();
    i.read_to_string(&mut s);
    Ok(s)
}

fn scan(s: &String) -> Result<TokenStream, Error>
{
    let chars: Vec<char> = s.chars().collect();
    match liblex::scan(chars) {
        Err(_) => Err(Error::Scan),
        Ok(tokens) => Ok(tokens)
    }

}

fn parse(tokens: TokenStream) -> Result<SyntaxTree, Error>
{
    match libparse::construct(tokens) {
        Err(_) => Err(Error::Parse),
        Ok(ast) => Ok(ast)
    }
}

fn codegen(ast: &SyntaxTree, cg: libgen::CodeGen, name: &str) -> String
{
    let w = Vec::new();
    let mut unit = libgen::CodeUnit::new(name, w);
    let ir = libgen::make(cg, unit, &ast);
    // TODO
    String::from_utf8(ir).unwrap()
}

fn compile(output: Option<&str>) -> Vec<&str>
{
    if let Some(o) = output {
        vec![
            "llc", "-o", o
        ]
    } else {
        vec!["llc"]
    }
}

// Runtime library
static RTLIB: &str = include_str!("../rt.ll");

fn asm(output: &str) -> Vec<&str>
{
    vec![
        "as", "-o", output
    ]
}

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
fn ld_cmd(output: &str) -> Vec<&str>
{
    vec![
        "ld", "-o", output
    ]
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
fn ld_cmd(output: &str) -> Vec<&str>
{
    vec![
        "ld", "--dynamic-linker", "/lib64/ld-linux-x86-64.so.2",
        "-o", output
    ]
}

// Default program entry point
static ENTRY: &str = "_start";

fn ld<'a>(output: &'a str, objs: &mut Vec<&'a str>) -> Vec<&'a str>
{
    let mut v = ld_cmd(output);
    v.append(&mut vec!["-lc", "-e", ENTRY]);
    v.append(objs);
    v
}

fn file_write<I>(name: I, s: &String) -> Result<fs::File, Error>
    where I: AsRef<Path>
{
    let mut f = match fs::File::create(name) {
        Err(_) => return Err(Error::Input),
        Ok(f) => f
    };
    f.write(s.as_bytes());
    Ok(f)
}

mod aux {
    pub mod ext {
        pub fn ll(s: &str) -> String
        {
            format!("{}.ll", s)
        }
    }
}

mod output {
    fn ir()
    {

    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum OutputFormat {
    IR(libgen::CodeGen),
    ASM,
    OBJ,
    BIN
}

impl OutputFormat {
    fn default() -> Self
    {
        OutputFormat::BIN
    }
}

#[derive(Debug)]
pub struct BuildCommand {
    input: InputStream,
    output: PathBuf,
    format: OutputFormat
}

impl BuildCommand {
    pub fn default(input: InputStream) -> Self
    {
        Self {
            input,
            output: "out".into(),
            format: OutputFormat::default()
        }
    }

    // Set the file output name
    pub fn output<P>(mut self, name: P) -> Self
        where P: Into<PathBuf>
    {
        self.output = name.into();
        self
    }

    pub fn codegen(&self) -> libgen::CodeGen
    {
        if let OutputFormat::IR(cg) = self.format {
            cg
        } else {
            libgen::CodeGen::LLVM
        }
    }

    pub fn format(mut self, f: OutputFormat) -> Self
    {
        self.format = f;
        self
    }

    // Output IR
    pub fn ir(mut self, cg: libgen::CodeGen) -> Self
    {
        self.format = OutputFormat::IR(cg);
        self
    }

    // Output assembly
    pub fn asm(mut self) -> Self
    {
        self.format = OutputFormat::ASM;
        self
    }

    // Output object file
    pub fn obj(mut self) -> Self
    {
        self.format = OutputFormat::OBJ;
        self
    }

    // Output executable binary
    pub fn bin(mut self) -> Self
    {
        self.format = OutputFormat::BIN;
        self
    }

    fn libs(&mut self, s: &mut String)
    {
        match self.format {
            OutputFormat::IR(_) => (),
            _ => {
                s.push('\n');
                s.push_str(RTLIB);
            }
        }
    }

    fn name(&self) -> &str
    {
        self.output.to_str().unwrap()
    }

    fn exec(&mut self) -> Result<(), Error>
    {
        let input = input(&mut self.input)?;
        let tokens = scan(&input)?;
        let ast = parse(tokens)?;

        let cg = self.codegen();
        let mut ir = codegen(&ast, cg, "");
        self.libs(&mut ir);

        use OutputFormat::*;

        use OutputFormat::*;
        if let IR(_) = self.format {
            file_write(&self.output, &ir)?;
        } else {
            let name = "tmp.ll";
            let clean = || {
                fs::remove_file(name);
            };
            let f = file_write(name, &ir)?;
            let mut pl = pipe::Pipeline::new(&ir);

            if let ASM = self.format {
                let out = Some(self.name());
                let comp = compile(out);
                pl.add(&comp);
                pl.run_with(clean);
                return Ok(());
            }

            let comp = compile(None);
            pl.add(&comp);

            if let OBJ = self.format {
                let out = self.name();
                let obj = asm(out);
                pl.add(&obj);
                pl.run_with(clean);
                return Ok(());
            }

            let name = "tmp.o";
            let obj = asm(name);
            pl.add(&obj);
            pl.run_with(clean);

            let out = self.name();
            let mut args = vec![name];
            let args = ld(out, &mut args);
            pipe::exec(&args);
            fs::remove_file(name);
        }
        Ok(())
    }

    pub fn build(mut self) -> Result<(), Error>
    {
        self.exec()
    }
}

#[cfg(test)]
mod tests {

    use crate::*;

    use libgen::CodeGen;

    #[test]
    fn ir_test()
    {
        let path = "tests/test";
        let input = InputStream::from(path);
        let status = BuildCommand::default(input)
                                  .ir(CodeGen::LLVM)
                                  .output("out.ll")
                                  .build();
        if let Err(e) = status {
            println!("{}: error: {}", path, e);
        }
    }

    #[test]
    fn asm_test()
    {
        let path = "tests/test";
        let input = InputStream::from(path);
        let status = BuildCommand::default(input)
                                  .asm()
                                  .output("out.s")
                                  .build();
    }

    #[test]
    fn obj_test()
    {
        let path = "tests/test";
        let input = InputStream::from(path);
        let status = BuildCommand::default(input)
                                  .obj()
                                  .output("out.o")
                                  .build();
    }

}
