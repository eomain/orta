
extern crate libcore;
extern crate clap;

use libcore::InputStream;
use libcore::BuildCommand;
use libcore::OutputFormat;
use libcore::CodeGen;
use clap::{ App, Arg, ArgMatches };
use clap::SubCommand as Command;

fn build(source: &str, output: &str, format: OutputFormat)
{
    let input = InputStream::from(source);
    let status = BuildCommand::default(input)
                              .output(output)
                              .format(format)
                              .build();
}

fn main()
{
    let mut app = App::new("ortac")
        .about("orta compiler")
        .version(env!("CARGO_PKG_VERSION"))
        .arg(Arg::with_name("INPUT")
                 .required(true)
                 .index(1)
                 .help("Specify the source file to use"))
        .arg(Arg::with_name("output")
                 .short("o")
                 .value_name("FILE")
                 .help("Specify the output filename"))
        .arg(Arg::with_name("emit")
                 .short("e")
                 .long("emit")
                 .value_name("FORMAT")
                 .possible_values(&["llvm"])
                 .takes_value(true)
                 .hide_possible_values(true)
                 .help("Output in specified intermediate file format"))
        .arg(Arg::with_name("asm")
                 .short("s")
                 .long("asm")
                 .takes_value(false)
                 .help("Output in assembly file format"))
        .arg(Arg::with_name("obj")
                 .short("c")
                 .long("obj")
                 .takes_value(false)
                 .help("Output in native object file format"));

    let m = app.clone().get_matches();
    let mut format = OutputFormat::BIN;
    let input = m.value_of("INPUT").unwrap();
    let mut output = "out";
    if m.is_present("output") {
        output = m.value_of("output").unwrap();
    }

    if m.is_present("emit") {
        match m.value_of("emit").unwrap() {
            "llvm" => {
                format = OutputFormat::IR(CodeGen::LLVM)
            },
            _ => ()
        }
    }
    if m.is_present("asm") {
        format = OutputFormat::ASM;
    }
    if m.is_present("obj") {
        format = OutputFormat::OBJ;
    }

    build(input, output, format);
}
