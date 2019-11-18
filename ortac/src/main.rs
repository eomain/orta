
extern crate libcore;
extern crate clap;

use clap::{ App, Arg, ArgMatches };
use clap::SubCommand as Command;

fn main()
{
    let mut app = App::new("ortac")
        .about("orta compiler")
        .version(env!("CARGO_PKG_VERSION"))
        .arg(Arg::with_name("INPUT")
            .help("Specify the source file to use"));

    let m = app.clone().get_matches();
    let (cmd, arg) = m.subcommand();

    match cmd {
        "" => {
            app.print_long_help();
        },

        _ => ()
    }
}
