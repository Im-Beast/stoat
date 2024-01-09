use std::process::ExitCode;

use clap::{arg, Command};

mod run;

use crate::run::run_file;

pub fn cli() -> Command {
    Command::new("stoat")
        .about("cool interpreted language")
        .version("0.0.1")
        .author("Im-Beast (Mateusz Franik)")
        .subcommand_required(true)
        .subcommand(
            Command::new("run")
                .arg(arg!(<FILE_PATH> "File to run"))
                .arg_required_else_help(true)
                .arg(arg!(-d --debug "Print debug information"))
                .arg_required_else_help(false)
                .about("Run desired file"),
        )
        .subcommand(
            Command::new("vm_test")
                .about("Run VM test")
                .arg_required_else_help(false),
        )
}

pub fn parse_cli() -> ExitCode {
    let matches = cli().get_matches();

    match matches.subcommand() {
        Some(("vm_test", _)) => {
            vm::vm_test();
            ExitCode::SUCCESS
        }
        Some(("run", args)) => {
            let module_path = args.get_one::<String>("FILE_PATH").expect("Required");
            let debug = args.get_one::<bool>("debug").unwrap_or(&false);

            if let Err(error) = run_file(&module_path, *debug) {
                eprintln!("Error: {error}");
                ExitCode::FAILURE
            } else {
                ExitCode::SUCCESS
            }
        }
        _ => ExitCode::FAILURE,
    }
}
