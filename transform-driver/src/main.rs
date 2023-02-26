use utils::DiagnosticEmitter;

use clap::Parser;
use transform_driver::Opt;
use std::process::ExitCode;

fn main() -> ExitCode {
    let opts = Opt::parse();
    let mut diag = DiagnosticEmitter::new(Box::new(std::io::stdout()), Box::new(std::io::stderr()));
    let contents = std::fs::read_to_string(&opts.filename).expect("Failed to read input file.");

    if transform_driver::process_source(&contents, &mut diag, &opts).is_none() {
        return ExitCode::from(1);
    }
    ExitCode::from(0)
}
