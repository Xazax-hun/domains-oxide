use utils::DiagnosticEmitter;

use domains_driver::Opt;
use structopt::StructOpt;

fn main() {
    let opts = Opt::from_args();
    let mut diag = DiagnosticEmitter::new(Box::new(std::io::stdout()), Box::new(std::io::stderr()));
    let contents = std::fs::read_to_string(&opts.filename).expect("Failed to read input file.");

    if domains_driver::process_source(&contents, &mut diag, &opts).is_none() {
        std::process::exit(1);
    }
}
