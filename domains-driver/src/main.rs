use domains_lib::cfg::*;
use domains_lib::eval::*;
use domains_lib::lexer::*;
use domains_lib::parser::*;
use structopt::StructOpt;
use utils::DiagnosticEmitter;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "domains",
    about = "Toy language to help learn about static analysis."
)]
struct Opt {
    /// Dump the control flow graph representation of the program in graphviz format.
    #[structopt(long)]
    dump_cfg: bool,

    /// Pretty print the source code with the traces projected on the top
    /// as annotations.
    #[structopt(long)]
    annotate_trace: bool,

    /// Number of traces to produce.
    #[structopt(short, long, default_value = "1")]
    executions: u32,

    /// The larger the number the more time will be spent in loops.
    #[structopt(short, long, default_value = "1")]
    loopiness: u32,

    /// File containing the program written in the language.
    filename: String,
}

fn main() {
    let opt = Opt::from_args();
    let mut diag = DiagnosticEmitter::new(Box::new(std::io::stdout()), Box::new(std::io::stderr()));
    let contents = std::fs::read_to_string(opt.filename).unwrap();
    let mut lexer = Lexer::new(&contents, &mut diag);
    let mut parser = Parser::new(lexer.lex_all(), &mut diag);
    let ctxt = parser.parse().unwrap();
    let cfg = Cfg::new(&ctxt);

    if opt.dump_cfg {
        let cfg_dump = domains_lib::cfg::print(&cfg, &ctxt);
        diag.to_out(&cfg_dump);
        return;
    }

    let mut walks = Vec::new();
    for i in 0..opt.executions {
        walks.push(create_random_walk(&cfg, &ctxt, opt.loopiness));

        if opt.executions > 1 {
            diag.to_out(&format!("{}. execution:\n", i));
        }
        for step in walks.last().unwrap() {
            diag.to_out(&format!("{{ x: {}, y: {} }}\n", step.pos.x, step.pos.y));
        }
    }
    if opt.annotate_trace {
        let anns = annotate_with_walks(&walks);
        let out = domains_lib::ast::print(ctxt.get_root(), &ctxt, &anns);
        diag.to_out(&out);
    }
}
