use domains_lib::cfg::*;
use domains_lib::eval::*;
use domains_lib::lexer::Lexer;
use domains_lib::parser::Parser;
use structopt::StructOpt;
use utils::DiagnosticEmitter;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "domains",
    about = "Toy language to help learn about static analysis."
)]
pub struct Opt {
    /// Dump the control flow graph representation of the program in graphviz format.
    #[structopt(long)]
    pub dump_cfg: bool,

    /// Pretty print the source code with the traces projected on the top
    /// as annotations.
    #[structopt(long)]
    pub annotate_trace: bool,

    /// Number of traces to produce.
    #[structopt(short, long, default_value = "1")]
    pub executions: u32,

    /// The larger the number the more time will be spent in loops.
    #[structopt(short, long, default_value = "1")]
    pub loopiness: u32,

    /// File containing the program written in the language.
    pub filename: String,
}

impl Default for Opt {
    fn default() -> Self {
        Self {
            dump_cfg: false,
            annotate_trace: false,
            executions: 1,
            loopiness: 1,
            filename: "".to_owned(),
        }
    }
}

pub fn process_source(src: &str, diag: &mut DiagnosticEmitter, opts: &Opt) -> Option<()> {
    let mut lexer = Lexer::new(src, diag);
    let tokens = lexer.lex_all();
    if tokens.is_empty() {
        return None;
    }

    let mut parser = Parser::new(tokens, diag);
    let ctxt = parser.parse()?;
    let cfg = Cfg::new(&ctxt);

    if opts.dump_cfg {
        let cfg_dump = domains_lib::cfg::print(&cfg, &ctxt);
        diag.to_out(&cfg_dump);
        return Some(());
    }

    let mut walks = Vec::new();
    for i in 1..=opts.executions {
        walks.push(create_random_walk(&cfg, &ctxt, opts.loopiness));

        if opts.executions > 1 {
            diag.to_out(&format!("{}. execution:\n", i));
        }
        for step in walks.last().unwrap() {
            diag.to_out(&format!("{{ x: {}, y: {} }}\n", step.pos.x, step.pos.y));
        }
    }
    if opts.annotate_trace {
        let anns = annotate_with_walks(&walks);
        let out = domains_lib::ast::print(ctxt.get_root(), &ctxt, &anns);
        diag.to_out(&(out + "\n"));
    }

    Some(())
}

#[cfg(test)]
mod driver_tests;
