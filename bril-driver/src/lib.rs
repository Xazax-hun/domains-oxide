use bril_lib::{
    eval::{Interpreter, Value},
    ir::print_dot,
    lexer::Lexer,
    parser::Parser,
};
use clap::Parser as CommandLineParser;
use utils::DiagnosticEmitter;

#[derive(Debug, CommandLineParser, Default)]
#[command(name = "bril", version, about = "Analyze and interpret Bril IR.")]
pub struct Opt {
    /// Dump the control flow graph representation of the program in graphviz format.
    #[arg(long)]
    pub dump_cfg: bool,

    /// File containing the program written in the language.
    pub filename: String,

    /// Arguments to the main function.
    // TODO: support boolean arguments.
    pub arguments: Vec<i32>,
}

pub fn process_source(src: &str, diag: &mut DiagnosticEmitter, opts: &Opt) -> Option<()> {
    let lexer = Lexer::new(src, diag);
    let tokens = lexer.lex_all();
    if tokens.tokens.is_empty() {
        return None;
    }
    let parser = Parser::new(tokens, diag);
    let unit = parser.parse()?;

    if opts.dump_cfg {
        diag.out_ln(&print_dot(&unit));
    }

    let mut interp = Interpreter::new(&unit, diag);
    let args: Vec<_> = opts.arguments.iter().map(|&i| Value::I(i)).collect();
    // TODO: Handle failures in eval.
    if let Some(result) = interp.eval_main(&args) {
        diag.out_ln(&format!("Return value: {result}"));
    }

    Some(())
}

#[cfg(test)]
mod driver_tests;
