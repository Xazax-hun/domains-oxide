use bril_lib::{
    analysis::{get_analysis_results, Analyses},
    eval::{Interpreter, Value},
    ir::{self, print_dot},
    lexer::Lexer,
    parser::Parser,
};
use clap::{Parser as CommandLineParser, ValueEnum};
use utils::DiagnosticEmitter;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, ValueEnum)]
pub enum CLIAnalyses {
    Sign,
}

impl From<CLIAnalyses> for Analyses {
    fn from(value: CLIAnalyses) -> Self {
        match value {
            CLIAnalyses::Sign => Analyses::Sign,
        }
    }
}

#[derive(Debug, CommandLineParser, Default)]
#[command(name = "bril", version, about = "Analyze and interpret Bril IR.")]
pub struct Opt {
    /// Dump the control flow graph representation of the program in graphviz format.
    #[arg(long)]
    pub dump_cfg: bool,

    /// Name of the analysis to execute
    #[arg(long, value_name = "ANALYSIS_NAME")]
    pub analyze: Option<CLIAnalyses>,

    /// File containing the program written in the language.
    pub filename: String,

    /// Arguments to the main function.
    #[arg(value_parser = parse_value)]
    pub arguments: Vec<Value>,
}

fn parse_value(arg: &str) -> Result<Value, std::num::ParseIntError> {
    if arg == "true" {
        return Ok(Value::B(true));
    }
    if arg == "false" {
        return Ok(Value::B(false));
    }
    Ok(Value::I(arg.parse()?))
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

    if let Some(analysis) = opts.analyze {
        let anns = get_analysis_results(analysis.into(), &unit);
        diag.out(&ir::print(&unit, &anns));
        return Some(());
    }

    let mut interp = Interpreter::new(&unit, diag);
    let result = interp.eval_main(&opts.arguments)?;
    if result != Value::Unit {
        diag.out_ln(&format!("Return value: {result}"));
    }

    Some(())
}

#[cfg(test)]
mod driver_tests;
