use clap::{Parser as CommandLineParser, ValueEnum};
use domains_lib::analysis::{get_analysis_results, Analyses, AnalysisResult};
use domains_lib::ast;
use domains_lib::cfg::{self, Cfg};
use domains_lib::eval::{annotate_with_walks, create_random_walk};
use domains_lib::lexer::Lexer;
use domains_lib::parser::Parser;
use domains_lib::render::render_random_walk;
use utils::DiagnosticEmitter;

const DEFAULT_EXECUTIONS: &str = "1";
const DEFAULT_LOOPINESS: &str = "1";

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, ValueEnum)]
pub enum CLIAnalyses {
    // A simple sign analysis.
    Sign,
    Interval,
    PastOperations,
    FutureOperations,
}

impl From<CLIAnalyses> for Analyses {
    fn from(value: CLIAnalyses) -> Self {
        match value {
            CLIAnalyses::Sign => Analyses::Sign,
            CLIAnalyses::Interval => Analyses::Interval,
            CLIAnalyses::PastOperations => Analyses::PastOperations,
            CLIAnalyses::FutureOperations => Analyses::FutureOperations,
        }
    }
}

#[derive(Debug, CommandLineParser)]
#[command(
    name = "domains",
    version,
    about = "Toy language to help learn about static analysis."
)]
pub struct Opt {
    /// Dump the control flow graph representation of the program in graphviz format.
    #[arg(long)]
    pub dump_cfg: bool,

    /// Pretty print the source code with the traces projected on the top
    /// as annotations.
    #[arg(long)]
    pub annotate_trace: bool,

    /// Number of traces to produce.
    #[arg(short, long, default_value = DEFAULT_EXECUTIONS)]
    pub executions: u32,

    /// The larger the number the more time will be spent in loops.
    #[arg(short, long, default_value = DEFAULT_LOOPINESS)]
    pub loopiness: u32,

    /// Emit svg to stdout.
    #[arg(long)]
    pub svg: bool,

    /// Do not draw the lines in the traces of the svg output.
    #[arg(long)]
    pub dots_only: bool,

    /// Name of the analysis to execute
    #[arg(long, value_name = "ANALYSIS_NAME")]
    pub analyze: Option<CLIAnalyses>,

    /// File containing the program written in the language.
    pub filename: String,
}

impl Default for Opt {
    fn default() -> Self {
        Self {
            dump_cfg: false,
            annotate_trace: false,
            executions: DEFAULT_EXECUTIONS.parse().unwrap(),
            loopiness: DEFAULT_LOOPINESS.parse().unwrap(),
            svg: false,
            dots_only: false,
            analyze: None,
            filename: String::default(),
        }
    }
}

pub fn process_source(src: &str, diag: &mut DiagnosticEmitter, opts: &Opt) -> Option<()> {
    let lexer = Lexer::new(src, diag);
    let tokens = lexer.lex_all();
    if tokens.is_empty() {
        return None;
    }

    let parser = Parser::new(tokens, diag);
    let ctxt = parser.parse()?;
    let cfg = Cfg::new(&ctxt);

    if opts.dump_cfg {
        let cfg_dump = cfg::print(&cfg, &ctxt);
        diag.out(&cfg_dump);
        return Some(());
    }

    let mut covered = Vec::new();
    if let Some(analysis) = opts.analyze {
        let AnalysisResult {
            annotations,
            covered: inferred,
        } = get_analysis_results(Analyses::from(analysis), &cfg);
        covered = inferred;
        let annotated = ast::print(ctxt.get_root(), &ctxt, &annotations);
        diag.out_ln(&annotated);
    }

    let mut walks = Vec::new();
    walks.reserve(opts.executions as usize);
    for i in 1..=opts.executions {
        walks.push(create_random_walk(&cfg, &ctxt, opts.loopiness));

        if !opts.svg {
            if opts.executions > 1 {
                diag.out_ln(&format!("{i}. execution:"));
            }
            for step in walks.last().unwrap() {
                diag.out_ln(&format!("{}", step.pos));
            }
        }
    }
    if opts.annotate_trace {
        let anns = annotate_with_walks(&walks);
        let out = ast::print(ctxt.get_root(), &ctxt, &anns);
        diag.out_ln(&out);
    }
    if opts.svg {
        let svg = render_random_walk(&walks, &ctxt, &covered, opts.dots_only);
        diag.out(&svg);
    }

    Some(())
}

#[cfg(test)]
mod driver_tests;
