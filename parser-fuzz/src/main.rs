#[macro_use]
extern crate afl;

use domains_lib::lexer::Lexer;
use domains_lib::parser::Parser;
use utils::DiagnosticEmitter;

fn parse_string(source: &str) {
    let mut diag = DiagnosticEmitter::new(Box::new(Vec::new()), Box::new(Vec::new()));
    let lexer = Lexer::new(source, &mut diag);
    let tokens = lexer.lex_all();
    if tokens.len() == 0 {
        return;
    }
    let parser = Parser::new(tokens, &mut diag);
    parser.parse();
}

fn main() {
    fuzz!(|data: &[u8]| {
        if let Ok(s) = std::str::from_utf8(data) {
            parse_string(&s);
        }
    });
}
