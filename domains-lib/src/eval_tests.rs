use super::ast::ASTContext;
use super::cfg::*;
use super::eval::*;

use super::lexer::Lexer;
use super::parser::Parser;
use utils::DiagnosticEmitter;

struct EvalResult {
    output: String,
    ctx: ASTContext,
    cfg: Cfg,
    walk: Walk,
}

fn eval_string(source: &str) -> Option<EvalResult> {
    let mut diag = DiagnosticEmitter::new(Box::new(Vec::new()), Box::new(Vec::new()));
    let lexer = Lexer::new(source, &mut diag);
    let tokens = lexer.lex_all();
    let parser = Parser::new(tokens, &mut diag);
    let ctx = parser.parse()?;
    let cfg = Cfg::new(&ctx);
    let walk = create_random_walk(&cfg, &ctx, 1);
    Some(EvalResult {
        output: diag.out_buffer().to_string() + diag.err_buffer(),
        ctx,
        cfg,
        walk,
    })
}

#[test]
fn no_control_flow_walk() {
    let source = r"init(50, 0, 0, 0);
translation(10, 0);
rotation(0, 0, 90)";
    let EvalResult {
        output,
        ctx,
        cfg: _cfg,
        walk,
    } = eval_string(source).unwrap();
    assert!(output.is_empty());
    assert!(matches!(walk[0].op, Operation::Init(_)));
    assert!(matches!(walk[1].op, Operation::Translation(_)));
    assert!(matches!(walk[2].op, Operation::Rotation(_)));

    let anns = annotate_with_walks(&[walk]);
    let pretty_printed = crate::ast::print(ctx.get_root(), &ctx, &anns);
    let expected = r#"init(50, 0, 0, 0) /* {{x: 50, y: 0}} */;
translation(10, 0) /* {{x: 60, y: 0}} */;
rotation(0, 0, 90) /* {{x: 0, y: 60}} */"#;
    assert_eq!(expected, pretty_printed);
}

#[test]
fn iter_test() {
    let source = r"init(50, 0, 0, 0);
iter { translation(10, 0) }";
    let EvalResult {
        output,
        ctx: _ctx,
        cfg: _cfg,
        walk,
    } = eval_string(source).unwrap();

    assert!(output.is_empty());
    assert!(matches!(walk[0].op, Operation::Init(_)));
    for step in walk.iter().skip(1) {
        assert!(matches!(step.op, Operation::Translation(_)));
    }
}

#[test]
fn or_test() {
    let source = r"init(50, 0, 0, 0);
{ translation(0, 10) } or { translation(10, 0) }";
    let EvalResult {
        output,
        ctx: _ctx,
        cfg: _cfg,
        walk,
    } = eval_string(source).unwrap();

    assert!(output.is_empty());
    assert!(matches!(walk[0].op, Operation::Init(_)));
    assert!(matches!(walk[1].op, Operation::Translation(_)));
}
