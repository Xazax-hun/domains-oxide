use crate::cfg_tests::parse_string;

use super::ast::ASTContext;
use super::ast::Operation;
use super::cfg::Cfg;
use super::cfg_tests::ParseResult;
use super::eval::*;

#[allow(dead_code)]
struct EvalResult<'ctx> {
    cfg: Cfg<'ctx>,
    walk: Walk,
}

fn eval_string<'ctx>(ctx: &'ctx ASTContext) -> Option<EvalResult<'ctx>> {
    let cfg = Cfg::new(&ctx);
    let walk = create_random_walk(&cfg, &ctx, 1);
    Some(EvalResult { cfg, walk })
}

#[test]
fn no_control_flow_walk() {
    let source = r"init(50, 0, 0, 0);
translation(10, 0);
rotation(0, 0, 90)";
    let ParseResult { output, ctx } = parse_string(source).unwrap();
    let EvalResult { walk, .. } = eval_string(&ctx).unwrap();
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
    let ParseResult { output, ctx } = parse_string(source).unwrap();
    let EvalResult { walk, .. } = eval_string(&ctx).unwrap();

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
    let ParseResult { output, ctx } = parse_string(source).unwrap();
    let EvalResult { walk, .. } = eval_string(&ctx).unwrap();

    assert!(output.is_empty());
    assert!(matches!(walk[0].op, Operation::Init(_)));
    assert!(matches!(walk[1].op, Operation::Translation(_)));
}
