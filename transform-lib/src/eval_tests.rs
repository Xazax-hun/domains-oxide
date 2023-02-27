use crate::parser_tests::parse_string;

use super::ast::ASTContext;
use super::ast::Operation;
use super::cfg::Cfg;
use super::eval::*;

fn eval_ast<'ctx>(ctx: &'ctx ASTContext) -> (Cfg<'ctx>, Walk) {
    let cfg = Cfg::new(&ctx);
    let walk = create_random_walk(&cfg, &ctx, 1);
    (cfg, walk)
}

#[test]
fn no_control_flow_walk() -> Result<(), String> {
    let source = r"init(50, 0, 0, 0);
translation(10, 0);
rotation(0, 0, 90)";
    let ctx = parse_string(source)?;
    let (_, walk) = eval_ast(&ctx);
    assert!(matches!(walk[0].op, Operation::Init(_)));
    assert!(matches!(walk[1].op, Operation::Translation(_)));
    assert!(matches!(walk[2].op, Operation::Rotation(_)));

    let anns = annotate_with_walks(&[walk]);
    let pretty_printed = crate::ast::print(ctx.get_root(), &ctx, &anns);
    let expected = r#"init(50, 0, 0, 0) /* {{x: 50, y: 0}} */;
translation(10, 0) /* {{x: 60, y: 0}} */;
rotation(0, 0, 90) /* {{x: 0, y: 60}} */"#;
    assert_eq!(expected, pretty_printed);

    Ok(())
}

#[test]
fn iter_test() -> Result<(), String> {
    let source = r"init(50, 0, 0, 0);
iter { translation(10, 0) }";
    let ctx = parse_string(source)?;
    let (_, walk) = eval_ast(&ctx);

    assert!(matches!(walk[0].op, Operation::Init(_)));
    for step in walk.iter().skip(1) {
        assert!(matches!(step.op, Operation::Translation(_)));
    }

    Ok(())
}

#[test]
fn or_test() -> Result<(), String> {
    let source = r"init(50, 0, 0, 0);
{ translation(0, 10) } or { translation(10, 0) }";
    let ctx = parse_string(source)?;
    let (_, walk) = eval_ast(&ctx);

    assert!(matches!(walk[0].op, Operation::Init(_)));
    assert!(matches!(walk[1].op, Operation::Translation(_)));

    Ok(())
}
