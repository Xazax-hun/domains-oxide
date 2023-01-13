use crate::{
    analysis::sign_analysis::{self, sign_analysis_results_to_annotations},
    cfg::Cfg,
    cfg_tests::{parse_string, ParseResult},
    ast::print
};

#[test]
fn test_linear_program() {
    let source = r"init(50, 50, 50, 50);
translation(10, 0);
rotation(0, 0, 0)";
    let expected = r"init(50, 50, 50, 50) /* { x: Positive, y: Positive } */;
translation(10, 0) /* { x: Positive, y: Positive } */;
rotation(0, 0, 0) /* { x: Positive, y: Positive } */";

    let ParseResult { output, ctx } = parse_string(source).unwrap();
    let cfg = Cfg::new(&ctx);
    let results = sign_analysis::get_sign_analysis(&cfg);
    let anns = sign_analysis_results_to_annotations(&cfg, &results);
    assert!(output.is_empty());
    assert!(!results.is_empty());
    assert_eq!(expected, print(ctx.get_root(), &ctx, &anns));
}

#[test]
fn test_branching_program() {
    let source = r"init(50, 50, 50, 50);
{
  translation(10, 0)
} or {
  translation(-10, 0)
}";
    let expected = r"init(50, 50, 50, 50) /* { x: Positive, y: Positive } */;
{
  translation(10, 0) /* { x: Positive, y: Positive } */
} or {
  translation(-10, 0) /* { x: Top, y: Positive } */
}";

    let ParseResult { output, ctx } = parse_string(source).unwrap();
    let cfg = Cfg::new(&ctx);
    let results = sign_analysis::get_sign_analysis(&cfg);
    let anns = sign_analysis_results_to_annotations(&cfg, &results);
    assert!(output.is_empty());
    assert!(!results.is_empty());
    assert_eq!(expected, print(ctx.get_root(), &ctx, &anns));
}
