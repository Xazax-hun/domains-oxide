use crate::{
    analysis::{interval_analysis::IntervalAnalysis, Analysis},
    ast::print,
    cfg::Cfg,
    cfg_tests::{parse_string, ParseResult},
};

fn check_expected_results(source: &str, expected: &str) {
    let ParseResult { output, ctx } = parse_string(source).unwrap();
    let cfg = Cfg::new(&ctx);
    let anns = IntervalAnalysis.annotate(&cfg);
    assert!(output.is_empty());
    assert_eq!(expected, print(ctx.get_root(), &ctx, &anns));
}

#[test]
fn test_primitive_program() {
    let source = r"init(50, 50, 50, 50);
translation(10, 0)";
    let expected = r"init(50, 50, 50, 50) /* { x: [50, 100], y: [50, 100] } */;
translation(10, 0) /* { x: [60, 110], y: [50, 100] } */";

    check_expected_results(source, expected);
}

#[test]
fn test_rotation() {
    let source1 = r"init(20, 20, 50, 50);
rotation(0, 0, 90)";
    let expected1 = r"init(20, 20, 50, 50) /* { x: [20, 70], y: [20, 70] } */;
rotation(0, 0, 90) /* { x: [-70, -20], y: [20, 70] } */";

    check_expected_results(source1, expected1);
    
    let source2 = r"init(20, 20, 50, 50);
rotation(0, 0, 180)";
    let expected2 = r"init(20, 20, 50, 50) /* { x: [20, 70], y: [20, 70] } */;
rotation(0, 0, 180) /* { x: [-70, -20], y: [-70, -20] } */";

    check_expected_results(source2, expected2);

    let source3 = r"init(20, 20, 50, 50);
rotation(0, 0, 360)";
    let expected3 = r"init(20, 20, 50, 50) /* { x: [20, 70], y: [20, 70] } */;
rotation(0, 0, 360) /* { x: [20, 70], y: [20, 70] } */";

    check_expected_results(source3, expected3);
}