use crate::{
    analysis::{sign_analysis::SignAnalysis, Analysis},
    ast::print,
    cfg::Cfg,
    cfg_tests::{parse_string, ParseResult},
};

fn check_expected_results(source: &str, expected: &str) {
    let ParseResult { output, ctx } = parse_string(source).unwrap();
    let cfg = Cfg::new(&ctx);
    let anns = SignAnalysis.annotate(&cfg);
    assert!(output.is_empty());
    assert_eq!(expected, print(ctx.get_root(), &ctx, &anns));
}

#[test]
fn test_linear_program() {
    let source = r"init(50, 50, 50, 50);
translation(10, 0);
rotation(0, 0, 0)";
    let expected = r"init(50, 50, 50, 50) /* { x: Positive, y: Positive } */;
translation(10, 0) /* { x: Positive, y: Positive } */;
rotation(0, 0, 0) /* { x: Positive, y: Positive } */";

    check_expected_results(source, expected);
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

    check_expected_results(source, expected);
}

#[test]
fn test_nested_loops() {
    let source = r"init(50, 50, 50, 50);
translation(10, 0);
iter {
  iter {
    translation(10, 0)
  };
  {
    translation(10, 0)
  } or {
    {
      translation(10, 0)
    } or {
      iter {
        rotation(0, 0, 90)
      }
    }
  }
}";
    let expected = r"init(50, 50, 50, 50) /* { x: Positive, y: Positive } */;
translation(10, 0) /* { x: Positive, y: Positive } */;
iter {
  iter {
    translation(10, 0) /* { x: Top, y: Top } */
  };
  {
    translation(10, 0) /* { x: Top, y: Top } */
  } or {
    {
      translation(10, 0) /* { x: Top, y: Top } */
    } or {
      iter {
        rotation(0, 0, 90) /* { x: Top, y: Top } */
      }
    }
  }
}";

    check_expected_results(source, expected);
}
