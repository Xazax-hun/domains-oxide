use crate::{
    analysis::interval_analysis::IntervalAnalysis, analysis::test_utils::check_expected_results,
};

#[test]
fn test_primitive_program() {
    let source = r"init(50, 50, 50, 50);
translation(10, 0)";
    let expected = r"init(50, 50, 50, 50) /* { x: [50, 100], y: [50, 100] } */;
translation(10, 0) /* { x: [60, 110], y: [50, 100] } */";

    check_expected_results(IntervalAnalysis, source, expected);
}

#[test]
fn test_rotation() {
    let source1 = r"init(20, 20, 50, 50);
rotation(0, 0, 90)";
    let expected1 = r"init(20, 20, 50, 50) /* { x: [20, 70], y: [20, 70] } */;
rotation(0, 0, 90) /* { x: [-70, -20], y: [20, 70] } */";

    check_expected_results(IntervalAnalysis, source1, expected1);

    let source2 = r"init(20, 20, 50, 50);
rotation(0, 0, 180)";
    let expected2 = r"init(20, 20, 50, 50) /* { x: [20, 70], y: [20, 70] } */;
rotation(0, 0, 180) /* { x: [-70, -20], y: [-70, -20] } */";

    check_expected_results(IntervalAnalysis, source2, expected2);

    let source3 = r"init(20, 20, 50, 50);
rotation(0, 0, 360)";
    let expected3 = r"init(20, 20, 50, 50) /* { x: [20, 70], y: [20, 70] } */;
rotation(0, 0, 360) /* { x: [20, 70], y: [20, 70] } */";

    check_expected_results(IntervalAnalysis, source3, expected3);
}

#[test]
fn test_loop() {
    let source1 = r"init(50, 50, 50, 50);
translation(10, 0);
iter{
  translation(10, 0)
}";
    let expected1 = r"init(50, 50, 50, 50) /* { x: [50, 100], y: [50, 100] } */;
translation(10, 0) /* { x: [60, 110], y: [50, 100] } */;
iter {
  translation(10, 0) /* { x: [70, inf], y: [50, 100] } */
}";

    check_expected_results(IntervalAnalysis, source1, expected1);

    let source2 = r"init(50, 50, 50, 50);
translation(10, 0);
iter{
  translation(10, 0)
};
rotation(0, 0, 90)";
    let expected2 = r"init(50, 50, 50, 50) /* { x: [50, 100], y: [50, 100] } */;
translation(10, 0) /* { x: [60, 110], y: [50, 100] } */;
iter {
  translation(10, 0) /* { x: [70, inf], y: [50, 100] } */
};
rotation(0, 0, 90) /* { x: [-100, -50], y: [70, inf] } */";

    check_expected_results(IntervalAnalysis, source2, expected2);

    let source3 = r"init(50, 50, 50, 50);
iter {
  {
    translation(10, 0)
  } or {
    translation(0, 10)
  }
};
rotation(0, 0, 180)";
    let expected3 = r"init(50, 50, 50, 50) /* { x: [50, 100], y: [50, 100] } */;
iter {
  {
    translation(10, 0) /* { x: [60, inf], y: [50, inf] } */
  } or {
    translation(0, 10) /* { x: [50, inf], y: [60, inf] } */
  }
};
rotation(0, 0, 180) /* { x: [-inf, -50], y: [-inf, -50] } */";

    check_expected_results(IntervalAnalysis, source3, expected3);
}
