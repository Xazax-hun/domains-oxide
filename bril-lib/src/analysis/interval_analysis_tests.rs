use super::{interval_analysis::IntervalAnalysis, test_utils::check_expected_results};

#[test]
fn basic_test() {
    let source = r"@main {
  v: int = const 5;
  ret;
}
";

    let expected = r"@main {
  v: int = const 5; /* v: [5, 5] */
  ret;
}
";
    check_expected_results(IntervalAnalysis, source, expected)
}

#[test]
fn factorial() {
    let source = r"@main(x: int): int {
  res: int = const 1;
  i: int = const 0;
  jmp .test;

.test:
  cond: bool = lt i x;
  br cond .loop .done;

.loop:
  one: int = const 1;
  i: int = add i one;
  res: int = mul res i;
  jmp .test;

.done:
  i: int = id i;
  ret res;
}
";

    let expected = r"@main(x: int): int {
  res: int = const 1; /* res: [1, 1] */
  i: int = const 0; /* i: [0, 0] */
  jmp .test;

.test:
  cond: bool = lt i x; /* cond: [0, 1] */
  br cond .loop .done;

.loop:
  one: int = const 1; /* one: [1, 1] */
  i: int = add i one; /* i: [1, inf] */
  res: int = mul res i; /* res: [1, inf] */
  jmp .test;

.done:
  i: int = id i; /* i: [0, inf] */
  ret res;
}
";
    check_expected_results(IntervalAnalysis, source, expected)
}

