use super::{sign_analysis::SignAnalysis, test_utils::check_expected_results};

#[test]
fn basic_test() {
    let source = r"@main {
  v: int = const 5;
  ret;
}
";

    let expected = r"@main {
  v: int = const 5;
  ret; /* v: Positive */
}
";
    check_expected_results(SignAnalysis, source, expected)
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
  ret res;
}
";

    let expected = r"@main(x: int): int {
  res: int = const 1;
  i: int = const 0;
  jmp .test; /* i: Zero, res: Positive */

.test:
  cond: bool = lt i x;
  br cond .loop .done; /* cond: NonNeg, i: NonNeg, res: Positive */

.loop:
  one: int = const 1;
  i: int = add i one;
  res: int = mul res i;
  jmp .test; /* cond: NonNeg, i: Positive, one: Positive, res: Positive */

.done:
  ret res; /* cond: NonNeg, i: NonNeg, res: Positive */
}
";
    check_expected_results(SignAnalysis, source, expected)
}
