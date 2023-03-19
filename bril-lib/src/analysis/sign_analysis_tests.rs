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
fn logical_operators() {
    let source = r"@main {
  pos: int = const 5;
  neg: int = const -5;
  zero: int = const 0;
  less: bool = lt neg pos;
  greater: bool = gt zero pos;
  equal: bool = eq less greater;
  conj: bool = and equal less;
  disj: bool = or equal less;
  ret;
}
";

    let expected = r"@main {
  pos: int = const 5;
  neg: int = const -5;
  zero: int = const 0;
  less: bool = lt neg pos;
  greater: bool = gt zero pos;
  equal: bool = eq less greater;
  conj: bool = and equal less;
  disj: bool = or equal less;
  ret; /* conj: Zero, disj: Positive, equal: Zero, greater: Zero, less: Positive, neg: Negative, pos: Positive, zero: Zero */
}
";
    check_expected_results(SignAnalysis, source, expected)
}

#[test]
fn branching() {
    let source = r"@main {
  pos: int = const 5;
  neg: int = const -5;
  greater: bool = gt neg pos;
  br greater .true .false;

.true:
  ret;

.false:
  ret;
}
";

    let expected = r"@main {
  pos: int = const 5;
  neg: int = const -5;
  greater: bool = gt neg pos;
  br greater .true .false; /* greater: Zero, neg: Negative, pos: Positive */

.true:
  ret; /*  */

.false:
  ret; /* greater: Zero, neg: Negative, pos: Positive */
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
  jmp .test; /* i: Zero, res: Positive, x: Top */

.test:
  cond: bool = lt i x;
  br cond .loop .done; /* cond: NonNeg, i: NonNeg, res: Positive, x: Top */

.loop:
  one: int = const 1;
  i: int = add i one;
  res: int = mul res i;
  jmp .test; /* cond: NonNeg, i: Positive, one: Positive, res: Positive, x: Top */

.done:
  ret res; /* cond: NonNeg, i: NonNeg, res: Positive, x: Top */
}
";
    check_expected_results(SignAnalysis, source, expected)
}
