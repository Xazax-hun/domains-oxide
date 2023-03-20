use super::{sign_analysis::SignAnalysis, test_utils::check_expected_results};

#[test]
fn basic_test() {
    let source = r"@main {
  v: int = const 5;
  ret;
}
";

    let expected = r"@main {
  v: int = const 5; /* v: Positive */
  ret;
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
  pos: int = const 5; /* pos: Positive */
  neg: int = const -5; /* neg: Negative */
  zero: int = const 0; /* zero: Zero */
  less: bool = lt neg pos; /* less: Positive */
  greater: bool = gt zero pos; /* greater: Zero */
  equal: bool = eq less greater; /* equal: Zero */
  conj: bool = and equal less; /* conj: Zero */
  disj: bool = or equal less; /* disj: Positive */
  ret;
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
  x: bool = id greater;
  ret;

.false:
  x: bool = id greater;
  ret;
}
";

    // TODO.
    let expected = r"@main {
  pos: int = const 5; /* pos: Positive */
  neg: int = const -5; /* neg: Negative */
  greater: bool = gt neg pos; /* greater: Zero */
  br greater .true .false;

.true:
  x: bool = id greater; /* x: Bottom */
  ret;

.false:
  x: bool = id greater; /* x: Zero */
  ret;
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
  res: int = const 1; /* res: Positive */
  i: int = const 0; /* i: Zero */
  jmp .test;

.test:
  cond: bool = lt i x; /* cond: NonNeg */
  br cond .loop .done;

.loop:
  one: int = const 1; /* one: Positive */
  i: int = add i one; /* i: Positive */
  res: int = mul res i;
  jmp .test;

.done:
  ret res;
}
";
    check_expected_results(SignAnalysis, source, expected)
}
