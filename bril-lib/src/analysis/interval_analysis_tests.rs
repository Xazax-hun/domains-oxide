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
  n: bool = not disj;
  ret;
}
";

    let expected = r"@main {
  pos: int = const 5; /* pos: [5, 5] */
  neg: int = const -5; /* neg: [-5, -5] */
  zero: int = const 0; /* zero: [0, 0] */
  less: bool = lt neg pos; /* less: [1, 1] */
  greater: bool = gt zero pos; /* greater: [0, 0] */
  equal: bool = eq less greater; /* equal: [0, 0] */
  conj: bool = and equal less; /* conj: [0, 0] */
  disj: bool = or equal less; /* disj: [1, 1] */
  n: bool = not disj; /* n: [0, 0] */
  ret;
}
";
    check_expected_results(IntervalAnalysis, source, expected)
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

    let expected = r"@main {
  pos: int = const 5; /* pos: [5, 5] */
  neg: int = const -5; /* neg: [-5, -5] */
  greater: bool = gt neg pos; /* greater: [0, 0] */
  br greater .true .false;

.true:
  x: bool = id greater; /* x: [inf, -inf] */
  ret;

.false:
  x: bool = id greater; /* x: [0, 0] */
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