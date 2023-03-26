use super::{congruence_analysis::CongruenceAnalysis, test_utils::check_expected_results};

#[test]
fn basic_test() {
    let source = r"@main {
  v: int = const 5;
  ret;
}
";

    let expected = r"@main {
  v: int = const 5; /* v: 5 mod 0 */
  ret;
}
";
    check_expected_results(CongruenceAnalysis, source, expected)
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
  pos: int = const 5; /* pos: 5 mod 0 */
  neg: int = const -5; /* neg: -5 mod 0 */
  greater: bool = gt neg pos; /* greater: 0 mod 0 */
  br greater .true .false;

.true:
  x: bool = id greater; /* x: 1 mod 1 */
  ret;

.false:
  x: bool = id greater; /* x: 0 mod 0 */
  ret;
}
";
    check_expected_results(CongruenceAnalysis, source, expected)
}



#[test]
fn loop_test() {
    let source = r"@main {
  hundred: int = const 100;
  three: int = const 3;
  six: int = const 6;
  zero: int = const 0;
  x: int = id zero;
  y: int = id zero;
  jmp .cond;

.cond:
  less: bool = lt x hundred;
  br less .body .after;

.body:
  equal: bool = eq y zero;
  br equal .then .else;

.then:
  x: int = add x three;
  jmp .body_cont;

.else:
  x: int = add x six;
  jmp .body_cont;

.body_cont:
  y: int = add y six;
  jmp .cond;

.after:
  ret;
}
";

    let expected = r"@main {
  hundred: int = const 100; /* hundred: 100 mod 0 */
  three: int = const 3; /* three: 3 mod 0 */
  six: int = const 6; /* six: 6 mod 0 */
  zero: int = const 0; /* zero: 0 mod 0 */
  x: int = id zero; /* x: 0 mod 0 */
  y: int = id zero; /* y: 0 mod 0 */
  jmp .cond;

.cond:
  less: bool = lt x hundred; /* less: 0 mod 1 */
  br less .body .after;

.body:
  equal: bool = eq y zero; /* equal: 0 mod 1 */
  br equal .then .else;

.then:
  x: int = add x three; /* x: 0 mod 3 */
  jmp .body_cont;

.else:
  x: int = add x six; /* x: 0 mod 3 */
  jmp .body_cont;

.body_cont:
  y: int = add y six; /* y: 0 mod 6 */
  jmp .cond;

.after:
  ret;
}
";
    check_expected_results(CongruenceAnalysis, source, expected)
}
