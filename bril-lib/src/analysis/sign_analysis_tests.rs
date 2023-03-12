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
