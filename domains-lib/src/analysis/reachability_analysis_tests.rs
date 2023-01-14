use crate::{
    analysis::reachability_analysis::FutureOperations,
    analysis::reachability_analysis::PastOperations, analysis::test_utils::check_expected_results,
};

#[test]
fn test_primitive_program() {
    let source = r"init(50, 50, 50, 50);
translation(10, 0)";
    let expected1 = r"init(50, 50, 50, 50) /* {Init} */;
translation(10, 0) /* {Init, Translation} */";

    check_expected_results(PastOperations, source, expected1);

    let expected2 = r"/* {Init, Translation} */ init(50, 50, 50, 50);
/* {Translation} */ translation(10, 0)";

    check_expected_results(FutureOperations, source, expected2);
}

#[test]
fn test_loops() {
    let source = r"init(50, 50, 50, 50);
translation(10, 0);
iter {
  translation(10, 0);
  rotation(0, 0, 90)
}";
    let expected1 = r"init(50, 50, 50, 50) /* {Init} */;
translation(10, 0) /* {Init, Translation} */;
iter {
  translation(10, 0) /* {Init, Rotation, Translation} */;
  rotation(0, 0, 90) /* {Init, Rotation, Translation} */
}";

    check_expected_results(PastOperations, source, expected1);

    let expected2 = r"/* {Init, Rotation, Translation} */ init(50, 50, 50, 50);
/* {Rotation, Translation} */ translation(10, 0);
iter {
  /* {Rotation, Translation} */ translation(10, 0);
  /* {Rotation, Translation} */ rotation(0, 0, 90)
}";

    check_expected_results(FutureOperations, source, expected2);
}

#[test]
fn test_branch() {
    let source = r"init(50, 50, 50, 50);
translation(10, 0);
{
  translation(10, 0)
} or {
  rotation(0, 0, 90)
}";
    let expected1 = r"init(50, 50, 50, 50) /* {Init} */;
translation(10, 0) /* {Init, Translation} */;
{
  translation(10, 0) /* {Init, Translation} */
} or {
  rotation(0, 0, 90) /* {Init, Rotation, Translation} */
}";

    check_expected_results(PastOperations, source, expected1);

    let expected2 = r"/* {Init, Rotation, Translation} */ init(50, 50, 50, 50);
/* {Rotation, Translation} */ translation(10, 0);
{
  /* {Translation} */ translation(10, 0)
} or {
  /* {Rotation} */ rotation(0, 0, 90)
}";

    check_expected_results(FutureOperations, source, expected2);
}
