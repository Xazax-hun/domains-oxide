use super::domains::*;

#[test]
fn sign_domain_tests() {
    let bottom = SignDomain::Bottom;
    let positive = SignDomain::Positive;
    let negative = SignDomain::Negative;
    let zero = SignDomain::Zero;
    let top = SignDomain::Top;

    // Comparisons, join
    assert_eq!(positive, positive);
    assert!(bottom <= negative);
    assert!(zero <= top);
    assert_eq!(zero.join(&zero), zero);
    assert_eq!(negative.join(&positive), top);
    assert_eq!(positive.join(&negative), top);
    assert_eq!(top.join(&negative), top);
    assert_eq!(negative.join(&top), top);
    assert_eq!(bottom.join(&negative), negative);
    assert_eq!(negative.join(&bottom), negative);

    // Conversions
    assert_eq!(SignDomain::from(5), SignDomain::Positive);
    assert_eq!(SignDomain::from(0), SignDomain::Zero);
    assert_eq!(SignDomain::from(-5), SignDomain::Negative);
}