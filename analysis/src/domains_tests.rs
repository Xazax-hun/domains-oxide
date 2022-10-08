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

    // Pretty printing
    assert_eq!(bottom.to_string(), "Bottom");
}

#[test]
fn vec2_domain_test() {
    // Vec2Sign
    {
        type SignVec = Vec2Domain<SignDomain>;
        let bottom = SignVec::bottom();
        let pos_pos = SignVec {
            x: SignDomain::Positive,
            y: SignDomain::Positive,
        };
        let pos_neg = SignVec {
            x: SignDomain::Positive,
            y: SignDomain::Negative,
        };
        let pos_top = SignVec {
            x: SignDomain::Positive,
            y: SignDomain::Top,
        };
        let top_top = SignVec {
            x: SignDomain::Top,
            y: SignDomain::Top,
        };

        assert_eq!(bottom, bottom);
        assert_eq!(pos_pos, pos_pos);
        assert!(bottom <= pos_pos);
        assert!(pos_pos <= pos_pos);
        assert!(pos_pos <= pos_top);
        assert!(!(pos_pos <= pos_neg));
        assert!(!(pos_pos >= pos_neg));
        assert_eq!(pos_pos.join(&pos_neg), pos_top);
        assert_eq!(top_top, SignVec::top());

        // Pretty printing
        assert_eq!(pos_top.to_string(), "{ x: Positive, y: Top }");
    }

    // Vec2Interval
    {
        type IntervalVec = Vec2Domain<IntervalDomain>;

        let bottom = IntervalVec::bottom();
        let top = IntervalVec::top();
        let singleton = IntervalVec {
            x: IntervalDomain::from(5),
            y: IntervalDomain::from(15),
        };
        let range = IntervalVec {
            x: IntervalDomain { min: 0, max: 10 },
            y: IntervalDomain { min: 10, max: 20 },
        };

        assert_eq!(singleton.widen(&range), top);
        assert_eq!(range.widen(&singleton), range);
        assert_eq!(bottom.widen(&singleton), singleton);

        assert_eq!(range.to_string(), "{ x: [0, 10], y: [10, 20] }")
    }
}

#[test]
fn vec2_interval_test() {
    let bottom = IntervalDomain::bottom();
    let top = IntervalDomain::top();
    let singleton = IntervalDomain::from(5);
    let small_range_a = IntervalDomain {
        min: 0.into(),
        max: 10.into(),
    };
    let small_range_b = IntervalDomain {
        min: 11.into(),
        max: 20.into(),
    };
    let large_range = IntervalDomain {
        min: (-100).into(),
        max: 100.into(),
    };

    // Ordering
    assert!(bottom < top);
    assert!(bottom < singleton);
    assert!(bottom < small_range_a);
    assert!(singleton < small_range_a);
    assert!(small_range_a < large_range);
    assert!(small_range_b < large_range);
    assert!(large_range < top);
    assert!(!(small_range_a <= small_range_b));
    assert!(!(small_range_b <= small_range_a));
    assert!(!(singleton <= small_range_b));
    assert!(!(small_range_b <= singleton));

    // Merging
    assert_eq!(bottom.join(&singleton), singleton);
    assert_eq!(bottom.join(&small_range_a), small_range_a);
    assert_eq!(small_range_a.join(&bottom), small_range_a);
    let merged_smalls = IntervalDomain {
        min: 0.into(),
        max: 20.into(),
    };
    assert_eq!(small_range_a.join(&small_range_b), merged_smalls);
    assert_eq!(large_range.join(&top), top);
    assert_eq!(top.join(&large_range), top);
    assert_eq!(large_range.join(&large_range), large_range);

    // Widening
    assert_eq!(large_range.widen(&small_range_a), large_range);
    assert_eq!(small_range_a.widen(&large_range), top);
    let bumped_max = IntervalDomain {
        min: small_range_a.min,
        max: small_range_a.max + 1,
    };
    let widened_max = IntervalDomain {
        min: small_range_a.min,
        max: INF,
    };
    let decremented_min = IntervalDomain {
        min: small_range_a.min - 1,
        max: small_range_a.max,
    };
    let widened_min = IntervalDomain {
        min: NEG_INF,
        max: small_range_a.max,
    };
    assert_eq!(small_range_a.widen(&bumped_max), widened_max);
    assert_eq!(small_range_a.widen(&decremented_min), widened_min);
    assert_eq!(bottom.widen(&large_range), large_range);

    // Arithmetic
    assert_eq!(singleton + singleton, IntervalDomain::from(10));
    assert_eq!(-singleton, IntervalDomain::from(-5));
    assert_eq!(singleton + top, top);
    assert_eq!(-top, top);
    assert_eq!(-small_range_b, IntervalDomain { min: -20, max: -11 });
    let add_expected = IntervalDomain { min: 5, max: 15 };
    assert_eq!(small_range_a + singleton, add_expected);
    assert_eq!(singleton + small_range_a, add_expected);
    let add_expected2 = IntervalDomain { min: 11, max: 30 };
    assert_eq!(small_range_a + small_range_b, add_expected2);
    assert_eq!(small_range_b + small_range_a, add_expected2);

    // Printing
    assert_eq!(singleton.to_string(), "[5, 5]");
    assert_eq!(small_range_a.to_string(), "[0, 10]");
    assert_eq!(top.to_string(), "[-inf, inf]");
    assert_eq!(bottom.to_string(), "[inf, -inf]");
}
