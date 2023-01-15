use crate::domains::*;
use std::collections::HashSet;

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

    // Meet
    assert_eq!(zero.meet(&zero), zero);
    assert_eq!(bottom.meet(&zero), bottom);
    assert_eq!(top.meet(&zero), zero);
    assert_eq!(negative.meet(&zero), bottom);

    // Conversions
    assert_eq!(SignDomain::from(5), SignDomain::Positive);
    assert_eq!(SignDomain::from(0), SignDomain::Zero);
    assert_eq!(SignDomain::from(-5), SignDomain::Negative);

    // Pretty printing
    assert_eq!(format!("{bottom:?}"), "Bottom");
}

#[test]
fn vec2_domain_tests() {
    // Vec2Sign
    {
        type SignVec = Vec2Domain<SignDomain>;
        let bottom = SignVec::bottom(&());
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
        let pos_bot = SignVec {
            x: SignDomain::Positive,
            y: SignDomain::Bottom,
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
        assert_eq!(top_top, SignVec::top(&()));
        assert_eq!(pos_pos.meet(&pos_neg), pos_bot);

        // Pretty printing
        assert_eq!(format!("{pos_top:?}"), "{ x: Positive, y: Top }");
    }

    // Vec2Interval
    {
        type IntervalVec = Vec2Domain<IntervalDomain>;

        let bottom = IntervalVec::bottom(&());
        let top = IntervalVec::top(&());
        let singleton = IntervalVec {
            x: IntervalDomain::from(5),
            y: IntervalDomain::from(15),
        };
        let range = IntervalVec {
            x: IntervalDomain { min: 0, max: 10 },
            y: IntervalDomain { min: 10, max: 20 },
        };

        assert_eq!(range.widen(&singleton, 0), top);
        assert_eq!(singleton.widen(&range, 0), singleton);
        assert_eq!(singleton.widen(&bottom, 0), singleton);

        assert_eq!(format!("{range:?}"), "{ x: [0, 10], y: [10, 20] }")
    }
}

#[test]
fn vec2_interval_tests() {
    let bottom = IntervalDomain::bottom(&());
    let top = IntervalDomain::top(&());
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

    // Merging / join
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

    // Intersection / meet
    assert_eq!(top.meet(&bottom), bottom);
    assert_eq!(bottom.meet(&singleton), bottom);
    assert_eq!(top.meet(&singleton), singleton);
    assert_eq!(singleton.meet(&small_range_a), singleton);
    assert_eq!(small_range_a.meet(&small_range_b), bottom);
    assert_eq!(large_range.meet(&small_range_b), small_range_b);

    // Widening
    assert_eq!(small_range_a.widen(&large_range, 0), small_range_a);
    assert_eq!(large_range.widen(&small_range_a, 0), top);
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
    assert_eq!(bumped_max.widen(&small_range_a, 0), widened_max);
    assert_eq!(decremented_min.widen(&small_range_a, 0), widened_min);
    assert_eq!(large_range.widen(&bottom, 0), large_range);

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
    assert_eq!(format!("{singleton:?}"), "[5, 5]");
    assert_eq!(format!("{small_range_a:?}"), "[0, 10]");
    assert_eq!(format!("{top:?}"), "[-inf, inf]");
    assert_eq!(format!("{bottom:?}"), "[inf, -inf]");
}

#[test]
fn set_domain_tests() {
    type IntSetDomain = PowerSetDomain<i32>;
    let ctx = PowerSetTop(PowerSetDomain::<i32>(HashSet::from([1, 2, 3, 4, 5])));
    let bottom = IntSetDomain::bottom(&ctx);
    let small_set = PowerSetDomain::<i32>(HashSet::from([1, 2, 3]));
    let small_set2 = PowerSetDomain::<i32>(HashSet::from([2, 3, 4]));
    let union = PowerSetDomain::<i32>(HashSet::from([1, 2, 3, 4]));
    let intersection = PowerSetDomain::<i32>(HashSet::from([2, 3]));

    assert!(bottom < small_set);
    assert!(bottom < small_set2);
    assert!(!(small_set2 < small_set));
    assert!(!(small_set2 > small_set));
    assert!(small_set < union);
    assert!(small_set2 < union);
    assert_eq!(small_set.join(&small_set2), union);
    assert_eq!(small_set2.join(&small_set), union);
    assert_eq!(small_set.meet(&small_set2), intersection);
    assert_eq!(small_set2.meet(&small_set), intersection);
}

#[test]
fn bitset_domain_tests() {
    let ctx = BitSetTop(5);
    let bottom = BitSetDomain::bottom(&ctx);
    let small_set = BitSetDomain::from(&ctx, &[1, 2, 3]);
    let small_set2 = BitSetDomain::from(&ctx, &[2, 3, 4]);
    let union = BitSetDomain::from(&ctx, &[1, 2, 3, 4]);
    let intersection = BitSetDomain::from(&ctx, &[2, 3]);

    assert!(bottom < small_set);
    assert!(bottom < small_set2);
    assert!(!(small_set2 < small_set));
    assert!(!(small_set2 > small_set));
    assert!(small_set < union);
    assert!(small_set2 < union);
    assert_eq!(small_set.join(&small_set2), union);
    assert_eq!(small_set2.join(&small_set), union);
    assert_eq!(small_set.meet(&small_set2), intersection);
    assert_eq!(small_set2.meet(&small_set), intersection);

    assert_eq!(format!("{bottom:?}"), "{}".to_owned());
    assert_eq!(format!("{small_set:?}"), "{1, 2, 3}".to_owned());
    assert_eq!(
        format!("{:?}", BitSetDomain::top(&ctx)),
        "{0, 1, 2, 3, 4}".to_owned()
    );
}

#[test]
fn flipped_sign_domain_tests() {
    let bottom = Flipped(SignDomain::Bottom);
    let positive = Flipped(SignDomain::Positive);
    let negative = Flipped(SignDomain::Negative);
    let zero = Flipped(SignDomain::Zero);
    let top = Flipped(SignDomain::Top);

    // Comparisons, join
    assert_eq!(positive, positive);
    assert!(bottom <= negative);
    assert!(zero <= top);
    assert_eq!(zero.join(&zero), zero);
    assert_eq!(negative.join(&positive), bottom);
    assert_eq!(positive.join(&negative), bottom);
    assert_eq!(bottom.join(&negative), bottom);
    assert_eq!(negative.join(&bottom), bottom);
    assert_eq!(top.join(&negative), negative);
    assert_eq!(negative.join(&top), negative);

    // Meet
    assert_eq!(zero.meet(&zero), zero);
    assert_eq!(top.meet(&zero), top);
    assert_eq!(bottom.meet(&zero), zero);
    assert_eq!(negative.meet(&zero), top);

    // Pretty printing
    assert_eq!(format!("{bottom:?}"), "Flipped(Bottom)");
}
