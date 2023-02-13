use crate::domains::*;
use std::collections::{HashMap, HashSet};

#[test]
fn sign_domain_tests() {
    use SignDomain::*;
    // Comparisons, join
    assert_eq!(Positive, Positive);
    assert!(Bottom <= Negative);
    assert!(Zero <= Top);
    assert_eq!(Zero.join(&Zero), Zero);
    assert_eq!(Negative.join(&Positive), Top);
    assert_eq!(Positive.join(&Negative), Top);
    assert_eq!(Top.join(&Negative), Top);
    assert_eq!(Negative.join(&Top), Top);
    assert_eq!(Bottom.join(&Negative), Negative);
    assert_eq!(Negative.join(&Bottom), Negative);

    // Meet
    assert_eq!(Zero.meet(&Zero), Zero);
    assert_eq!(Bottom.meet(&Zero), Bottom);
    assert_eq!(Top.meet(&Zero), Zero);
    assert_eq!(Negative.meet(&Zero), Bottom);

    // Conversions
    assert_eq!(SignDomain::from(5), Positive);
    assert_eq!(SignDomain::from(0), Zero);
    assert_eq!(SignDomain::from(-5), Negative);
    assert_eq!(SignDomain::from(IntervalDomain::bottom(&())), Bottom);
    assert_eq!(SignDomain::from(IntervalDomain::top(&())), Top);
    assert_eq!(SignDomain::from(IntervalDomain::from(0)), Zero);
    assert_eq!(SignDomain::from(IntervalDomain::from(5)), Positive);
    assert_eq!(SignDomain::from(IntervalDomain::from(-5)), Negative);

    // Operations
    assert_eq!(Positive + Zero, Positive);
    assert_eq!(Positive - Bottom, Bottom);
    assert_eq!(-Positive, Negative);
    assert_eq!(Positive - Positive, Top);
    assert_eq!(Positive + Positive, Positive);
    assert_eq!(Positive * Positive, Positive);
    assert_eq!(Positive * Negative, Negative);
    assert_eq!(Top * Zero, Zero);

    // Pretty printing
    assert_eq!(format!("{Bottom:?}"), "Bottom");
}

#[test]
fn vec2_domain_tests() {
    // Vec2Sign
    {
        type SignVec = Vec2Domain<SignDomain>;
        use SignDomain::*;
        let bottom = SignVec::bottom(&());
        let pos_pos = SignVec {
            x: Positive,
            y: Positive,
        };
        let pos_neg = SignVec {
            x: Positive,
            y: Negative,
        };
        let pos_top = SignVec {
            x: Positive,
            y: Top,
        };
        let pos_bot = SignVec {
            x: Positive,
            y: Bottom,
        };
        let top_top = SignVec { x: Top, y: Top };

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
fn interval_domain_tests() {
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

    // Conversions
    assert_eq!(IntervalDomain::from(SignDomain::Bottom), bottom);
    assert_eq!(IntervalDomain::from(SignDomain::Top), top);
    assert_eq!(
        IntervalDomain::from(SignDomain::Zero),
        IntervalDomain::from(0)
    );
    assert!(singleton < IntervalDomain::from(SignDomain::Positive));
    assert!(!(singleton < IntervalDomain::from(SignDomain::Negative)));

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
    let sub_expected = IntervalDomain { min: -20, max: -1 };
    assert_eq!(small_range_a - small_range_b, sub_expected);
    let mul_expected1 = IntervalDomain { min: 0, max: 200 };
    assert_eq!(small_range_a * small_range_b, mul_expected1);
    assert_eq!(small_range_a * top, top);
    let neg_range = IntervalDomain { min: -10, max: -2 };
    let mul_expected2 = IntervalDomain {
        min: -200,
        max: -22,
    };
    assert_eq!(small_range_b * neg_range, mul_expected2);
    let mul_expected3 = IntervalDomain { min: 4, max: 100 };
    assert_eq!(neg_range * neg_range, mul_expected3);

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

#[test]
fn array_domain_test() {
    use SignDomain::*;
    type MyDomain = Array<SignDomain, 3>;
    let bottom = MyDomain::bottom(&());
    let top = MyDomain::top(&());
    let pos_zero_neg = Array([Positive, Zero, Negative]);
    let pos_zero_bottom = Array([Positive, Zero, Bottom]);
    let neg_zero_top = Array([Negative, Zero, Top]);

    assert!(pos_zero_neg == pos_zero_neg);
    assert!(pos_zero_neg < top);
    assert!(bottom < pos_zero_neg);
    assert!(pos_zero_bottom < pos_zero_neg);
    assert!(!(pos_zero_neg < neg_zero_top));
    assert!(!(neg_zero_top < pos_zero_neg));

    assert_eq!(pos_zero_neg.join(&neg_zero_top), Array([Top, Zero, Top]));
    assert_eq!(pos_zero_neg.join(&top), top);
    assert_eq!(bottom.join(&neg_zero_top), neg_zero_top);
    assert_eq!(
        pos_zero_neg.meet(&neg_zero_top),
        Array([Bottom, Zero, Negative])
    );
    assert_eq!(pos_zero_neg.meet(&top), pos_zero_neg);
    assert_eq!(bottom.meet(&neg_zero_top), bottom);

    // Pretty printing
    assert_eq!(
        format!("{pos_zero_neg:?}"),
        "Array([Positive, Zero, Negative])"
    );
}

#[test]
fn product_domain_test() {
    type MyDomain = Prod3<SignDomain, (), BitSetDomain>;
    let bit_ctx = BitSetTop(2);
    let ctx = ((), (), bit_ctx);
    let bottom = MyDomain::bottom(&ctx);
    let top = MyDomain::top(&ctx);
    let a = Prod3(SignDomain::Zero, (), BitSetDomain::from(&bit_ctx, &[1]));
    let b = Prod3(SignDomain::Top, (), BitSetDomain::bottom(&bit_ctx));
    let c = Prod3(SignDomain::Zero, (), BitSetDomain::from(&bit_ctx, &[0, 1]));

    assert!(top > bottom);
    assert!(top > a);
    assert!(bottom < a);
    assert!(!(b > a));
    assert!(!(b < a));
    assert!(c > a);
    assert!(a == a);
    assert_eq!(
        a.join(&b),
        Prod3(SignDomain::Top, (), BitSetDomain::from(&bit_ctx, &[1]))
    );
    assert_eq!(
        a.meet(&c),
        Prod3(SignDomain::Zero, (), BitSetDomain::from(&bit_ctx, &[1]))
    );
    assert_eq!(
        b.meet(&c),
        Prod3(SignDomain::Zero, (), BitSetDomain::bottom(&bit_ctx))
    );
    assert_eq!(format!("{top:?}"), "Prod3(Top, (), {0, 1})");
}

#[test]
fn flat_domain_test() {
    let bottom = Flat::bottom(&());
    let top = Flat::top(&());
    let a = Flat::Element(5);
    let b = Flat::Element(3);

    assert!(top > bottom);
    assert!(top > a);
    assert!(bottom < a);
    assert!(a == a);
    assert!(a != b);
    assert!(!(a < b));
    assert!(!(a > b));
    assert_eq!(a.join(&b), top);
    assert_eq!(a.meet(&b), bottom);
    assert_eq!(a.join(&a), a);
    assert_eq!(a.meet(&a), a);
    assert_eq!(format!("{top:?}"), "Top");
    assert_eq!(format!("{a:?}"), "Element(5)");
    assert_eq!(format!("{bottom:?}"), "Bottom");
}

#[test]
fn bool_domain_test() {
    let bottom = bool::bottom(&());
    let top = bool::top(&());

    assert!(top > bottom);
    assert!(top == top);
    assert!(bottom == bottom);
    assert_eq!(top.join(&bottom), top);
    assert_eq!(top.meet(&bottom), bottom);
    assert_eq!(top.join(&top), top);
    assert_eq!(top.meet(&top), top);
    assert_eq!(bottom.join(&bottom), bottom);
    assert_eq!(bottom.meet(&bottom), bottom);
}

#[test]
fn natural_domain_test() {
    let bottom = u64::bottom(&());

    assert!(5 > bottom);
    assert_eq!(bottom.join(&bottom), bottom);
    assert_eq!(bottom.join(&5), bottom);
    assert_eq!(5.join(&3), 3);
}

#[test]
fn lifted_domain_test() {
    let bottom = Lift::<bool>::bottom(&());
    let false_val = Some(false);
    let true_val = Some(true);

    assert!(false_val > bottom);
    assert!(true_val > bottom);
    assert!(true_val > false_val);
    assert_eq!(bottom, bottom);
    assert_eq!(true_val, true_val);
    assert_eq!(bottom.join(&bottom), bottom);
    assert_eq!(bottom.join(&true_val), true_val);
    assert_eq!(bottom.meet(&true_val), bottom);
    assert_eq!(false_val.join(&true_val), true_val);
    assert_eq!(false_val.meet(&true_val), false_val);
    assert_eq!(format!("{bottom:?}"), "None");
    assert_eq!(format!("{true_val:?}"), "Some(true)");
}

#[test]
fn map_domain_test() {
    type MyDomain = Map<&'static str, SignDomain>;
    use SignDomain::*;
    let ctx = MapCtx(HashSet::from(["Foo", "Bar", "Baz"]), ());
    let bottom = MyDomain::bottom(&ctx);
    let top = MyDomain::top(&ctx);
    let a = Map(HashMap::from([("Foo", Zero)]));
    let b = Map(HashMap::from([("Foo", Top), ("Bar", Positive)]));
    let c = Map(HashMap::from([("Foo", Top), ("Bar", Negative)]));

    assert!(a == a);
    assert!(bottom < top);
    assert!(a < top);
    assert!(bottom < a);
    assert!(a < b);
    assert!(!(b < c));
    assert!(!(b > c));
    assert_eq!(a.join(&b), b);
    assert_eq!(
        b.join(&c),
        Map(HashMap::from([("Foo", Top), ("Bar", Top),]))
    );
    assert_eq!(a.meet(&b), Map(HashMap::from([("Foo", Zero),])));
    assert_eq!(
        b.meet(&c),
        Map(HashMap::from([("Foo", Top), ("Bar", Bottom)]))
    );
    assert_eq!(format!("{bottom:?}"), "Map()");
    assert_eq!(
        format!("{top:?}"),
        r#"Map(("Bar", Top), ("Baz", Top), ("Foo", Top))"#
    );
}

#[test]
fn stack_domain_test() {
    type MyDomain = Stack2<SignDomain, IntervalDomain>;
    let bottom = MyDomain::bottom(&());
    let top = MyDomain::top(&());
    let a = MyDomain::S1(SignDomain::Negative);
    let b = MyDomain::S1(SignDomain::Positive);
    let c = MyDomain::S2(IntervalDomain { min: 5, max: 10 });
    let d = MyDomain::S2(IntervalDomain { min: 8, max: 12 });
    let e = MyDomain::S2(IntervalDomain { min: 4, max: 13 });

    assert!(top > bottom);
    assert!(top > a);
    assert!(a > bottom);
    assert!(a == a);
    assert!(!(a < b));
    assert!(!(a > b));
    assert!(a < e);
    assert!(d < e);
    assert_eq!(a.join(&top), top);
    assert_eq!(a.join(&bottom), a);
    assert_eq!(a.meet(&top), a);
    assert_eq!(a.meet(&bottom), bottom);
    assert_eq!(a.join(&b), MyDomain::S1(SignDomain::Top));
    assert_eq!(a.join(&c), c);
    assert_eq!(a.meet(&c), a);
    assert_eq!(c.join(&d), MyDomain::S2(IntervalDomain { min: 5, max: 12 }));
    assert_eq!(c.meet(&d), MyDomain::S2(IntervalDomain { min: 8, max: 10 }));

    assert_eq!(format!("{bottom:?}"), "Bottom");
    assert_eq!(format!("{top:?}"), "S2([-inf, inf])");
    assert_eq!(format!("{a:?}"), "S1(Negative)");
    assert_eq!(format!("{e:?}"), "S2([4, 13])");
}

#[test]
fn union_domain_test() {
    type MyDomain = Union2<SignDomain, IntervalDomain>;
    let bottom = MyDomain::bottom(&());
    let top = MyDomain::top(&());
    let a = MyDomain::U1(SignDomain::Negative);
    let b = MyDomain::U1(SignDomain::Positive);
    let c = MyDomain::U2(IntervalDomain { min: 5, max: 10 });
    let d = MyDomain::U2(IntervalDomain { min: 8, max: 12 });
    let e = MyDomain::U2(IntervalDomain { min: 4, max: 13 });

    assert!(top > bottom);
    assert!(top > a);
    assert!(a > bottom);
    assert!(a == a);
    assert!(!(a < b));
    assert!(!(a > b));
    assert!(d < e);
    assert!(!(a < e));
    assert!(!(a > e));
    assert_eq!(a.join(&top), top);
    assert_eq!(a.join(&bottom), a);
    assert_eq!(a.meet(&top), a);
    assert_eq!(a.meet(&bottom), bottom);
    assert_eq!(a.join(&b), MyDomain::U1(SignDomain::Top));
    assert_eq!(a.join(&c), top);
    assert_eq!(a.meet(&c), bottom);
    assert_eq!(c.join(&d), MyDomain::U2(IntervalDomain { min: 5, max: 12 }));
    assert_eq!(c.meet(&d), MyDomain::U2(IntervalDomain { min: 8, max: 10 }));

    assert_eq!(format!("{bottom:?}"), "Bottom");
    assert_eq!(format!("{top:?}"), "Top");
    assert_eq!(format!("{a:?}"), "U1(Negative)");
    assert_eq!(format!("{e:?}"), "U2([4, 13])");
}
