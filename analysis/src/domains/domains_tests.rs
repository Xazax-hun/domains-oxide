use crate::domains::*;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};

fn finite_domain_properties<T: Lattice>(all: &[T], ctx: &T::LatticeContext) {
    assert!(T::top(ctx) > T::bottom(ctx));
    for x in all {
        assert!(*x <= T::top(ctx));
        assert!(*x >= T::bottom(ctx));

        assert!(*x == *x);
        assert!(*x <= *x);
        assert!(*x >= *x);
        assert!(!(*x < *x));
        assert!(!(*x > *x));

        assert_eq!(x.join(x, ctx), *x);
        assert_eq!(x.meet(x, ctx), *x);
    }

    for (x, y) in all.iter().cartesian_product(all) {
        assert!(x.join(y, ctx) >= *x);
        assert!(x.join(y, ctx) >= *y);
        assert!(x.meet(y, ctx) <= *x);
        assert!(x.meet(y, ctx) <= *y);

        assert!(x.meet(y, ctx) <= x.join(y, ctx));

        assert_eq!(x.join(y, ctx), y.join(x, ctx));
        assert_eq!(x.meet(y, ctx), y.meet(x, ctx));

        if x > y {
            assert_eq!(x.join(y, ctx), *x);
            assert_eq!(x.meet(y, ctx), *y);
        }
    }
}

#[test]
fn sign_domain_tests() {
    use Sign::*;

    // Join, meet
    assert_eq!(Negative.join(&Positive, &()), NonZero);
    assert_eq!(Bottom.join(&Negative, &()), Negative);
    assert_eq!(Bottom.meet(&Zero, &()), Bottom);
    assert_eq!(Top.meet(&Zero, &()), Zero);
    assert_eq!(Negative.meet(&Zero, &()), Bottom);

    // Properties
    let all = [
        Bottom, Top, Negative, Zero, Positive, NonNeg, NonZero, NonPos,
    ];
    finite_domain_properties(&all, &());

    // Conversions
    assert_eq!(Sign::from(5), Positive);
    assert_eq!(Sign::from(0), Zero);
    assert_eq!(Sign::from(-5), Negative);
    for d in all {
        if d != NonZero {
            assert_eq!(Sign::from(Interval::from(d)), d);
        }
    }
    assert_eq!(Sign::from(Interval::from(0)), Zero);
    assert_eq!(Sign::from(Interval::from(5)), Positive);
    assert_eq!(Sign::from(Interval::from(-5)), Negative);

    // Operations
    for (x, y) in (-2..2).cartesian_product(-2..2) {
        // Multiplicative operations are precise
        let x_sign = Sign::from(x);
        let y_sign = Sign::from(y);
        assert_eq!(x_sign * y_sign, Sign::from(x * y));
        assert_eq!(-x_sign, Sign::from(-x));

        // Integer division is not precise, result could be zero.
        if y != 0 {
            assert!(x_sign / y_sign >= Sign::from(x / y));
        }

        // Additive operations are over-approximated.
        assert!(x_sign + y_sign >= Sign::from(x + y));
        assert!(x_sign - y_sign >= Sign::from(x - y));

        assert_eq!(x_sign + Sign::Zero, Sign::from(x + 0));
        assert_eq!(x_sign - Sign::Zero, Sign::from(x - 0));

        assert_eq!(x_sign + Sign::Bottom, Sign::Bottom);
        assert_eq!(x_sign + Sign::Top, Sign::Top);

        match x_sign.strict_cmp(y_sign) {
            Some(Ordering::Less) => assert!(x < y),
            Some(Ordering::Equal) => assert!(x == y),
            Some(Ordering::Greater) => assert!(x > y),
            _ => (),
        };

        match x_sign.weak_cmp(y_sign) {
            Some(Ordering::Less) => assert!(x <= y),
            Some(Ordering::Equal) => assert!(x == y),
            Some(Ordering::Greater) => assert!(x >= y),
            _ => (),
        };

        assert!(x_sign.logical_and(y_sign) >= Sign::from(i32::from(x != 0 && y != 0)));
        assert!(x_sign.logical_or(y_sign) >= Sign::from(i32::from(x != 0 || y != 0)));
    }
    assert_eq!(Positive - Positive, Top);

    let to_check = [Top, Negative, Zero, Positive, NonNeg, NonPos];
    for (&x, &y) in to_check.iter().cartesian_product(&to_check) {
        let (x_interval, y_interval) = (Interval::from(x), Interval::from(y));

        assert_eq!(x.partial_cmp(&y), x_interval.partial_cmp(&y_interval));

        assert_eq!(x * y, Sign::from(x_interval * y_interval));
        assert_eq!(x + y, Sign::from(x_interval + y_interval));
        assert_eq!(x - y, Sign::from(x_interval - y_interval));
    }

    // Pretty printing
    assert_eq!(format!("{Bottom:?}"), "Bottom");
}

#[test]
fn vec2_domain_tests() {
    // Vec2Sign
    {
        type SignVec = Vec2Domain<Sign>;
        use Sign::*;
        let bottom = SignVec::bottom(&());
        let pos_pos = SignVec {
            x: Positive,
            y: Positive,
        };
        let pos_neg = SignVec {
            x: Positive,
            y: Negative,
        };
        let pos_non_zero = SignVec {
            x: Positive,
            y: NonZero,
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
        assert!(pos_pos <= pos_non_zero);
        assert!(!(pos_pos <= pos_neg));
        assert!(!(pos_pos >= pos_neg));
        assert_eq!(pos_pos.join(&pos_neg, &()), pos_non_zero);
        assert_eq!(top_top, SignVec::top(&()));
        assert_eq!(pos_pos.meet(&pos_neg, &()), pos_bot);

        let all_signs = [
            Bottom, Top, Negative, Zero, Positive, NonNeg, NonZero, NonPos,
        ];
        let mut all = Vec::new();
        for (x, y) in all_signs.iter().cartesian_product(all_signs) {
            all.push(SignVec { x: *x, y });
        }
        finite_domain_properties(&all, &());

        // Pretty printing
        assert_eq!(format!("{pos_non_zero:?}"), "{ x: Positive, y: NonZero }");
    }

    // Vec2Interval
    {
        type IntervalVec = Vec2Domain<Interval>;

        let bottom = IntervalVec::bottom(&());
        let top = IntervalVec::top(&());
        let singleton = IntervalVec {
            x: Interval::from(5),
            y: Interval::from(15),
        };
        let range = IntervalVec {
            x: Interval { min: 0, max: 10 },
            y: Interval { min: 10, max: 20 },
        };

        assert_eq!(range.widen(&singleton, &(), 0), top);
        assert_eq!(singleton.widen(&range, &(), 0), range);
        assert_eq!(singleton.widen(&bottom, &(), 0), singleton);

        assert_eq!(format!("{range:?}"), "{ x: [0, 10], y: [10, 20] }")
    }
}

#[test]
fn interval_domain_tests() {
    let bottom = Interval::bottom(&());
    let top = Interval::top(&());
    let singleton = Interval::from(5);
    let small_range_a = Interval {
        min: 0.into(),
        max: 10.into(),
    };
    let small_range_b = Interval {
        min: 11.into(),
        max: 20.into(),
    };
    let large_range = Interval {
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
    assert_eq!(bottom.join(&singleton, &()), singleton);
    assert_eq!(bottom.join(&small_range_a, &()), small_range_a);
    assert_eq!(small_range_a.join(&bottom, &()), small_range_a);
    let merged_smalls = Interval {
        min: 0.into(),
        max: 20.into(),
    };
    assert_eq!(small_range_a.join(&small_range_b, &()), merged_smalls);
    assert_eq!(large_range.join(&top, &()), top);
    assert_eq!(top.join(&large_range, &()), top);
    assert_eq!(large_range.join(&large_range, &()), large_range);

    // Intersection / meet
    assert_eq!(top.meet(&bottom, &()), bottom);
    assert_eq!(bottom.meet(&singleton, &()), bottom);
    assert_eq!(top.meet(&singleton, &()), singleton);
    assert_eq!(singleton.meet(&small_range_a, &()), singleton);
    assert_eq!(small_range_a.meet(&small_range_b, &()), bottom);
    assert_eq!(large_range.meet(&small_range_b, &()), small_range_b);

    // Widening
    assert_eq!(small_range_a.widen(&large_range, &(), 0), large_range);
    assert_eq!(large_range.widen(&small_range_a, &(), 0), top);
    let bumped_max = Interval {
        min: small_range_a.min,
        max: small_range_a.max + 1,
    };
    let widened_max = Interval {
        min: small_range_a.min,
        max: INF,
    };
    let decremented_min = Interval {
        min: small_range_a.min - 1,
        max: small_range_a.max,
    };
    let widened_min = Interval {
        min: NEG_INF,
        max: small_range_a.max,
    };
    assert_eq!(bumped_max.widen(&small_range_a, &(), 0), widened_max);
    assert_eq!(decremented_min.widen(&small_range_a, &(), 0), widened_min);
    assert_eq!(large_range.widen(&bottom, &(), 0), large_range);

    // Narrowing
    assert_eq!(large_range.narrow(&top, &(), 0), large_range);

    // Conversions
    assert_eq!(Interval::from(Sign::Bottom), bottom);
    assert_eq!(Interval::from(Sign::Top), top);
    assert_eq!(Interval::from(Sign::Zero), Interval::from(0));
    assert!(singleton < Interval::from(Sign::Positive));
    assert!(!(singleton < Interval::from(Sign::Negative)));

    // Arithmetic
    assert_eq!(singleton + singleton, Interval::from(10));
    assert_eq!(-singleton, Interval::from(-5));
    assert_eq!(singleton + top, top);
    assert_eq!(-top, top);
    assert_eq!(-small_range_b, Interval { min: -20, max: -11 });
    let add_expected = Interval { min: 5, max: 15 };
    assert_eq!(small_range_a + singleton, add_expected);
    assert_eq!(singleton + small_range_a, add_expected);
    let add_expected2 = Interval { min: 11, max: 30 };
    assert_eq!(small_range_a + small_range_b, add_expected2);
    assert_eq!(small_range_b + small_range_a, add_expected2);
    let sub_expected = Interval { min: -20, max: -1 };
    assert_eq!(small_range_a - small_range_b, sub_expected);
    let mul_expected1 = Interval { min: 0, max: 200 };
    assert_eq!(small_range_a * small_range_b, mul_expected1);
    assert_eq!(small_range_a * top, top);
    let neg_range = Interval { min: -10, max: -2 };
    let mul_expected2 = Interval {
        min: -200,
        max: -22,
    };
    assert_eq!(small_range_b * neg_range, mul_expected2);
    let mul_expected3 = Interval { min: 4, max: 100 };
    assert_eq!(neg_range * neg_range, mul_expected3);

    // Printing
    assert_eq!(format!("{singleton:?}"), "[5, 5]");
    assert_eq!(format!("{small_range_a:?}"), "[0, 10]");
    assert_eq!(format!("{top:?}"), "[-inf, inf]");
    assert_eq!(format!("{bottom:?}"), "[inf, -inf]");
}

#[test]
fn congruence_domain_test() {
    let top = Congruence::top(&());
    let one = Congruence::from(1, 0);
    let two = Congruence::from(2, 0);
    let c1mod2 = Congruence::from(1, 2);
    let c1mod3 = Congruence::from(1, 3);
    let c1mod4 = Congruence::from(1, 4);
    let c1mod5 = Congruence::from(1, 5);
    let c2mod3 = Congruence::from(2, 3);
    let c1mod6 = Congruence::from(1, 6);
    let bot = Congruence::bottom(&());

    // Ordering
    assert!(c1mod2 > bot);
    assert!(bot < c1mod2);
    assert!(bot < top);
    assert!(c1mod2 < top);
    assert!(c2mod3 < top);
    assert!(c1mod2 > c1mod6);
    assert!(c1mod3 > c1mod6);
    assert!(one < c1mod2);
    assert!(c1mod2 > one);
    assert!(!(c2mod3 > c1mod3) && !(c2mod3 < c1mod3));
    assert!(!(c2mod3 > c1mod6) && !(c2mod3 < c1mod6));
    assert!(!(c1mod5 > c1mod6) && !(c1mod5 < c1mod6));
    assert!(!(c2mod3 > one) && !(c2mod3 < one));
    assert!(!(two > one) && !(two < one));

    // Join
    assert_eq!(c1mod6.join(&c1mod4, &()), c1mod2);
    assert_eq!(c1mod4.join(&c1mod6, &()), c1mod2);
    assert_eq!(c1mod2.join(&c1mod3, &()), top);
    assert_eq!(c1mod3.join(&c1mod2, &()), top);
    assert_eq!(c1mod3.join(&one, &()), c1mod3);
    assert_eq!(one.join(&c1mod3, &()), c1mod3);
    assert_eq!(one.join(&c2mod3, &()), top);
    assert_eq!(c2mod3.join(&one, &()), top);
    assert_eq!(
        c1mod4.join(&Congruence::from(3, 6), &()),
        Congruence::from(1, 2)
    );

    // Meet
    assert_eq!(
        Congruence::from(0, 3).meet(&Congruence::from(3, 7), &()),
        Congruence::from(0, 21)
    );
    assert_eq!(Congruence::from(0, 3).meet(&c1mod3, &()), bot);
    assert_eq!(c1mod4.meet(&c1mod2, &()), c1mod4);
    assert_eq!(one.meet(&c1mod2, &()), one);
    assert_eq!(one.meet(&two, &()), bot);

    // Operations
    assert_eq!(one + two, Congruence::from(3, 0));
    assert_eq!(c1mod3 + c2mod3, Congruence::from(0, 3));
    assert_eq!(c1mod3 * two, Congruence::from(2, 6));
    assert_eq!(c1mod3 * c1mod6, Congruence::from(1, 3));
    assert!(c1mod4.disjoint(Congruence::from(0, 6)));
}

#[test]
fn set_domain_tests() {
    let ctx = PowerSetTop(PowerSet::<i32>(HashSet::from([1, 2, 3, 4, 5])));
    let small_set = PowerSet::<i32>(HashSet::from([1, 2, 3]));
    let small_set2 = PowerSet::<i32>(HashSet::from([2, 3, 4]));
    let union = PowerSet::<i32>(HashSet::from([1, 2, 3, 4]));
    let intersection = PowerSet::<i32>(HashSet::from([2, 3]));

    assert!(!(small_set2 < small_set));
    assert!(!(small_set2 > small_set));
    assert!(small_set < union);
    assert!(small_set2 < union);
    assert_eq!(small_set.join(&small_set2, &ctx), union);
    assert_eq!(small_set2.join(&small_set, &ctx), union);
    assert_eq!(small_set.meet(&small_set2, &ctx), intersection);
    assert_eq!(small_set2.meet(&small_set, &ctx), intersection);

    {
        let ctx = PowerSetTop(PowerSet::<i32>(HashSet::from([1, 2])));
        let all = [
            PowerSet::<i32>(HashSet::from([])),
            PowerSet::<i32>(HashSet::from([1])),
            PowerSet::<i32>(HashSet::from([2])),
            PowerSet::<i32>(HashSet::from([1, 2])),
        ];
        finite_domain_properties(&all, &ctx);
    }
}

#[test]
fn bitset_domain_tests() {
    let ctx = BitSetTop(5);
    let bottom = BitSet::bottom(&ctx);
    let small_set = BitSet::from(&ctx, &[1, 2, 3]);
    let small_set2 = BitSet::from(&ctx, &[2, 3, 4]);
    let union = BitSet::from(&ctx, &[1, 2, 3, 4]);
    let intersection = BitSet::from(&ctx, &[2, 3]);

    assert!(!(small_set2 < small_set));
    assert!(!(small_set2 > small_set));
    assert!(small_set < union);
    assert!(small_set2 < union);
    assert_eq!(small_set.join(&small_set2, &ctx), union);
    assert_eq!(small_set2.join(&small_set, &ctx), union);
    assert_eq!(small_set.meet(&small_set2, &ctx), intersection);
    assert_eq!(small_set2.meet(&small_set, &ctx), intersection);

    assert_eq!(format!("{bottom:?}"), "{}".to_owned());
    assert_eq!(format!("{small_set:?}"), "{1, 2, 3}".to_owned());
    assert_eq!(
        format!("{:?}", BitSet::top(&ctx)),
        "{0, 1, 2, 3, 4}".to_owned()
    );

    {
        let ctx = BitSetTop(2);
        let all = [
            BitSet::from(&ctx, &[]),
            BitSet::from(&ctx, &[0]),
            BitSet::from(&ctx, &[1]),
            BitSet::from(&ctx, &[0, 1]),
        ];
        finite_domain_properties(&all, &ctx);
    }
}

#[test]
fn flipped_sign_domain_tests() {
    use Sign::*;
    let bottom = Flipped(Top);
    let positive = Flipped(Positive);
    let negative = Flipped(Negative);
    let zero = Flipped(Zero);
    let top = Flipped(Bottom);

    // Comparisons, join
    assert_eq!(zero.join(&zero, &()), zero);
    assert_eq!(negative.join(&positive, &()), top);
    assert_eq!(bottom.join(&negative, &()), negative);
    assert_eq!(negative.join(&top, &()), top);

    // Meet
    assert_eq!(bottom.meet(&zero, &()), bottom);
    assert_eq!(top.meet(&zero, &()), zero);
    assert_eq!(negative.meet(&zero, &()), Flipped(NonPos));

    // Pretty printing
    assert_eq!(format!("{bottom:?}"), "Flipped(Top)");

    let all = [
        Flipped(Bottom),
        Flipped(Top),
        Flipped(Negative),
        Flipped(Zero),
        Flipped(Positive),
        Flipped(NonNeg),
        Flipped(NonZero),
        Flipped(NonPos),
    ];
    finite_domain_properties(&all, &());
}

#[test]
fn array_domain_test() {
    use Sign::*;
    let bottom = Array::<Sign, 3>::bottom(&());
    let top = Array::<Sign, 3>::top(&());
    let pos_zero_neg = Array([Positive, Zero, Negative]);
    let pos_zero_bottom = Array([Positive, Zero, Bottom]);
    let neg_zero_top = Array([Negative, Zero, Top]);

    assert!(pos_zero_bottom < pos_zero_neg);
    assert!(!(pos_zero_neg < neg_zero_top));
    assert!(!(neg_zero_top < pos_zero_neg));

    assert_eq!(
        pos_zero_neg.join(&neg_zero_top, &()),
        Array([NonZero, Zero, Top])
    );
    assert_eq!(pos_zero_neg.join(&top, &()), top);
    assert_eq!(bottom.join(&neg_zero_top, &()), neg_zero_top);
    assert_eq!(
        pos_zero_neg.meet(&neg_zero_top, &()),
        Array([Bottom, Zero, Negative])
    );
    assert_eq!(pos_zero_neg.meet(&top, &()), pos_zero_neg);
    assert_eq!(bottom.meet(&neg_zero_top, &()), bottom);

    // Pretty printing
    assert_eq!(
        format!("{pos_zero_neg:?}"),
        "Array([Positive, Zero, Negative])"
    );

    // Properties
    {
        let all_signs = [
            Bottom, Top, Negative, Zero, Positive, NonNeg, NonZero, NonPos,
        ];
        let mut all = Vec::new();
        for (x, y) in all_signs.iter().cartesian_product(all_signs) {
            all.push(Array([*x, y]));
        }
        finite_domain_properties(&all, &());
    }
}

#[test]
fn product_domain_test() {
    use Sign::*;
    type MyDomain = Prod3<Sign, (), BitSet>;
    let bit_ctx = BitSetTop(2);
    let ctx = ((), (), bit_ctx);
    let top = MyDomain::top(&ctx);
    let a = Prod3(Zero, (), BitSet::from(&bit_ctx, &[1]));
    let b = Prod3(Top, (), BitSet::bottom(&bit_ctx));
    let c = Prod3(Zero, (), BitSet::from(&bit_ctx, &[0, 1]));

    assert!(!(b > a));
    assert!(!(b < a));
    assert!(c > a);
    assert_eq!(
        a.join(&b, &ctx),
        Prod3(Top, (), BitSet::from(&bit_ctx, &[1]))
    );
    assert_eq!(
        a.meet(&c, &ctx),
        Prod3(Zero, (), BitSet::from(&bit_ctx, &[1]))
    );
    assert_eq!(b.meet(&c, &ctx), Prod3(Zero, (), BitSet::bottom(&bit_ctx)));
    assert_eq!(format!("{top:?}"), "Prod3(Top, (), {0, 1})");

    // Properties
    {
        let all_signs = [
            Bottom, Top, Negative, Zero, Positive, NonNeg, NonZero, NonPos,
        ];
        let mut all = Vec::new();
        for (x, y) in all_signs.iter().cartesian_product(all_signs) {
            all.push(Prod2(*x, y));
        }
        finite_domain_properties(&all, &((), ()));
    }
}

#[test]
fn flat_domain_test() {
    let bottom = Flat::bottom(&());
    let top = Flat::top(&());
    let a = Flat::Element(5);
    let b = Flat::Element(3);

    assert!(a != b);
    assert!(!(a < b));
    assert!(!(a > b));
    assert_eq!(a.join(&b, &()), top);
    assert_eq!(a.meet(&b, &()), bottom);
    assert_eq!(a.join(&a, &()), a);
    assert_eq!(a.meet(&a, &()), a);
    assert_eq!(format!("{top:?}"), "Top");
    assert_eq!(format!("{a:?}"), "Element(5)");
    assert_eq!(format!("{bottom:?}"), "Bottom");

    // Properties
    {
        use Sign::*;
        let all = [
            Flat::Bottom,
            Flat::Top,
            Flat::Element(Top),
            Flat::Element(Bottom),
            Flat::Element(Negative),
            Flat::Element(Zero),
            Flat::Element(Positive),
            Flat::Element(NonNeg),
            Flat::Element(NonZero),
            Flat::Element(NonPos),
        ];
        finite_domain_properties(&all, &());
    }
}

#[test]
fn bool_domain_test() {
    let bottom = bool::bottom(&());
    let top = bool::top(&());

    assert_eq!(top.join(&bottom, &()), top);
    assert_eq!(top.meet(&bottom, &()), bottom);

    let all = [true, false];
    finite_domain_properties(&all, &());
}

#[test]
fn natural_domain_test() {
    let bottom = u64::bottom(&());

    assert!(5 > bottom);
    assert_eq!(bottom.join(&bottom, &()), bottom);
    assert_eq!(bottom.join(&5, &()), bottom);
    assert_eq!(5.join(&3, &()), 3);
}

#[test]
fn lifted_domain_test() {
    let bottom = Lift::<bool>::bottom(&());
    let false_val = Some(false);
    let true_val = Some(true);

    assert!(true_val > false_val);
    assert_eq!(bottom.join(&true_val, &()), true_val);
    assert_eq!(bottom.meet(&true_val, &()), bottom);
    assert_eq!(false_val.join(&true_val, &()), true_val);
    assert_eq!(false_val.meet(&true_val, &()), false_val);
    assert_eq!(format!("{bottom:?}"), "None");
    assert_eq!(format!("{true_val:?}"), "Some(true)");

    let all = [None, Some(true), Some(false)];
    finite_domain_properties(&all, &());
}

#[test]
fn map_domain_test() {
    type MyDomain = Map<&'static str, Sign>;
    use Sign::*;
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
    assert_eq!(a.join(&b, &ctx), b);
    assert_eq!(
        b.join(&c, &ctx),
        Map(HashMap::from([("Foo", Top), ("Bar", NonZero),]))
    );
    assert_eq!(a.meet(&b, &ctx), Map(HashMap::from([("Foo", Zero),])));
    assert_eq!(
        b.meet(&c, &ctx),
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
    type MyDomain = Stack2<Sign, Interval>;
    let ctx = ((), ());
    let bottom = MyDomain::bottom(&ctx);
    let top = MyDomain::top(&ctx);
    let a = MyDomain::S1(Sign::Negative);
    let b = MyDomain::S1(Sign::Positive);
    let c = MyDomain::S2(Interval { min: 5, max: 10 });
    let d = MyDomain::S2(Interval { min: 8, max: 12 });
    let e = MyDomain::S2(Interval { min: 4, max: 13 });

    assert!(!(a < b));
    assert!(!(a > b));
    assert!(a < e);
    assert!(d < e);
    assert_eq!(a.join(&top, &ctx), top);
    assert_eq!(a.join(&bottom, &ctx), a);
    assert_eq!(a.meet(&top, &ctx), a);
    assert_eq!(a.meet(&bottom, &ctx), bottom);
    assert_eq!(a.join(&b, &ctx), MyDomain::S1(Sign::NonZero));
    assert_eq!(a.join(&c, &ctx), c);
    assert_eq!(a.meet(&c, &ctx), a);
    assert_eq!(c.join(&d, &ctx), MyDomain::S2(Interval { min: 5, max: 12 }));
    assert_eq!(c.meet(&d, &ctx), MyDomain::S2(Interval { min: 8, max: 10 }));

    assert_eq!(format!("{bottom:?}"), "Bottom");
    assert_eq!(format!("{top:?}"), "S2([-inf, inf])");
    assert_eq!(format!("{a:?}"), "S1(Negative)");
    assert_eq!(format!("{e:?}"), "S2([4, 13])");

    // Properties
    {
        use Sign::*;
        let all = [
            Stack2::Bottom,
            Stack2::S1(Bottom),
            Stack2::S1(Top),
            Stack2::S1(Negative),
            Stack2::S1(Zero),
            Stack2::S1(Positive),
            Stack2::S1(NonNeg),
            Stack2::S1(NonZero),
            Stack2::S1(NonPos),
            Stack2::S2(true),
            Stack2::S2(false),
        ];
        finite_domain_properties(&all, &((), ()));
    }
}

#[test]
fn union_domain_test() {
    type MyDomain = Union2<Sign, Interval>;
    let ctx = ((), ());
    let bottom = MyDomain::bottom(&ctx);
    let top = MyDomain::top(&ctx);
    let a = MyDomain::U1(Sign::Negative);
    let b = MyDomain::U1(Sign::Positive);
    let c = MyDomain::U2(Interval { min: 5, max: 10 });
    let d = MyDomain::U2(Interval { min: 8, max: 12 });
    let e = MyDomain::U2(Interval { min: 4, max: 13 });

    assert!(!(a < b));
    assert!(!(a > b));
    assert!(d < e);
    assert!(!(a < e));
    assert!(!(a > e));
    assert_eq!(a.join(&top, &ctx), top);
    assert_eq!(a.join(&bottom, &ctx), a);
    assert_eq!(a.meet(&top, &ctx), a);
    assert_eq!(a.meet(&bottom, &ctx), bottom);
    assert_eq!(a.join(&b, &ctx), MyDomain::U1(Sign::NonZero));
    assert_eq!(a.join(&c, &ctx), top);
    assert_eq!(a.meet(&c, &ctx), bottom);
    assert_eq!(c.join(&d, &ctx), MyDomain::U2(Interval { min: 5, max: 12 }));
    assert_eq!(c.meet(&d, &ctx), MyDomain::U2(Interval { min: 8, max: 10 }));

    assert_eq!(format!("{bottom:?}"), "Bottom");
    assert_eq!(format!("{top:?}"), "Top");
    assert_eq!(format!("{a:?}"), "U1(Negative)");
    assert_eq!(format!("{e:?}"), "U2([4, 13])");

    // Properties
    {
        use Sign::*;
        let all = [
            Union2::Bottom,
            Union2::U1(Bottom),
            Union2::U1(Top),
            Union2::U1(Negative),
            Union2::U1(Zero),
            Union2::U1(Positive),
            Union2::U1(NonNeg),
            Union2::U1(NonZero),
            Union2::U1(NonPos),
            Union2::U2(true),
            Union2::U2(false),
        ];
        finite_domain_properties(&all, &((), ()));
    }
}

#[test]
fn finite_domain_test() {
    #[derive(Debug, Clone, PartialEq, Eq)]
    #[rustfmt::skip]
    enum Elems { A, B, C, D, E, F, G }
    use Elems::*;

    assert!(matches!(
        FiniteCtx::new(&[A], &[]),
        Err(FiniteDomainError::LatticeTooSmall)
    ));
    assert!(matches!(
        FiniteCtx::new(&[A, A], &[]),
        Err(FiniteDomainError::HasDuplicateElements)
    ));
    assert!(matches!(
        FiniteCtx::new_idx(&[A, B], &[(1, 5)]),
        Err(FiniteDomainError::NonExistentEdge)
    ));
    assert!(matches!(
        FiniteCtx::new(&[A, B], &[(A, C)]),
        Err(FiniteDomainError::NonExistentEdge)
    ));

    // Not a lattice, no greatest lower bound of B and C.
    //     A
    //    / \
    //   B   C
    assert!(matches!(
        FiniteCtx::new(&[A, B, C], &[(B, A), (C, A)]),
        Err(FiniteDomainError::NoGreatestLowerBound(1, 2))
    ));

    // Not a lattice, no least upper bound B and C.
    //   B   C
    //    \ /
    //     A
    assert!(matches!(
        FiniteCtx::new(&[A, B, C], &[(A, B), (A, C)]),
        Err(FiniteDomainError::NoLeastUpperBound(1, 2))
    ));

    //     A
    //    / \
    //   B   C
    //    \ /
    //     D
    assert!(matches!(
        FiniteCtx::new(&[B, A, C, D], &[(B, A), (C, A), (D, B), (D, C)]),
        Err(FiniteDomainError::TopNotFirst)
    ));
    assert!(matches!(
        FiniteCtx::new(&[A, B, D, C], &[(B, A), (C, A), (D, B), (D, C)]),
        Err(FiniteDomainError::BottomNotLast)
    ));
    assert!(FiniteCtx::new(&[A, B, C, D], &[(B, A), (C, A), (D, B), (D, C)]).is_ok());

    //     A
    //    / \
    //   B   C
    //   | X |
    //   D   E
    //    \ /
    //     F
    assert!(matches!(
        FiniteCtx::new(
            &[A, B, C, D, E, F],
            &[
                (B, A),
                (C, A),
                (D, B),
                (E, B),
                (D, C),
                (E, C),
                (F, E),
                (F, D)
            ]
        ),
        Err(FiniteDomainError::NoGreatestLowerBound(1, 2))
    ));

    //       A
    //     / | \
    //    /  |  \
    //   B   C   E
    //    \ /    |
    //     D     F
    //      \   /
    //        G
    let ctx = FiniteCtx::new(
        &[A, B, C, D, E, F, G],
        &[
            (B, A),
            (C, A),
            (E, A),
            (D, B),
            (D, C),
            (F, E),
            (G, D),
            (G, F),
        ],
    )
    .unwrap();

    // Encodings
    let a = ctx.encode(&A).unwrap();
    let b = ctx.encode(&B).unwrap();
    let c = ctx.encode(&C).unwrap();
    let d = ctx.encode(&D).unwrap();
    let e = ctx.encode(&E).unwrap();
    let f = ctx.encode(&F).unwrap();
    let g = ctx.encode(&G).unwrap();

    assert_eq!(ctx.decode(&a), &A);
    assert_eq!(ctx.decode(&b), &B);
    assert_eq!(ctx.decode(&c), &C);
    assert_eq!(ctx.decode(&d), &D);
    assert_eq!(ctx.decode(&e), &E);
    assert_eq!(ctx.decode(&f), &F);
    assert_eq!(ctx.decode(&g), &G);

    assert_eq!(a, Finite::top(&ctx));
    assert_eq!(g, Finite::bottom(&ctx));

    // Lattice operations
    assert!(b < a);
    assert!(!(b < c));
    assert!(!(c < b));
    assert!(g < b);

    assert_eq!(b.join(&c, &ctx), a);
    assert_eq!(b.meet(&c, &ctx), d);
    assert_eq!(b.join(&e, &ctx), a);
    assert_eq!(b.meet(&e, &ctx), g);

    assert_eq!(b.join(&a, &ctx), a);
    assert_eq!(b.join(&g, &ctx), b);
    assert_eq!(b.meet(&a, &ctx), b);
    assert_eq!(b.meet(&g, &ctx), g);

    let all = [a, b, c, d, e, f, g];
    finite_domain_properties(&all, &ctx);
}
