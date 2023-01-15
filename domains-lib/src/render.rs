use crate::{
    ast::{ASTContext, NodeRef, Operation},
    eval::Walk,
};

use analysis::domains::{IntervalDomain, JoinSemiLattice, SignDomain, Vec2Domain};
use cairo::{Context, SvgSurface};
use rand::{prelude::ThreadRng, Rng};
use utils::{Polygon, Vec2};

const RADIUS: f64 = 3.0;
const WIDTH: f64 = 500.0;
const HEIGHT: f64 = 500.0;

fn clip_width(pos: i64) -> f64 {
    if pos == analysis::domains::INF {
        WIDTH / 2_f64
    } else if pos == analysis::domains::NEG_INF {
        -WIDTH / 2_f64
    } else {
        pos as f64
    }
}
fn clip_height(pos: i64) -> f64 {
    if pos == analysis::domains::INF {
        HEIGHT / 2_f64
    } else if pos == analysis::domains::NEG_INF {
        -HEIGHT / 2_f64
    } else {
        pos as f64
    }
}

/// Returns an SVG that represents a list of random walks and the are
/// they can cover according to an analysis.
pub fn render_random_walk(
    walks: &[Walk],
    ctxt: &ASTContext,
    covered: &[Polygon],
    dots_only: bool,
) -> String {
    let stream: Box<Vec<u8>> = Box::default();
    let surface =
        SvgSurface::for_stream(WIDTH, HEIGHT, stream).expect("Failed to create cairo surface.");
    let cr = Context::new(&surface).expect("Failed to create context.");

    // Set background to white.
    cr.set_source_rgb(1.0, 1.0, 1.0);
    cr.new_path();
    cr.rectangle(0.0, 0.0, WIDTH, HEIGHT);
    cr.fill().expect("Failed to fill background.");

    // Move origo to the middle.
    cr.translate(WIDTH / 2.0, HEIGHT / 2.0);

    render_covered_area(&cr, covered);

    render_random_walk_impl(&cr, walks, ctxt, dots_only);

    let stream = surface.finish_output_stream().unwrap();
    let stream = stream.downcast::<Box<Vec<u8>>>().unwrap();

    String::from_utf8(**stream).expect("Generated SVG is not valid UTF8.")
}

fn render_covered_area(cr: &Context, covered: &[Polygon]) {
    // Set background to grey
    cr.set_source_rgb(0.75, 0.75, 0.75);

    for p in covered {
        cr.new_path();
        let mut first = true;
        for point in p {
            if first {
                cr.move_to(clip_width(point.x), -clip_height(point.y));
                first = false;
                continue;
            }
            cr.line_to(clip_width(point.x), -clip_height(point.y));
        }
        cr.close_path();
        cr.fill().expect("Failed to fill polygon.");
    }
}

fn render_random_walk_impl(cr: &Context, walks: &[Walk], ctxt: &ASTContext, dots_only: bool) {
    // Draw the axes.
    cr.set_source_rgb(0.0, 0.0, 0.0);
    cr.new_path();
    cr.move_to(0.0, -HEIGHT / 2.0);
    cr.line_to(0.0, HEIGHT / 2.0);
    cr.move_to(-WIDTH / 2.0, 0.0);
    cr.line_to(WIDTH / 2.0, 0.0);
    cr.stroke().unwrap();

    let mut picker = ColorPicker::new();
    for walk in walks {
        render_random_path(cr, picker.next(), walk, ctxt, dots_only);
    }
}

fn render_random_path(cr: &Context, color: Rgb, walk: &Walk, ctxt: &ASTContext, dots_only: bool) {
    // Render lines.
    if !dots_only {
        let Rgb(red, green, blue) = color;
        cr.set_source_rgb(red, green, blue);
        for i in 1..walk.len() {
            // Had to hoist these due to lame type inference.
            let prev_y = walk[i - 1].pos.y as f64;
            let y = walk[i].pos.y as f64;
            cr.new_path();
            if let NodeRef::Rotation(rot) = ctxt.op_to_ref(walk[i].op) {
                let orig_x: f64 = rot.origin.x.value.to_num().into();
                let orig_y: f64 = rot.origin.y.value.to_num().into();
                let x_diff: f64 = orig_x - (walk[i].pos.x as f64);
                let y_diff: f64 = orig_y - (walk[i].pos.y as f64);
                let dist = f64::sqrt(x_diff * x_diff + y_diff * y_diff);
                let deg_prev = f64::atan2(-prev_y, walk[i - 1].pos.x as f64);
                let deg_cur = f64::atan2(-y, walk[i].pos.x as f64);
                cr.arc(orig_x, -orig_y, dist, deg_cur, deg_prev);
            } else {
                cr.move_to(walk[i - 1].pos.x as f64, -prev_y);
                cr.line_to(walk[i].pos.x as f64, -y);
            }
            cr.stroke().unwrap();
        }
    }

    // Render the dots.
    for step in walk {
        cr.new_path();
        let green = if matches!(step.op, Operation::Init(_)) {
            START_DOT_COLOR.1
        } else {
            DOT_COLOR.1
        };
        cr.set_source_rgb(DOT_COLOR.0, green, DOT_COLOR.2);
        let y = step.pos.y as f64;
        cr.arc(
            step.pos.x as f64,
            -y,
            RADIUS,
            0.0,
            2.0 * core::f64::consts::PI,
        );
        cr.fill().unwrap();
    }
}

#[derive(Clone, Copy)]
struct Rgb(f64, f64, f64);
#[allow(clippy::eq_op)]
const COLORS: [Rgb; 21] = [
    Rgb(230. / 255., 25. / 255., 75. / 255.),
    Rgb(60. / 255., 180. / 255., 75. / 255.),
    Rgb(0. / 255., 130. / 255., 200. / 255.),
    Rgb(245. / 255., 130. / 255., 48. / 255.),
    Rgb(145. / 255., 30. / 255., 180. / 255.),
    Rgb(70. / 255., 240. / 255., 240. / 255.),
    Rgb(240. / 255., 50. / 255., 230. / 255.),
    Rgb(210. / 255., 245. / 255., 60. / 255.),
    Rgb(250. / 255., 190. / 255., 212. / 255.),
    Rgb(0. / 255., 128. / 255., 128. / 255.),
    Rgb(220. / 255., 190. / 255., 255. / 255.),
    Rgb(170. / 255., 110. / 255., 40. / 255.),
    Rgb(255. / 255., 250. / 255., 200. / 255.),
    Rgb(255. / 255., 250. / 255., 200. / 255.),
    Rgb(128. / 255., 0. / 255., 0. / 255.),
    Rgb(170. / 255., 255. / 255., 195. / 255.),
    Rgb(128. / 255., 128. / 255., 0. / 255.),
    Rgb(255. / 255., 215. / 255., 180. / 255.),
    Rgb(0. / 255., 0. / 255., 128. / 255.),
    Rgb(255. / 255., 225. / 255., 25. / 255.),
    Rgb(128. / 255., 128. / 255., 128. / 255.),
];
#[deny(clippy::eq_op)]
const DOT_COLOR: Rgb = Rgb(0.0, 0.0, 0.0);
const START_DOT_COLOR: Rgb = Rgb(0.0, 1.0, 0.0);

#[derive(Default)]
struct ColorPicker {
    current: usize,
    rng: ThreadRng,
}

impl ColorPicker {
    fn new() -> Self {
        Self::default()
    }

    fn next(&mut self) -> Rgb {
        if self.current < COLORS.len() {
            let rgb = COLORS[self.current];
            self.current += 1;
            return rgb;
        }
        Rgb(
            self.rng.gen_range(0.0..=1.0),
            self.rng.gen_range(0.0..=1.0),
            self.rng.gen_range(0.0..=1.0),
        )
    }
}

pub trait RenderableDomain: JoinSemiLattice {
    fn render(&self) -> Vec<Polygon>;
}

impl RenderableDomain for IntervalDomain {
    fn render(&self) -> Vec<Polygon> {
        vec![vec![Vec2 { x: self.min, y: 0 }, Vec2 { x: self.max, y: 0 }]]
    }
}

impl RenderableDomain for SignDomain {
    fn render(&self) -> Vec<Polygon> {
        match *self {
            SignDomain::Top => vec![vec![
                Vec2 {
                    x: analysis::domains::NEG_INF,
                    y: 0,
                },
                Vec2 {
                    x: analysis::domains::INF,
                    y: 0,
                },
            ]],
            SignDomain::Bottom => vec![],
            SignDomain::Negative => vec![vec![
                Vec2 {
                    x: analysis::domains::NEG_INF,
                    y: 0,
                },
                Vec2 { x: 0, y: 0 },
            ]],
            SignDomain::Positive => vec![vec![
                Vec2 { x: 0, y: 0 },
                Vec2 {
                    x: analysis::domains::INF,
                    y: 0,
                },
            ]],
            SignDomain::Zero => vec![vec![Vec2 { x: 0, y: 0 }, Vec2 { x: 0, y: 0 }]],
        }
    }
}

impl<T: JoinSemiLattice + RenderableDomain> RenderableDomain for Vec2Domain<T> {
    fn render(&self) -> Vec<Polygon> {
        let xs = self.x.render();
        let ys = self.y.render();

        assert!(xs.len() == ys.len());
        let mut polys = Vec::new();
        for i in 0..xs.len() {
            polys.push(Vec::new());
            let mut first = true;
            for x in &xs[i] {
                let mut y_coords = ys[i].clone();
                if first {
                    first = false;
                } else {
                    y_coords.reverse();
                }
                for y in y_coords {
                    polys.last_mut().unwrap().push(Vec2 { x: x.x, y: y.x })
                }
            }
        }

        polys
    }
}
