use crate::{
    ast::{ASTContext, NodeRef},
    cfg::*,
    eval::Walk,
};

use cairo::*;
use rand::{prelude::ThreadRng, Rng};

const RADIUS: f64 = 3.0;
const WIDTH: f64 = 500.0;
const HEIGHT: f64 = 500.0;

/// Returns an SVG that represents a list of random walks.
pub fn render_random_walk(walks: &[Walk], ctxt: &ASTContext, dots_only: bool) -> String {
    let stream: Box<Vec<u8>> = Box::new(Vec::new());
    let surface =
        SvgSurface::for_stream(WIDTH, HEIGHT, stream).expect("Failed to create cairo surface.");
    let cr = Context::new(&surface).expect("Failed to create context.");

    render_random_walk_impl(&cr, walks, ctxt, dots_only);

    let stream = surface.finish_output_stream().unwrap();
    let stream = stream.downcast::<Box<Vec<u8>>>().unwrap();

    String::from_utf8(**stream).expect("Generated SVG is not valid UTF8.")
}

fn render_random_walk_impl(cr: &Context, walks: &[Walk], ctxt: &ASTContext, dots_only: bool) {
    // Set background to white.
    cr.set_source_rgb(1.0, 1.0, 1.0);
    cr.new_path();
    cr.rectangle(0.0, 0.0, WIDTH, HEIGHT);
    cr.fill().expect("Failed to fill background.");

    // Move origo to the middle.
    cr.translate(WIDTH / 2.0, HEIGHT / 2.0);

    // TODO: Render the visualization of the analysis here.

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
            let prev_y: f64 = walk[i - 1].pos.y.into();
            let y: f64 = walk[i].pos.y.into();
            cr.new_path();
            if let NodeRef::Rotation(rot) = ctxt.node_to_ref((&walk[i].op).into()) {
                let orig_x: f64 = rot.origin.x.value.to_num().into();
                let orig_y: f64 = rot.origin.y.value.to_num().into();
                let x_diff: f64 = orig_x - walk[i].pos.x as f64;
                let y_diff: f64 = orig_y - walk[i].pos.y as f64;
                let dist = f64::sqrt(x_diff * x_diff + y_diff * y_diff);
                let deg_prev = f64::atan2(-prev_y, walk[i - 1].pos.x.into());
                let deg_cur = f64::atan2(-y, walk[i].pos.x.into());
                cr.arc(orig_x, -orig_y, dist, deg_cur, deg_prev)
            } else {
                cr.move_to(walk[i - 1].pos.x.into(), -prev_y);
                cr.line_to(walk[i].pos.x.into(), -y);
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
        let y: f64 = step.pos.y.into();
        cr.arc(
            step.pos.x.into(),
            -y,
            RADIUS,
            0.0,
            2.0 * std::f64::consts::PI,
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

struct ColorPicker {
    current: usize,
    rng: ThreadRng,
}

impl ColorPicker {
    fn new() -> Self {
        Self {
            current: 0,
            rng: rand::thread_rng(),
        }
    }

    fn next(&mut self) -> Rgb {
        if self.current < COLORS.len() {
            return COLORS[self.current];
        }
        Rgb(
            self.rng.gen_range(0.0..=1.0),
            self.rng.gen_range(0.0..=1.0),
            self.rng.gen_range(0.0..=1.0),
        )
    }
}

impl Default for ColorPicker {
    fn default() -> Self {
        ColorPicker::new()
    }
}
