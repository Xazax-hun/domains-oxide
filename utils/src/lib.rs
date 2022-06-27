use std::io::BufWriter;
use std::io::Write;

pub struct DiagnosticEmitter {
    out: BufWriter<Box<dyn std::io::Write>>,
    err: BufWriter<Box<dyn std::io::Write>>,
}

impl DiagnosticEmitter {
    pub fn new(out: Box<dyn std::io::Write>, err: Box<dyn std::io::Write>) -> Self {
        DiagnosticEmitter {
            out: BufWriter::new(out),
            err: BufWriter::new(err),
        }
    }

    pub fn to_out(&mut self, msg: &str) {
        self.out.write_all(msg.as_bytes()).unwrap();
    }

    pub fn to_err(&mut self, msg: &str) {
        self.err.write_all(msg.as_bytes()).unwrap();
    }

    pub fn out_buffer(&self) -> &[u8] {
        self.out.buffer()
    }

    pub fn err_buffer(&self) -> &[u8] {
        self.err.buffer()
    }

    pub fn error(&mut self, line: u32, message: &str) {
        self.report(line, "", message);
    }

    pub fn report(&mut self, line: u32, item: &str, message: &str) {
        let _ = self
            .err
            .write(format!("[line {line}] Error {item}: {message}\n").as_bytes());
    }
}

#[derive(Clone, Copy)]
pub struct Vec2 {
    pub x: i32,
    pub y: i32,
}

impl std::ops::Add<Vec2> for Vec2 {
    type Output = Vec2;

    fn add(self, rhs: Vec2) -> Vec2 {
        Vec2 {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl std::ops::AddAssign<Vec2> for Vec2 {
    fn add_assign(&mut self, rhs: Vec2) {
        *self = *self + rhs;
    }
}

impl std::ops::Sub<Vec2> for Vec2 {
    type Output = Vec2;

    fn sub(self, rhs: Vec2) -> Vec2 {
        Vec2 {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl std::ops::SubAssign<Vec2> for Vec2 {
    fn sub_assign(&mut self, rhs: Vec2) {
        *self = *self - rhs;
    }
}
