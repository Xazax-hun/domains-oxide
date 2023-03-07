use core::fmt::Display;
use std::io::BufWriter;
use std::io::Write;

pub struct DiagnosticEmitter {
    out: BufWriter<Box<dyn Write>>,
    err: BufWriter<Box<dyn Write>>,
}

impl DiagnosticEmitter {
    pub fn new(out: Box<dyn Write>, err: Box<dyn Write>) -> Self {
        Self {
            out: BufWriter::new(out),
            err: BufWriter::new(err),
        }
    }

    pub fn out(&mut self, msg: &str) {
        self.out
            .write_all(msg.as_bytes())
            .expect("Failed to write to output buffer.");
    }

    pub fn out_ln(&mut self, msg: &str) {
        self.out(msg);
        self.out("\n");
    }

    pub fn err(&mut self, msg: &str) {
        self.err
            .write_all(msg.as_bytes())
            .expect("Failed to write to error buffer.");
    }

    pub fn err_ln(&mut self, msg: &str) {
        self.err(msg);
        self.err("\n");
    }

    pub fn out_buffer(&self) -> &str {
        core::str::from_utf8(self.out.buffer()).expect("Failed to create string from bytes.")
    }

    pub fn err_buffer(&self) -> &str {
        core::str::from_utf8(self.err.buffer()).expect("Failed to create string from bytes.")
    }

    pub fn error(&mut self, line: u32, message: &str) {
        self.report(line, "", message);
    }

    pub fn report(&mut self, line: u32, item: &str, message: &str) {
        let _ = self
            .err
            .write(format!("[line {line}] Error {item}: {message}\n").as_bytes());
    }

    pub fn flush(&mut self) {
        self.out.flush().expect("Failed to flush output buffer.");
        self.err.flush().expect("Failed to flush error buffer.");
    }
}

impl Drop for DiagnosticEmitter {
    fn drop(&mut self) {
        self.flush();
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Vec2 {
    pub x: i64,
    pub y: i64,
}

impl Vec2 {
    pub fn len(&self) -> f64 {
        f64::sqrt((self.x * self.x + self.y * self.y) as f64)
    }
}

impl Display for Vec2 {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{{ x: {}, y: {} }}", self.x, self.y)
    }
}

impl core::ops::Add<Vec2> for Vec2 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Vec2 {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl core::ops::AddAssign<Vec2> for Vec2 {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl core::ops::Sub<Vec2> for Vec2 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        Vec2 {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl core::ops::SubAssign<Vec2> for Vec2 {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

pub type Polygon = Vec<Vec2>;
