use core::fmt::Display;
use std::io::BufWriter;
use std::io::Cursor;
use std::io::Write;

enum LogOrWrite {
    Log(Cursor<Vec<u8>>),
    Write(BufWriter<Box<dyn Write>>),
}

impl Write for LogOrWrite {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            LogOrWrite::Log(inner) => inner.write(buf),
            LogOrWrite::Write(inner) => inner.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            LogOrWrite::Log(_) => Ok(()),
            LogOrWrite::Write(inner) => inner.flush(),
        }
    }

    fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
        match self {
            LogOrWrite::Log(inner) => inner.write_all(buf),
            LogOrWrite::Write(inner) => inner.write_all(buf),
        }
    }

    fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> std::io::Result<()> {
        match self {
            LogOrWrite::Log(inner) => inner.write_fmt(fmt),
            LogOrWrite::Write(inner) => inner.write_fmt(fmt),
        }
    }

    fn write_vectored(&mut self, bufs: &[std::io::IoSlice<'_>]) -> std::io::Result<usize> {
        match self {
            LogOrWrite::Log(inner) => inner.write_vectored(bufs),
            LogOrWrite::Write(inner) => inner.write_vectored(bufs),
        }
    }
}

pub struct DiagnosticEmitter {
    out: LogOrWrite,
    err: LogOrWrite,
}

impl DiagnosticEmitter {
    pub fn new(out: Box<dyn Write>, err: Box<dyn Write>) -> Self {
        Self {
            out: LogOrWrite::Write(BufWriter::new(out)),
            err: LogOrWrite::Write(BufWriter::new(err)),
        }
    }

    pub fn log_to_buffer() -> Self {
        Self {
            out: LogOrWrite::Log(Cursor::new(Vec::new())),
            err: LogOrWrite::Log(Cursor::new(Vec::new())),
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

    pub fn out_buffer(&self) -> Option<String> {
        if let LogOrWrite::Log(inner) = &self.out {
            return Some(
                core::str::from_utf8(inner.get_ref())
                    .expect("Failed to convert bytes to utf-8 string")
                    .to_owned(),
            );
        }
        None
    }

    pub fn err_buffer(&self) -> Option<String> {
        if let LogOrWrite::Log(inner) = &self.err {
            return Some(
                core::str::from_utf8(inner.get_ref())
                    .expect("Failed to convert bytes to utf-8 string")
                    .to_owned(),
            );
        }
        None
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
