use std::io::BufWriter;
use std::io::Write;

pub struct DiagnosticEmitter<W: std::io::Write> {
    #[allow(dead_code)]
    pub out: BufWriter<W>,
    pub err: BufWriter<W>,
}

impl<W: std::io::Write> DiagnosticEmitter<W> {
    pub fn new(out: W, err: W) -> Self {
        DiagnosticEmitter {
            out: BufWriter::new(out),
            err: BufWriter::new(err),
        }
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
