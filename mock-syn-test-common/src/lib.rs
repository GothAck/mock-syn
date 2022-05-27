use std::{
    io::Write,
    process::{Command, Stdio},
};

use syn::{Error, Result};

pub trait DebugSynResult {
    fn print_syn_error_spanned(self, input: &str) -> Self;
}

impl<T> DebugSynResult for Result<T> {
    fn print_syn_error_spanned(self, input: &str) -> Self {
        if let Err(e) = &self {
            print_err(e, input)
        }
        self
    }
}

pub fn print_err(e: &Error, input: &str) {
    let span = e.span();
    let start = span.start();
    let end = span.end();

    let mut lines = Vec::new();
    for (i, line) in input.lines().enumerate() {
        let i = i + 1;
        lines.push(line.to_string());

        if i == start.line {
            let mut err_line = line.to_string();
            err_line = err_line.drain(..).map(|_| ' ').collect();

            if i == end.line {
                let replacement: String = { start.column..end.column }.map(|_| '=').collect();
                err_line.replace_range(start.column..end.column, &replacement);
            } else {
                let replacement: String = { start.column..err_line.len() }.map(|_| '=').collect();
                err_line.replace_range(start.column.., &replacement);
            }
            lines.push(err_line);
        }
        if i > start.line && i < end.line {
            let mut err_line = line.to_string();
            // err_line = err_line.drain(..).map(|_| ' ').collect();

            err_line = { 0..err_line.len() }.map(|_| '=').collect();
            lines.push(err_line);
        }
        if i > start.line && i == end.line {
            let mut err_line = line.to_string();
            err_line = err_line.drain(..).map(|_| ' ').collect();

            let replacement: String = { 0..end.column }.map(|_| '=').collect();
            err_line.replace_range(..end.column, &replacement);
            lines.push(err_line);
        }
    }

    eprintln!("{:?} {:?} {:?} {:?}", e, span, start, end);
    eprintln!("{}", lines.join("\n"));
}

pub fn rustfmt<T: std::fmt::Display>(input: T) -> String {
    let input = input.to_string();

    let mut child = Command::new("rustfmt")
        .args(["--emit", "stdout"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    let mut stdin = child.stdin.take().unwrap();
    std::thread::spawn(move || {
        stdin.write_all(input.as_bytes()).unwrap();
    });

    let output = child.wait_with_output().unwrap();

    String::from_utf8(output.stdout).unwrap()
}
