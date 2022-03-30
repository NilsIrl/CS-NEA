use prog::{ParseSettings, Program};
use std::{
    io::{BufReader, Read, Write},
    str,
};
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen(module = "/js/utils.js")]
extern "C" {
    #[wasm_bindgen]
    fn print(data: &[u8]);

    #[wasm_bindgen]
    fn close();
}

#[derive(Debug)]
struct WorkerOutput;

impl Write for WorkerOutput {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        print(buf);
        Ok(buf.len())
    }

    /// Does nothing, the writer doesn't have a buffer, so nothing can be flushed or not
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

#[derive(Debug)]
struct WorkerInput;

impl Read for WorkerInput {
    fn read(&mut self, _buf: &mut [u8]) -> std::io::Result<usize> {
        todo!("input on worker")
    }
}

#[wasm_bindgen]
pub fn init() {
    std::panic::set_hook(Box::new(|info| {
        writeln!(WorkerOutput, "\x1b[31m{}\x1b[39m", info.to_string()).unwrap();
        close();
    }));
}

#[wasm_bindgen]
pub fn run(
    source: &str,
    case_sensitive: bool,
    reject_single_quote_as_quote: bool,
    for_next_not_enforced: bool,
) {
    let ast = Program::from_str(
        source,
        &ParseSettings {
            case_sensitive,
            for_next_not_enforced,
            reject_single_quote_as_quote,
        },
    )
    .unwrap();
    ast.interpret_with_io(WorkerOutput, BufReader::new(WorkerInput));
    close();
}

#[wasm_bindgen]
pub fn ast(
    source: &str,
    case_sensitive: bool,
    reject_single_quote_as_quote: bool,
    for_next_not_enforced: bool,
) {
    let ast = Program::from_str(
        source,
        &ParseSettings {
            case_sensitive,
            for_next_not_enforced,
            reject_single_quote_as_quote,
        },
    )
    .unwrap();
    writeln!(WorkerOutput, "{:#?}", ast).unwrap();
    close();
}
