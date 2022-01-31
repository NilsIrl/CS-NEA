use prog::{ParseSettings, Program};
use std::{io, str};
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen]
    fn postMessage(data: &[u8]);
}

struct WorkerOutput;

impl io::Write for WorkerOutput {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        postMessage(buf);
        Ok(buf.len())
    }

    /// Does nothing, the writer doesn't have a buffer, so nothing can be flushed or not
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

#[wasm_bindgen]
pub fn run(source: &str) {
    console_error_panic_hook::set_once();

    let ast = Program::from_str(source, &ParseSettings::default()).unwrap();
    ast.interpret_with_write(WorkerOutput);
}
