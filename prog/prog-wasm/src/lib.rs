use prog::{ParseSettings, Program};
use std::{
    io::{BufReader, Read, Write},
    str,
    time::Duration,
};
use wasm_bindgen::{prelude::wasm_bindgen, JsValue};
use wasm_rs_shared_channel::spsc;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen]
    fn postMessage(data: &str);
}

fn print(data: &[u8]) {
    postMessage(str::from_utf8(data).unwrap());
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

struct WorkerInput(spsc::Receiver<char>);

impl WorkerInput {
    fn new(channel: JsValue) -> Self {
        let (_, receiver) = spsc::SharedChannel::from(channel).split();
        Self(receiver)
    }
}

impl Read for WorkerInput {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let mut i = 0;
        while buf.len() - i >= 4 {
            match self.0.recv(Some(Duration::from_secs(9999))) {
                Ok(Some(v)) => {
                    i += v.encode_utf8(&mut buf[i..]).len();
                    if v == '\n' {
                        return Ok(i);
                    }
                }
                Ok(None) => return Ok(i),
                Err(err) => panic!("{:?}", err),
            }
        }
        Ok(i)
    }
}

#[wasm_bindgen]
pub fn init() {
    std::panic::set_hook(Box::new(|info| {
        writeln!(WorkerOutput, "\x1b[31m{}\x1b[39m", info.to_string()).unwrap();
    }));
}

#[wasm_bindgen]
pub fn run(
    source: &str,
    channel: JsValue,
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
    ast.interpret_with_io(WorkerOutput, BufReader::new(WorkerInput::new(channel)));
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
}
