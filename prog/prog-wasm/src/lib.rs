use prog::{ParseSettings, Program};
use std::{io, str};
use wasm_bindgen::{prelude::wasm_bindgen, JsCast};
use web_sys::{window, HtmlOutputElement};

#[wasm_bindgen]
extern "C" {
    fn append_to_output(s: &str);
    fn clear_output();
}

struct WebsiteOutput<'a> {
    output_element: &'a HtmlOutputElement,
}

impl io::Write for WebsiteOutput<'_> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let string = match str::from_utf8(buf) {
            Ok(string) => string,
            Err(_) => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Failed to convert output to UTF8",
                ))
            }
        };
        self.output_element.append_with_str_1(string).unwrap();
        Ok(string.len())
    }

    /// Does nothing, the writer doesn't have a buffer, so nothing can be flushed or not
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

#[wasm_bindgen]
pub fn run(source: &str) {
    let ast = Program::from_str(source, &ParseSettings::default()).unwrap();
    let window = window().unwrap();
    let document = window.document().unwrap();
    let element = document.get_element_by_id("output").unwrap();
    let output = element.dyn_ref::<HtmlOutputElement>().unwrap();
    output.set_value("");
    ast.interpret_with_write(WebsiteOutput {
        output_element: output,
    });
}
