use wasm_bindgen::prelude::{wasm_bindgen, JsValue};
use wasm_rs_shared_channel::spsc;

#[wasm_bindgen]
pub struct Sender(spsc::Sender<char>);

#[wasm_bindgen]
impl Sender {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        let (sender, _) = spsc::channel::<char>(1024).split();
        Sender(sender)
    }

    #[wasm_bindgen]
    pub fn replica(&self) -> JsValue {
        // The first field of an spsc::Sender is a SharedChannel
        self.0.0.clone().into()
    }

    #[wasm_bindgen]
    pub fn send(&self, v: char) {
        self.0.send(&v).unwrap();
    }
}
