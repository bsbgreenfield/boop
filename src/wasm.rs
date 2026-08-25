use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn init() {
    console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub fn compile(source: &str) -> Result<String, JsValue> {
    let owned = source.to_string();
    Ok(crate::capture::capture(|| {
        let mut vm = crate::vm::Vm::new(&owned);
        let _ = vm.run();
    }))
}

#[wasm_bindgen]
pub fn take_output() -> String {
    crate::capture::drain()
}
