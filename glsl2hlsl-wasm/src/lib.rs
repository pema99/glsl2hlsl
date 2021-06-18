mod utils;

use glsl2hlsl::{get_files, make_shader};
use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen(module = "/www/file.js")]
extern "C" {
    fn download_file(name: &str, contents: &str);
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen]
pub fn transpile(input: String, extract_props: bool, raymarch: bool) -> String {
    glsl2hlsl::transpile(input, extract_props, raymarch)
}

#[wasm_bindgen]
pub fn download(json: String, extract_props: bool, raymarch: bool) {
    let shader = make_shader(&json).unwrap();
    let files = get_files(&shader, extract_props, raymarch);
    for f in files.iter() {
        unsafe {
            download_file(&f.name, &f.contents);
        }
    }
}
