mod utils;

use glsl2hlsl::{extract_image_pass_code, make_shader};
use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen]
pub fn transpile(input: String) -> String {
    glsl2hlsl::transpile(input, false, false)
}

// Takes a Shadertoy API JSON response and returns the converted HLSL of
// the first Image-pass renderpass. Returns empty string on parse failure.
#[wasm_bindgen]
pub fn transpile_shadertoy_json(json: String) -> String {
    match make_shader(&json) {
        Ok(shader) => match extract_image_pass_code(&shader) {
            Some(code) => glsl2hlsl::transpile(code, false, false),
            None => String::new(),
        },
        Err(e) => format!("// Failed to parse Shadertoy JSON: {}", e),
    }
}
