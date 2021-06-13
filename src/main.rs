use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::iter;

use glsl::parser::Parse as _;
use glsl::syntax::*;

mod lib;
use lib::*;

// ---- Setup ----
fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: glsl2hlsl <filename>");
        return;
    }

    let path = std::path::Path::new(args[1].as_str());
    let glsl = std::fs::read_to_string(path).expect("Error reading file");

    // Preprocessor step
    let (glsl, defs) = process_macros(glsl);

    let stage = ShaderStage::parse(glsl);
    match &stage {
        Err(a) => println!("Err: {}", a.info),
        _ => {
            let mut s = String::new();
            show_translation_unit(&mut s, &stage.unwrap());
            s = replace_macros(s, defs);

            let mut arg = args[1].clone();
            arg.push_str(".shader");
            let path = std::path::Path::new(arg.as_str());
            std::fs::write(path, s).expect("Error writing file")
        }
    };
}
