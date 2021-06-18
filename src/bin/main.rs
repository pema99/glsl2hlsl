extern crate glsl2hlsl;
use glsl2hlsl::*;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        println!("Usage: glsl2hlsl <filename>");
        return;
    }

    let path = std::path::Path::new(args[1].as_str());
    let glsl = std::fs::read_to_string(path).expect("Error reading file");

    let compiled = transpile(glsl, true, true);

    let mut arg = args[1].clone();
    arg.push_str(".shader");
    let path = std::path::Path::new(arg.as_str());
    std::fs::write(path, compiled).expect("Error writing file");
}
