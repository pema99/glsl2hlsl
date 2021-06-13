import * as wasm from "glsl2hlsl-wasm";

let inp = document.getElementById("in");
let outp = document.getElementById("out");

document.getElementById("convert").addEventListener("click", function (e) { 
    outp.value = wasm.transpile(inp.value);
});
