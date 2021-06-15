import * as wasm from "glsl2hlsl-wasm";

let inp = document.getElementById("in");
let outp = document.getElementById("out");

let shader = document.getElementById("shader");

document.getElementById("convert").addEventListener("click", function (e) { 
    outp.value = wasm.transpile(inp.value);
});

document.getElementById("download").addEventListener("click", function (e) {

    var arr = shader.value.split("/").filter(x => x.length > 0);

    const xhttp = new XMLHttpRequest();
    xhttp.onload = function() {
        wasm.download(this.responseText);
    }
    
    xhttp.open("GET", "https://www.shadertoy.com/api/v1/shaders/" + arr[arr.length-1] + "?key=NtHtMm");
    xhttp.send();
});