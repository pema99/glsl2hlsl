import * as wasm from "glsl2hlsl-wasm";

let inp = document.getElementById("in");
let outp = document.getElementById("out");

let shader = document.getElementById("shader");

let raymarch = document.getElementById("raymarch");
let extract = document.getElementById("extract");

document.getElementById("convert").addEventListener("click", function (e) { 
    outp.value = wasm.transpile(inp.value, extract.checked, raymarch.checked);
});

document.getElementById("download").addEventListener("click", function (e) {

    var arr = shader.value.split("/").filter(x => x.length > 0);

    const xhttp = new XMLHttpRequest();
    xhttp.onload = function() {
        wasm.download(this.responseText, extract.checked, raymarch.checked);
    }
    
    xhttp.open("GET", "https://www.shadertoy.com/api/v1/shaders/" + arr[arr.length-1] + "?key=NtHtMm");
    xhttp.send();
});