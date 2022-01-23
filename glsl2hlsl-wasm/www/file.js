// Text write
var textFile = null,
makeTextFile = function (text) {
    var data = new Blob([text], {type: 'text/plain'});

    if (textFile !== null) {
        window.URL.revokeObjectURL(textFile);
    }

    textFile = window.URL.createObjectURL(data);

    return textFile;
};

// Set up downloading
export function download_file(name, contents) {
    const a = document.createElement("a");
    a.style.display = "none";
    a.href = makeTextFile(contents);
    a.download = name;
    document.body.appendChild(a);
    a.click();
}

export function download_image(name, contents) {
    
    const c = document.createElement("br");
    document.querySelector("#links").appendChild(c);
    const a = document.createElement("a");
    a.innerHTML = name;
    a.href = contents;
    a.download = name;
    document.querySelector("#links").appendChild(a);
    
    //document.body.appendChild(a);
    // a.click();
}

export function reset() {
    document.querySelector("#links").innerHTML="<p></p><h2>Textures (Ctrl+Click and Save-As):</h2><br>";
}