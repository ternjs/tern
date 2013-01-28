var editor;

CodeMirror.on(window, "load", function() {
  editor = CodeMirror.fromTextArea(document.getElementById("code"), {
    lineNumbers: true,
    extraKeys: {"Ctrl-Space": analyze}
  });
});

function load(file) {
  var xhr = new XMLHttpRequest();
  xhr.open("get", file, false);
  xhr.send();
  return xhr.responseText;
}
var ecma5 = JSON.parse(load("../ecma5.json")), browser = JSON.parse(load("../browser.json"));

function analyze() {
  editor.focus();
  var out = document.getElementById("out");
  var cx = new tern.Context([ecma5, browser]);
  tern.withContext(cx, function() {
    var data = tern.analyze(editor.getValue());
    var end = editor.indexFromPos(editor.getCursor()), start = null;
    if (editor.somethingSelected()) start = editor.indexFromPos(editor.getCursor("anchor"));
    var tp = tern.expressionType(data.ast, start, end);
    out.innerHTML = tp ? tp.toString(2) : "not found";
  });
}
