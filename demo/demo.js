var editor;

CodeMirror.on(window, "load", function() {
  editor = CodeMirror.fromTextArea(document.getElementById("code"), {
    lineNumbers: true,
    extraKeys: {"Ctrl-I": findType,
                "Ctrl-Space": function(cm) { CodeMirror.showHint(cm, complete); },
                "Alt-.": jumpToDef}
  });
});

function load(file) {
  var xhr = new XMLHttpRequest();
  xhr.open("get", file, false);
  xhr.send();
  return xhr.responseText;
}
var ecma5 = JSON.parse(load("../ecma5.json")), browser = JSON.parse(load("../browser.json"));

function findType(cm) {
  var out = document.getElementById("out");
  var cx = new tern.Context([ecma5, browser]);
  tern.withContext(cx, function() {
    var data = tern.analyze(cm.getValue());
    var end = cm.indexFromPos(cm.getCursor()), start = null;
    if (cm.somethingSelected()) start = cm.indexFromPos(cm.getCursor("anchor"));
    var tp = tern.expressionType(data.ast, start, end);
    out.innerHTML = tp ? tp.toString(2) : "not found";
  });
}

function complete(cm) {
  var cx = new tern.Context([ecma5, browser]);
  var cur = cm.getCursor(), token = cm.getTokenAt(cur);
  var isProp = false, name, pos = cur, start = cur, end = cur;
  if (token.string == ".") {
    isProp = true;
    name = "";
    pos = {line: cur.line, ch: cur.ch - 1};
  } else if (cm.getLine(cur.line).charAt(token.start - 1) == ".") {
    isProp = true; name = token.string;
    pos = {line: cur.line, ch: token.start - 1};
    start = {line: cur.line, ch: token.start};
    end = {line: cur.line, ch: token.end};
  } else if (/^[\w$]+$/.test(token.string)) {
    name = token.string;
    start = {line: cur.line, ch: token.start};
    end = {line: cur.line, ch: token.end};
  } else {
    name = "";
  }
  var index = cm.indexFromPos(pos);

  return tern.withContext(cx, function() {
    var data = tern.analyze(cm.getValue());
    if (isProp) {
      var tp = tern.expressionType(data.ast, null, index);
      if (tp) return {
        list: tern.propertiesOf(tp, name),
        from: start, to: end
      };
    }
    return {
      list: tern.localsAt(data.ast, index, name),
      from: start, to: end
    };
  });
}

function jumpToDef(cm) {
  // FIXME
}