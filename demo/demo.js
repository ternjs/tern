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
  var isProp = false, name, pos = cur, end = cur;
  if (token.string == ".") {
    isProp = true;
    name = "";
    pos = {line: cur.line, ch: cur.ch - 1};
  } else if (cm.getLine(cur.line).charAt(token.start - 1)) {
    isProp = true; name = token.string;
    pos = {line: cur.line, ch: token.start - 1};
    end = {line: cur.line, ch: token.end};
  } else if (/^[\w$]+$/.test(token.string)) {
    name = token.string;
  } else {
    name = "";
  }

  return tern.withContext(cx, function() {
    var data = tern.analyze(cm.getValue());
    if (isProp) {
      var tp = tern.expressionType(data.ast, null, cm.indexFromPos(pos));
      if (tp) {
        if (tp.types) tp = tp.types[0];
        if (tp.proto) tp = tp.proto;
        if (!tp.props) throw new Error("FIXME");
        var found = [];
        for (var prop in tp.props) if ((!name || prop.slice(0, name.length) == name) && tp.props[prop].flags & 4)
          found.push(prop);
        return {list: found,
                from: {line: cur.line, ch: pos.ch + 1},
                to: end};
      }
    }
    // FIXME get local variables from tern (?)
    if (!hints) return null;
  });
}

function jumpToDef(cm) {
  // FIXME
}