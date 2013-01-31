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
    var expr = tern.findExpression(data.ast, start, end);
    if (!expr) return;
    var tp = tern.expressionType(expr);
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
      var found = tern.findExpression(data.ast, null, index);
      var tp = found && tern.expressionType(found);
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
  var cx = new tern.Context([ecma5, browser]);
  tern.withContext(cx, function() {
    var data = tern.analyze(cm.getValue());
    var expr = tern.findExpression(data.ast, null, cm.indexFromPos(cm.getCursor())), def;
    if (!expr) return;
    if (expr.node.type == "Identifier") {
      var found = expr.state.lookup(expr.node.name);
      def = found && found.name;
    }
    if (!def) {
      var type = tern.expressionType(expr);
      if (type.types) for (var i = 0; i < type.types.length && !def; ++i) def = type.types[i].origin;
      else def = type.origin;
    }
    if (def) cm.setSelection(cm.posFromIndex(def.end), cm.posFromIndex(def.start));
  });
}