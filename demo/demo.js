var editor;

CodeMirror.on(window, "load", function() {
  editor = CodeMirror.fromTextArea(document.getElementById("code"), {
    lineNumbers: true,
    extraKeys: {"Ctrl-I": findType,
                "Ctrl-Space": function(cm) { CodeMirror.showHint(cm, complete); },
                "Alt-.": jumpToDef}
  });
  editor.on("cursorActivity", updateArgumentHints);
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
    out.innerHTML = tp ? tern.toString(tp.getType()) : "not found";
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
      if (type.types) for (var i = 0; i < type.types.length && !def; ++i) def = type.types[i].originNode;
      else def = type.originNode;
    }
    if (def) cm.setSelection(cm.posFromIndex(def.end), cm.posFromIndex(def.start));
  });
}

function elt(tagname, text, cls) {
  var e = document.createElement(tagname);
  if (text) e.appendChild(document.createTextNode(text));
  if (cls) e.className = cls;
  return e;
}

var cachedFunction = {line: null, ch: null, name: null, type: null, bad: null, cx: null};

function updateArgumentHints(cm) {
  var out = document.getElementById("out");
  out.innerHTML = "";
  if (cm.somethingSelected()) return;

  var lex = cm.getTokenAt(cm.getCursor()).state.lexical;
  if (lex.info != "call") return;
  var ch = lex.column, pos = lex.pos || 0;
  for (var line = cm.getCursor().line, e = Math.max(0, line - 9), found = false; line >= e; --line)
    if (cm.getLine(line).charAt(ch) == "(") {found = true; break;}
  if (!found) return;

  var cache = cachedFunction;
  if (cache.line != line || cache.ch != ch) {
    cache.line = line; cache.ch = ch; cache.bad = true;

    var cx = new tern.Context([ecma5, browser]);
    if (tern.withContext(cx, function() {
      var data = tern.analyze(cm.getValue());
      var callee = tern.findExpression(data.ast, null, cm.indexFromPos({line: line, ch: ch}));
      if (!callee) return true;
      var tp = tern.expressionType(callee).getFunctionType(), name;
      if (!tp) return true;

      if (callee.node.type == "Identifier")
        cache.name = callee.node.name;
      else if (callee.node.type == "MemberExpression" && !callee.node.computed)
        cache.name = callee.node.property.name;
      else
        cache.name = tp.name || "fn";

      cache.type = tp;
      cache.cx = cx;
      cache.bad = false;
    })) return;
  }

  if (cache.bad) return;

  tern.withContext(cache.cx, function() {
    var tp = cache.type;

    out.appendChild(elt("span", cache.name, "Tern-fname"));
    out.appendChild(document.createTextNode("("));
    for (var i = 0; i < tp.argNames.length; ++i) {
      if (i) out.appendChild(document.createTextNode(", "));
      var argname = tp.argNames[i];
      if (typeof argname == "object") argname = argname.name;
      out.appendChild(elt("span", argname, "Tern-farg" + (i == pos ? " Tern-farg-current" : "")));
      var argtype = tp.args[i].getType();
      if (argtype) {
        out.appendChild(document.createTextNode(": "));
        out.appendChild(elt("span", argtype.toString(), "Tern-type"));
      }
    }
    var rettype = !tp.retval.isEmpty() && tern.toString(tp.retval.getType());
    out.appendChild(document.createTextNode(rettype ? ") -> " : ")"));
    if (rettype) out.appendChild(elt("span", rettype, "Tern-type"));
  });
}
