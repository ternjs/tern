var editor;

CodeMirror.on(window, "load", function() {
  editor = CodeMirror.fromTextArea(document.getElementById("code"), {
    lineNumbers: true,
    extraKeys: {"Ctrl-Space": function(cm) { CodeMirror.showHint(cm, ternHints, {async: true}); }},
    autofocus: true
  });
//  editor.on("cursorActivity", updateArgumentHints);
});

function load(file) {
  var xhr = new XMLHttpRequest();
  xhr.open("get", file, false);
  xhr.send();
  return xhr.responseText;
}

var Tern = new tern.Tern({
  getFile: function(name, c) {
    if (name != "local") throw new Error("Why are you trying to fetch " + name + "?");
    c(null, editor.getValue());
  }
});
Tern.addEnvironment(JSON.parse(load("../ecma5.json")));
Tern.addEnvironment(JSON.parse(load("../browser.json")));
Tern.addFile("local");

/*function findType(cm) {
  var out = document.getElementById("out");
  var cx = new tern.Context(environment);
  tern.withContext(cx, function() {
    var data = tern.analyze(cm.getValue());
    var end = cm.indexFromPos(cm.getCursor()), start = null;
    if (cm.somethingSelected()) start = cm.indexFromPos(cm.getCursor("anchor"));
    var expr = tern.findExpression(data.ast, start, end);
    if (!expr) return;
    var tp = tern.expressionType(expr);
    window.tp = tp; window.cx = cx; window.scope = expr.state;
    out.innerHTML = tp ? tern.toString(tp.getType()) : "not found";
  });
}*/

function ternHints(cm, c) {
  var query = {query: {type: "completions",
                       position: cm.indexFromPos(cm.getCursor()),
                       file: "local"}};
  if (!cm.isClean()) {
    query.files = [{type: "full",
                    name: "local",
                    text: cm.getValue()}];
    cm.markClean();
  }
  Tern.request(query, function(error, data) {
    if (error) throw new Error(error);
    else c({from: cm.posFromIndex(data.from),
            to: cm.posFromIndex(data.to),
            list: data.completions});
  });
}

/*
function jumpToDef(cm) {
  var cx = new tern.Context(environment);
  tern.withContext(cx, function() {
    var data = tern.analyze(cm.getValue());
    var expr = tern.findExpression(data.ast, null, cm.indexFromPos(cm.getCursor())), def;
    if (!expr) return;
    if (expr.node.type == "Identifier") {
      var found = expr.state.findVar(expr.node.name);
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

    var cx = new tern.Context(environment);
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

function condense(cm) {
  var cx = new tern.Context(environment);
  tern.withContext(cx, function() {
    var data = tern.analyze(cm.getValue(), "local");
    console.log(JSON.stringify(tern.condense("local"), null, 2));
  });
}
*/