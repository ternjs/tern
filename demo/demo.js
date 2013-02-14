var editor;

CodeMirror.on(window, "load", function() {
  editor = CodeMirror.fromTextArea(document.getElementById("code"), {
    lineNumbers: true,
    extraKeys: {
      "Ctrl-I": findType,
      "Ctrl-Space": function(cm) { CodeMirror.showHint(cm, ternHints, {async: true}); }
    },
    autofocus: true
  });
  editor.setValue(load("../node_modules/codemirror/lib/codemirror.js"));
  editor.setCursor(CodeMirror.Pos(5129));
  editor.replaceSelection("\n      st", "end");

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

function getFragmentAround(cm, start, end) {
  var minIndent = null, minLine = null;
  for (var p = start.line - 1, min = Math.max(0, start.line - 50); p >= min; --p) {
    var line = cm.getLine(p), fn = line.search(/\bfunction\b/);
    if (fn < 0) continue;
    var indent = CodeMirror.countColumn(line, null, cm.getOption("tabSize"));
    if (minIndent != null && minIndent <= indent) continue;
    if (cm.getTokenAt({line: p, ch: fn + 1}).type != "keyword") continue;
    minIndent = indent;
    minLine = p;
  }
  if (minLine == null) minLine = min;
  var from = CodeMirror.Pos(minLine, 0);
  return {type: "part",
          name: "local",
          offset: cm.indexFromPos(from),
          text: cm.getRange(from, CodeMirror.Pos(end.line))};
}

function buildRequest(cm, query, asRange) {
  var files, offset = 0, startPos, endPos;
  if (typeof query == "string") query = {type: query};
  if (asRange) {
    query.end = cm.indexFromPos(endPos = cm.getCursor("end"));
    if (cm.somethingSelected())
      query.start = cm.indexFromPos(startPos = cm.getCursor("start"));
    else
      startPos = endPos;
  } else {
    query.position = cm.indexFromPos(startPos = endPos = cm.getCursor());
  }
  if (!cm.isClean()) {
    if (cm.lineCount() > 100) {
      files = [getFragmentAround(cm, startPos, endPos)];
      query.file = "#0";
      offset = files[0].offset;
      if (asRange) {
        if (query.start != null) query.start -= offset;
        query.end -= offset;
      } else {
        query.position -= offset;
      }
    } else {
      files = [{type: "full",
                name: "local",
                text: cm.getValue()}];
      query.file = "local";
      cm.markClean();
    }
  }
  return {request: {query: query, files: files},
          offset: offset};
}

function findType(cm) {
  Tern.request(buildRequest(cm, "type", true).request, function(error, data) {
    if (error) throw new Error(error);
    var out = document.getElementById("out");
    out.innerHTML = "";
    out.appendChild(document.createTextNode(data.typeName || "not found"));
  });
}

function ternHints(cm, c) {
  var req = buildRequest(cm, "completions");

  Tern.request(req.request, function(error, data) {
    if (error) throw new Error(error);
    c({from: cm.posFromIndex(data.from + req.offset),
       to: cm.posFromIndex(data.to + req.offset),
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
