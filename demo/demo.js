var editor;

var Pos = CodeMirror.Pos;

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

  editor.on("cursorActivity", updateArgumentHints);
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
  var minIndent = null, minLine = null, endLine, tabSize = cm.getOption("tabSize");
  for (var p = start.line - 1, min = Math.max(0, p - 50); p >= min; --p) {
    var line = cm.getLine(p), fn = line.search(/\bfunction\b/);
    if (fn < 0) continue;
    var indent = CodeMirror.countColumn(line, null, tabSize);
    if (minIndent != null && minIndent <= indent) continue;
    if (cm.getTokenAt(Pos(p, fn + 1)).type != "keyword") continue;
    minIndent = indent;
    minLine = p;
  }
  if (minLine == null) minLine = min;
  var max = Math.min(cm.lastLine(), start.line + 20);
  if (minIndent == null || minIndent == CodeMirror.countColumn(cm.getLine(start.line), null, tabSize))
    endLine = max;
  else for (endLine = start.line + 1; endLine < max; ++endLine) {
    var indent = CodeMirror.countColumn(cm.getLine(endLine), null, tabSize);
    if (indent <= minIndent) break;
  }
  var from = Pos(minLine, 0);

  return {type: "part",
          name: "local",
          offset: cm.indexFromPos(from),
          text: cm.getRange(from, Pos(endLine, 0))};
}

function buildRequest(cm, how, query) {
  var files, offset = 0, startPos, endPos;
  if (typeof query == "string") query = {type: query};
  if (how == "range") {
    query.end = cm.indexFromPos(endPos = cm.getCursor("end"));
    if (cm.somethingSelected())
      query.start = cm.indexFromPos(startPos = cm.getCursor("start"));
  } else if (how == "pos") {
    query.position = cm.indexFromPos(endPos = cm.getCursor());
  } else if (how == "from") {
    if (query.position != null) {
      query.position = cm.indexFromPos(endPos = query.position);
    } else {
      query.end = cm.indexFromPos(endPos = query.end);
      if (query.start) query.start = cm.indexFromPos(startPos = query.start);
    }
  }
  if (!startPos) startPos = endPos;

  if (!cm.isClean()) {
    if (cm.lineCount() > 100) {
      files = [getFragmentAround(cm, startPos, endPos)];
      query.file = "#0";
      offset = files[0].offset;
      if (query.position != null) {
        query.position -= offset;
      } else {
        if (query.start != null) query.start -= offset;
        query.end -= offset;
      }
    } else {
      files = [{type: "full",
                name: "local",
                text: cm.getValue()}];
      query.file = "local";
      cm.markClean();
    }
  } else {
    query.file = "local";
  }
  return {request: {query: query, files: files},
          offset: offset};
}

function findType(cm) {
  Tern.request(buildRequest(cm, "range", "type").request, function(error, data) {
    if (error) throw new Error(error);
    var out = document.getElementById("out");
    out.innerHTML = "";
    out.appendChild(document.createTextNode(data.type || "not found"));
  });
}

function ternHints(cm, c) {
  var req = buildRequest(cm, "pos", "completions");

  Tern.request(req.request, function(error, data) {
    if (error) throw new Error(error);
    c({from: cm.posFromIndex(data.from + req.offset),
       to: cm.posFromIndex(data.to + req.offset),
       list: data.completions});
  });
}

function elt(tagname, text, cls) {
  var e = document.createElement(tagname);
  if (text) e.appendChild(document.createTextNode(text));
  if (cls) e.className = cls;
  return e;
}

function parseFnType(text) {
  var args = [], pos = 3;

  function skipMatching(upto) {
    var depth = 0, start = pos;
    for (;;) {
      var next = text.charAt(pos);
      if (upto.test(next) && !depth) return text.slice(start, pos);
      if (/[{\[\(]/.test(next)) ++depth;
      else if (/[}\]\)]/.test(next)) --depth;
      ++pos;
    }
  }

  // Parse arguments
  for (;;) {
    var name = text.slice(pos).match(/^(\w+): /);
    if (name) {
      pos += name[0].length;
      name = name[1];
    }
    args.push({name: name, type: skipMatching(/[\),]/)});
    if (text.charAt(pos) == ")") break;
    pos += 2;
  }

  var rettype = text.slice(pos).match(/^\) -> (.*)$/);
  
  return {args: args, rettype: rettype && rettype[1]};
}

var cachedFunction = {line: null, ch: null, name: null, type: null, bad: null};

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

    var query = {type: "type", preferFunction: true, end: Pos(line, ch)}
    Tern.request(buildRequest(cm, "from", query).request, function(error, data) {
      if (error) throw new Error(error);
      if (!data.type || !/^fn\(/.test(data.type)) return;
    
      cache.type = parseFnType(data.type);
      cache.name = data.exprName || data.name || "fn";
      cache.bad = false;
      showArgumentHints(cache, out, pos);
    });
  } else if (!cache.bad) {
    showArgumentHints(cache, out, pos);
  }
}

function showArgumentHints(cache, out, pos) {
  out.appendChild(elt("span", cache.name, "Tern-fname"));
  out.appendChild(document.createTextNode("("));

  var tp = cache.type;
  for (var i = 0; i < tp.args.length; ++i) {
    if (i) out.appendChild(document.createTextNode(", "));
    var arg = tp.args[i];
    out.appendChild(elt("span", arg.name || "?", "Tern-farg" + (i == pos ? " Tern-farg-current" : "")));
    if (arg.type != "?") {
      out.appendChild(document.createTextNode(": "));
      out.appendChild(elt("span", arg.type, "Tern-type"));
    }
  }
  out.appendChild(document.createTextNode(tp.rettype ? ") -> " : ")"));
  if (tp.rettype) out.appendChild(elt("span", tp.rettype, "Tern-type"));
}
