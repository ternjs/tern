var server, editor, environment = [];
var Pos = CodeMirror.Pos;
var docs = [], curDoc;

function load(file, c) {
  var xhr = new XMLHttpRequest();
  xhr.open("get", file, true);
  xhr.send();
  xhr.onreadystatechange = function() {
    if (xhr.readyState == 4) c(xhr.responseText, xhr.status);
  };
}

CodeMirror.on(window, "load", function() {
  var files = ["ecma5.json", "browser.json", "plugin/requirejs/requirejs.json", "jquery.json"];
  var loaded = 0;
  for (var i = 0; i < files.length; ++i) (function(i) {
    load(files[i], function(json) {
      environment[i] = JSON.parse(json);
      if (++loaded == files.length) initEditor();
    });
  })(i);
});

function initEditor() {
  editor = CodeMirror.fromTextArea(document.getElementById("code"), {
    lineNumbers: true,
    extraKeys: {
      "Ctrl-I": findType,
      "Ctrl-Space": function(cm) { CodeMirror.showHint(cm, ternHints, {async: true}); },
      "Alt-.": jumpToDef,
      "Alt-,": jumpBack
    },
    autofocus: true,
    matchBrackets: true
  });
  server = new tern.Server({getFile: getFile}, environment);
  registerDoc("test.js", editor.getDoc());
  editor.on("cursorActivity", updateArgumentHints);

  registerDoc("test_dep.js", new CodeMirror.Doc(document.getElementById("requirejs_test_dep").firstChild.nodeValue, "javascript"));
  load("demo/underscore.js", function(body) {
    registerDoc("underscore.js", new CodeMirror.Doc(body, "javascript"));
  });

  CodeMirror.on(document.getElementById("docs"), "click", function(e) {
    var target = e.target || e.srcElement;
    if (target.nodeName.toLowerCase() != "li") return;
    for (var i = 0, c = target.parentNode.firstChild; ; ++i, (c = c.nextSibling))
      if (c == target) return selectDoc(i);
  });
}

var httpCache = {};
function getFile(name, c) {
  if (/^https?:\/\//.test(name)) {
    if (httpCache[name]) return c(null, httpCache[name]);
    load(name, function(body, status) {
      if (status >= 400) body = "";
      httpCache[name] = body;
      c(null, body);
    });
  } else {
    for (var i = 0; i < docs.length; ++i) {
      var doc = docs[i];
      if (doc.name == name) return c(null, doc.doc.getValue());
    }
    return c(null, "");
  }
}

function registerDoc(name, doc) {
  docs.push({name: name, doc: doc});
  var docTabs = document.getElementById("docs");
  var li = docTabs.appendChild(document.createElement("li"));
  li.appendChild(document.createTextNode(name));
  if (editor.getDoc() == doc) {
    setSelectedDoc(docs.length - 1);
    curDoc = docs[docs.length - 1];
  }
  server.addFile(name);
}

function setSelectedDoc(pos) {
  var docTabs = document.getElementById("docs");
  for (var i = 0; i < docTabs.childNodes.length; ++i)
    docTabs.childNodes[i].className = pos == i ? "selected" : "";
}

function selectDoc(pos) {
  setSelectedDoc(pos);
  curDoc = docs[pos];
  editor.swapDoc(curDoc.doc);
}

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
          name: curDoc.name,
          offset: cm.indexFromPos(from),
          text: cm.getRange(from, Pos(endLine, 0))};
}

function displayError(cm, err) {
  var out = document.getElementById("out");
  out.innerHTML = "";
  out.appendChild(document.createTextNode(err.message || err.toString()));
}

function buildRequest(cm, query, allowFragments) {
  var files, offset = 0, startPos, endPos;
  if (typeof query == "string") query = {type: query};
  if (query.end == null && query.start == null) {
    query.end = cm.indexFromPos(endPos = cm.getCursor("end"));
    if (cm.somethingSelected())
      query.start = cm.indexFromPos(startPos = cm.getCursor("start"));
  } else {
    query.end = cm.indexFromPos(endPos = query.end);
    if (query.start != null)
      query.start = cm.indexFromPos(startPos = query.start);
  }
  if (!startPos) startPos = endPos;

  if (!cm.isClean()) {
    if (cm.lineCount() > 100 && allowFragments !== false) {
      files = [getFragmentAround(cm, startPos, endPos)];
      query.file = "#0";
      offset = files[0].offset;
      if (query.start != null) query.start -= offset;
      query.end -= offset;
    } else {
      files = [{type: "full",
                name: curDoc.name,
                text: cm.getValue()}];
      query.file = curDoc.name;
      cm.markClean();
    }
  } else {
    query.file = curDoc.name;
  }
  return {request: {query: query, files: files},
          offset: offset};
}

function findType(cm) {
  server.request(buildRequest(cm, "type").request, function(error, data) {
    if (error) return displayError(cm, error);
    var out = document.getElementById("out");
    out.innerHTML = "";
    out.appendChild(document.createTextNode(data.type || "not found"));
  });
}

function ternHints(cm, c) {
  var req = buildRequest(cm, "completions");

  server.request(req.request, function(error, data) {
    if (error) return displayError(cm, error);
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
  if (text.charAt(pos) != ")") for (;;) {
    var name = text.slice(pos).match(/^([\w?$]+): /);
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
    server.request(buildRequest(cm, query).request, function(error, data) {
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

var jumpStack = [];

function jumpToDef(cm) {
  server.request(buildRequest(cm, "definition", false).request, function(error, data) {
    if (error) return displayError(cm, error);
    jumpStack.push({file: curDoc.name,
                    start: cm.getCursor("from"),
                    end: cm.getCursor("to")});
    if (data.file != curDoc.name) {
      for (var i = 0; i < docs.length; ++i)
        if (docs[i].name == data.file) { selectDoc(i); break; }
      if (i == docs.length) return displayError("Definition is not in a local buffer");
    }
    setTimeout(function() {
      cm.setSelection(cm.posFromIndex(data.start), cm.posFromIndex(data.end));
    }, 20);
  });
}

function jumpBack(cm) {
  var pos = jumpStack.pop();
  if (!pos) return;
  if (pos.file != curDoc.name) {
    for (var i = 0; i < docs.length; ++i)
      if (docs[i].name == pos.file) { selectDoc(i); break; }
    if (i == docs.length) return;
  }
  setTimeout(function() {
    cm.setSelection(pos.start, pos.end);
  }, 20);
}
