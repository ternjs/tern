var server, editor, defs = [];
var Pos = CodeMirror.Pos;
var docs = [], curDoc;

var bigDoc = 250, useWorker = false;

function findDoc(name) {
  for (var i = 0; i < docs.length; ++i) if (docs[i].name == name) return docs[i];
}

function load(file, c) {
  var xhr = new XMLHttpRequest();
  xhr.open("get", file, true);
  xhr.send();
  xhr.onreadystatechange = function() {
    if (xhr.readyState == 4) c(xhr.responseText, xhr.status);
  };
}

CodeMirror.on(window, "load", function() {
  if (!Object.create)
    return document.getElementById("demospace").innerHTML = "<p><strong>Sorry!</strong> You seem to be using a browser that does not support ECMAScript version 5. Tern is using some ECMAScript 5 features, so it will not work on your browser. Try with a more modern one if you really want to see this.</p><p>(I may change the library to not depend on ECMAScript 5 in the future. It would not be all that hard, but I have not yet decided whether this is worthwhile.)</p>";

  var files = ["../defs/ecma5.json", "../defs/browser.json", "../defs/jquery.json"];
  var loaded = 0;
  for (var i = 0; i < files.length; ++i) (function(i) {
    load(files[i], function(json) {
      defs[i] = JSON.parse(json);
      if (++loaded == files.length) initEditor();
    });
  })(i);

  var cmds = document.getElementById("commands");
  CodeMirror.on(cmds, "change", function() {
    if (!editor || cmds.selectedIndex == 0) return;
    var found = commands[cmds.value];
    cmds.selectedIndex = 0;
    editor.focus();
    if (found) found(editor);
  });
});

function initEditor() {
  var keyMap = {
    "Ctrl-I": findType,
    "Ctrl-Space": function(cm) { CodeMirror.showHint(cm, ternHints, {async: true}); },
    "Alt-.": jumpToDef,
    "Shift-Alt-.": function(cm) { jumpToDef(cm, true); },
    "Alt-,": jumpBack,
    "Ctrl-Q": renameVar
  };

  editor = CodeMirror.fromTextArea(document.getElementById("code"), {
    lineNumbers: true,
    extraKeys: keyMap,
    matchBrackets: true
  });

  if (useWorker) {
    server = workerServer();
  } else {
    server = new tern.Server({
      getFile: getFile,
      async: true,
      defs: defs,
      debug: true,
      plugins: {requirejs: {}}
    });
  }
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

function workerServer() {
  var worker = new Worker("demo/worker.js");
  worker.postMessage({type: "defs", data: defs});
  var msgId = 0, pending = {};

  function send(data, c) {
    if (c) {
      data.id = ++msgId;
      pending[msgId] = c;
    }
    worker.postMessage(data);
  }
  worker.onmessage = function(e) {
    var data = e.data;
    if (data.type == "getFile") {
      getFile(data.name, function(err, text) {
        send({type: "getFile", err: String(err), text: text, id: data.id});
      });
    } else if (data.type == "debug") {
      console.log(data.message);
    } else if (data.id && pending[data.id]) {
      pending[data.id](data.err, data.body);
      delete pending[data.id];
    }
  };
  worker.onerror = function(e) {
    for (var id in pending) pending[id](e);
    pending = {};
  };

  return {
    worker: worker,
    addFile: function(name, text) { send({type: "add", name: name, text: text}); },
    delFile: function(name) { send({type: "del", name: name}); },
    request: function(body, c) { send({type: "req", body: body}, c); }
  };
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
    var doc = findDoc(name);
    return c(null, doc ? doc.doc.getValue() : "");
  }
}

function registerDoc(name, doc) {
  var data = {name: name, doc: doc, changed: null};
  docs.push(data);
  var docTabs = document.getElementById("docs");
  var li = docTabs.appendChild(document.createElement("li"));
  li.appendChild(document.createTextNode(name));
  if (editor.getDoc() == doc) {
    setSelectedDoc(docs.length - 1);
    curDoc = data;
  }
  server.addFile(name, doc.getValue());
  CodeMirror.on(doc, "change", trackChange);
}

function trackChange(doc, change) {
  if (cachedFunction.line > change.from.line ||
      cachedFunction.line == change.from.line && cachedFunction.ch >= change.from.ch)
    cachedFunction.line = -1;

  for (var i = 0; i < docs.length; ++i) {var data = docs[i]; if (data.doc == doc) break;}
  var changed = data.changed;
  if (changed == null)
    data.changed = changed = {from: change.from.line, to: change.from.line};
  var end = change.from.line + (change.text.length - 1);
  if (change.from.line < changed.to) changed.to = changed.to - (change.to.line - end);
  if (end >= changed.to) changed.to = end + 1;
  if (changed.from > change.from.line) changed.from = change.from.line;

  if (doc.lineCount() > bigDoc && change.to - changed.from > 100) setTimeout(function() {
    if (data.changed && data.changed.to - data.changed.from > 100) sendDoc(data);
  }, 100);
}

function unregisterDoc(doc) {
  server.delFile(doc.name);
  for (var i = 0; i < docs.length && doc != docs[i]; ++i) {}
  docs.splice(i, 1);
  var docList = document.getElementById("docs");
  docList.removeChild(docList.childNodes[i]);
  selectDoc(Math.max(0, i - 1));
  CodeMirror.off(doc.doc, "change", trackChange);
}

function setSelectedDoc(pos) {
  var docTabs = document.getElementById("docs");
  for (var i = 0; i < docTabs.childNodes.length; ++i)
    docTabs.childNodes[i].className = pos == i ? "selected" : "";
}

function selectDoc(pos) {
  cachedFunction.bad = true;
  setSelectedDoc(pos);
  if (curDoc.changed) sendDoc(curDoc);
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
          offsetLines: from.line,
          text: cm.getRange(from, Pos(endLine, 0))};
}

function displayError(err) {
  var out = document.getElementById("out");
  out.innerHTML = "";
  out.appendChild(document.createTextNode(err.message || String(err)));
}

function incLine(off, pos) { return Pos(pos.line + off, pos.ch); }

function buildRequest(cm, query, allowFragments) {
  var files = [], offsetLines = 0;
  if (typeof query == "string") query = {type: query};
  query.lineCharPositions = true;
  if (query.end == null) {
    query.end = cm.getCursor("end");
    if (cm.somethingSelected())
      query.start = cm.getCursor("start");
  }
  var startPos = query.start || query.end;

  if (curDoc.changed) {
    if (cm.lineCount() > bigDoc && allowFragments !== false &&
        curDoc.changed.to - curDoc.changed.from < 100 &&
        curDoc.changed.from <= startPos.line && curDoc.changed.to > query.end.line) {
      files.push(getFragmentAround(cm, startPos, query.end));
      query.file = "#0";
      var offsetLines = files[0].offsetLines;
      if (query.start != null) query.start = incLine(-offsetLines, query.start);
      query.end = incLine(-offsetLines, query.end);
    } else {
      files.push({type: "full",
                  name: curDoc.name,
                  text: cm.getValue()});
      query.file = curDoc.name;
      curDoc.changed = null;
    }
  } else {
    query.file = curDoc.name;
  }
  for (var i = 0; i < docs.length; ++i) {
    var doc = docs[i];
    if (doc.changed && doc != curDoc) {
      files.push({type: "full", name: doc.name, text: doc.doc.getValue()});
      doc.changed = null;
    }
  }

  return {query: query, files: files};
}

function sendDoc(doc) {
  server.request({files: [{type: "full", name: doc.name, text: doc.doc.getValue()}]}, function(error) {
    if (error) return displayError(error);
    else doc.changed = null;
  });
}

function findType(cm) {
  server.request(buildRequest(cm, "type"), function(error, data) {
    if (error) return displayError(error);
    var out = document.getElementById("out");
    out.innerHTML = "";
    out.appendChild(document.createTextNode(data.type || "not found"));
    if (data.doc || data.url) {
      var docnode = out.appendChild(document.createElement("div"));
      docnode.className = "hint-doc";
      docnode.style.marginTop = ".3em";
      if (data.doc) docnode.appendChild(document.createTextNode(data.doc + " "));
      if (data.url) {
        var a = docnode.appendChild(document.createElement("a"));
        a.innerHTML = "[docs]";
        a.href = data.url;
      }
    }
  });
}

function typeToIcon(type) {
  var suffix;
  if (type == "?") suffix = "unknown";
  else if (type == "number" || type == "string" || type == "bool") suffix = type;
  else if (/^fn\(/.test(type)) suffix = "fn";
  else if (/^\[/.test(type)) suffix = "array";
  else suffix = "object";
  return "Tern-completion Tern-completion-" + suffix;
}

function ternHints(cm, c) {
  var req = buildRequest(cm, {type: "completions", types: true, docs: true});

  server.request(req, function(error, data) {
    if (error) return displayError(error);
    var completions = [], after = "";
    var from = data.start, to = data.end;
    if (cm.getRange(Pos(from.line, from.ch - 2), from) == "[\"" &&
        cm.getRange(to, Pos(to.line, to.ch + 2)) != "\"]")
      after = "\"]";

    for (var i = 0; i < data.completions.length; ++i) {
      var completion = data.completions[i], className = typeToIcon(completion.type);
      if (data.guess) className += " Tern-completion-guess";
      completions.push({text: completion.name + after,
                        displayText: completion.name,
                        className: className,
                        doc: completion.doc});
    }

    var out = document.getElementById("out");
    var obj = {from: from, to: to, list: completions};
    CodeMirror.on(obj, "close", function() { out.innerHTML = ""; });
    CodeMirror.on(obj, "select", function(cur) {
      out.innerHTML = "";
      if (cur.doc) {
        var node = out.appendChild(document.createElement("div"));
        node.className = "hint-doc";
        node.appendChild(document.createTextNode(cur.doc));
      }
    });
    c(obj);
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
    var name = text.slice(pos).match(/^([^, \(\[\{]+): /);
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
    server.request(buildRequest(cm, query), function(error, data) {
      if (error) return;
      if (!data.type || !(/^fn\(/).test(data.type)) return;
    
      cache.type = parseFnType(data.type);
      cache.name = data.exprName || data.name || "fn";
      cache.bad = false;
      cache.guess = data.guess;
      showArgumentHints(cache, out, pos);
    });
  } else if (!cache.bad) {
    showArgumentHints(cache, out, pos);
  }
}

function showArgumentHints(cache, out, pos) {
  if (cache.guess) {
    out = out.appendChild(document.createElement("div"));
    out.className = "Tern-fhint-guess";
  }
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

function moveTo(name, start, end) {
  if (name == curDoc.name) return curDoc.doc.setSelection(end, start);

  for (var i = 0; i < docs.length; ++i) if (docs[i].name == name) {
    selectDoc(i);
    setTimeout(function() { curDoc.doc.setSelection(end, start); }, 50);
    return;
  }
  displayError("Definition is not in a local buffer");
}

// The {line,ch} representation of positions makes this rather awkward.
function findContext(data) {
  var doc = findDoc(data.file).doc;
  var before = data.context.slice(0, data.contextOffset).split("\n");
  var startLine = data.start.line - (before.length - 1);
  var start = Pos(startLine, (before.length == 1 ? data.start.ch : doc.getLine(startLine).length) - before[0].length);

  var text = doc.getLine(startLine).slice(start.ch), off = 0;
  for (var cur = startLine + 1; cur < doc.lineCount() && text.length < data.context.length; ++cur)
    text += "\n" + doc.getLine(cur);
  if (text.slice(0, data.context.length) == data.context) return data;

  var cursor = doc.getSearchCursor(data.context, 0, false);
  var nearest, nearestDist = Infinity;
  while (cursor.findNext()) {
    var from = cursor.from(), dist = Math.abs(from.line - start.line) * 10000;
    if (!dist) dist = Math.abs(from.ch - start.ch);
    if (dist < nearestDist) { nearest = from; nearestDist = dist; }
  }
  if (!nearest) return null;

  if (before.length == 1)
    nearest.ch += before[0].length;
  else
    nearest = Pos(nearest.line + (before.length - 1), before[before.length - 1].length);
  if (data.start.line == data.end.line)
    end = Pos(nearest.line, nearest.ch + (data.end.ch - data.start.ch));
  else
    end = Pos(nearest.line + (data.end.line - data.start.line), data.end.ch);
  return {start: nearest, end: end};
}

function atInterestingExpression(cm) {
  var pos = cm.getCursor("end"), tok = cm.getTokenAt(pos);
  if (tok.start < pos.ch && (tok.type == "comment" || tok.type == "string")) return false;
  return /\w/.test(cm.getLine(pos.line).slice(Math.max(pos.ch - 1, 0), pos.ch + 1));
}

function jumpToDef(cm, named) {
  if (named == true || !atInterestingExpression(cm))
    cm.openDialog("Jump to variable: <input type=text>", inner);
  else
    inner();

  function inner(v) {
    server.request(buildRequest(cm, {type: "definition", variable: v || null}), function(error, data) {
      if (error) return displayError(error);
      if (data.file) {
        var found = findContext(data);
        if (!found)
          return displayError("Could not find a definition.");
        jumpStack.push({file: curDoc.name,
                        start: cm.getCursor("from"),
                        end: cm.getCursor("to")});
        moveTo(data.file, found.start, found.end);
      } else if (data.url) {
        document.getElementById("out").innerHTML = "Opening documentation in a new window.";
        window.open(data.url);
      } else {
        displayError("Could not find a definition.");
      }
    });
  }
}

function jumpBack(cm) {
  var pos = jumpStack.pop();
  if (!pos) return;
  moveTo(pos.file, pos.start, pos.end);
}

var nextChangeOrig = 0;
function applyChanges(changes) {
  var perFile = Object.create(null);
  for (var i = 0; i < changes.length; ++i) {
    var ch = changes[i];
    (perFile[ch.file] || (perFile[ch.file] = [])).push(ch);
  }
  for (var file in perFile) {
    var chs = perFile[file], doc = findDoc(file).doc;
    chs.sort(function(a, b) { return b.start.line - a.start.line || b.start.ch - a.start.ch; });
    var origin = "*" + (++nextChangeOrig);
    for (var i = 0; i < chs.length; ++i) {
      var ch = chs[i];
      doc.replaceRange(ch.text, ch.start, ch.end, origin);
    }
  }
}

function renameVar(cm) {
  var cur = cm.getCursor(), token = cm.getTokenAt(cur);
  if (!/^variable|^def$/.test(token.type)) {
    token = cm.getTokenAt(Pos(cur.line, cur.ch + 1));
//    if (!/^variable|^def$/.test(token.type)) return displayError("Not at a variable name");
  }
  cm.openDialog("New name for " + token.string + ": <input type=text>", function(newName) {
    server.request(buildRequest(cm, {type: "rename", newName: newName}, false), function(error, data) {
      if (error) return displayError(error);
      applyChanges(data.changes);
    });
  });
}

var commands = {
  complete: function(cm) { CodeMirror.showHint(cm, ternHints, {async: true}); },
  jumptodef: jumpToDef,
  findtype: findType,
  rename: renameVar,
  addfile: function() {
    var name = prompt("Name of the new buffer", "");
    if (name == null) return;
    if (!name) name = "test";
    var i = 0;
    while (findDoc(name + (i || ""))) ++i;
    registerDoc(name + (i || ""), new CodeMirror.Doc("", "javascript"));
    selectDoc(docs.length - 1);
  },
  delfile: function() {
    if (docs.length == 1) return;
    unregisterDoc(curDoc);
  }
};
