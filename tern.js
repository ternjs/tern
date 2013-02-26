(function(exports) {
  var infer, condense;
  if (typeof require != "undefined") {
    infer = require("./infer.js");
    condense = require("./condense.js");
  } else {
    infer = condense = exports;
  }

  var Tern = exports.Tern = function(callbacks) {
    this.cx = null;
    this.uses = 0;
    this.callbacks = callbacks;
    this.environment = [];
    this.filesToLoad = [];
    this.files = null;
  };
  Tern.prototype = {
    addEnvironment: function(data) {
      this.environment.push(data);
    },
    addFile: function(file) {
      if (this.filesToLoad.indexOf(file) < 0) this.filesToLoad.push(file);
    },

    request: function(doc, c, clean) {
      // FIXME somehow validate doc's structure

      var self = this, files = doc.files || [];
      // FIXME better heuristic for when to reset. And try to reset when the client is not waiting
      if (!this.cx || this.uses > 20)
        return reset(this, files, function() {self.request(doc, c, true);});
      ++this.uses;

      for (var i = 0; i < files.length; ++i) {
        var file = files[i];
        if (file.type == "full" && (!clean || !findFile(this.files, file.name)))
          loadFile(this, file.name, file.text);
      }

      infer.withContext(this.cx, function() {
// FIXME reinstate this when the code stops crashing all the time
//        try {
          switch (doc.query.type) {
          case "completions":
            c(null, findCompletions(resolveFile(self, files, doc.query.file), doc.query));
            break;
          case "type":
            c(null, findTypeAt(resolveFile(self, files, doc.query.file), doc.query));
            break;
          default:
            c("Unsupported query type: " + doc.query.type);
          }
  /*      } catch (e) {
          c(e.message || e);
        }*/
      });
    }
  };

  function reset(tern, replacements, c) {
    tern.cx = new infer.Context(tern.environment);
    tern.uses = 0;
    tern.files = [];

    function step(i) {
      if (i >= tern.filesToLoad.length) return c(); 
      var file = tern.filesToLoad[i], repl;
      if (replacements) for (var j = 0; j < replacements.length; ++j) {
        var cur = replacements[j];
        if (cur.name == file && cur.type == "full") {
          repl = cur.content;
          break;
        }
      }
      if (repl) {
        loadFile(tern, file, repl);
        step(i + 1);
      } else {
        tern.callbacks.getFile(file, function(err, content) {
          if (err) return c(err);
          loadFile(tern, file, content);
          step(i + 1);
        });
      }
    }
    step(0);
  }

  function loadFile(tern, filename, data) {
    infer.withContext(tern.cx, function() {
      var result = infer.analyze(data, filename);
      var known = findFile(tern.files, filename);
      if (!known) tern.files.push(known = {name: filename});
      known.text = data;
      known.ast = result.ast;
    });
  }

  function findFile(arr, name) {
    if (!arr) debugger;
    for (var i = 0; i < arr.length; ++i) {
      var file = arr[i];
      if (file.name == name && file.type != "part") return file;
    }
  }

  function firstLine(str) {
    var end = str.indexOf("\n");
    if (end < 0) return str;
    return str.slice(0, end);
  }

  function findMatchingPosition(line, file, near) {
    var pos = 0, closest = null;
    if (!/^\s*$/.test(line)) for (;;) {
      var found = file.indexOf(line, pos);
      if (found < 0) break;
      if (closest == null || Math.abs(closest - near) > Math.abs(found - near))
        closest = found;
      pos = found + line.length;
    }
    return closest;
  }

  function resolveFile(tern, localFiles, name) {
    var file, isRef = name.match(/^#(\d+)$/);
    if (isRef)
      file = localFiles[isRef[1]];
    else
      file = findFile(tern.files, name);
    if (!file) throw new Error("Reference to unknown file " + name);

    if (file.type == "part") {
      var realFile = findFile(tern.files, file.name);
      if (!realFile) throw new Error("Partial file provided for " + file.name + ", which is not known");
      var line = firstLine(file.text);
      var foundPos = findMatchingPosition(line, realFile.text, file.position);
      var pos = foundPos == null ? Math.max(0, realFile.text.lastIndexOf("\n", file.position)) : foundPos;

      var scope = file.scope = infer.scopeAt(realFile.ast, pos), text = file.text, m;
      if (foundPos && (m = line.match(/^(.*?)\bfunction\b/))) {
        var cut = m[1].length, white = "";
        for (var i = 0; i < cut; ++i) white += " ";
        text = white + text.slice(cut);
      }
      file.ast = infer.analyze(file.text, file.name, scope).ast;

      // This is a kludge to tie together the function types (if any)
      // outside and inside of the fragment, so that arguments and
      // return values have some information known about them.
      var inner = infer.scopeAt(realFile.ast, pos + line.length);
      if (m && inner != scope && inner.fnType) {
        var newInner = infer.scopeAt(file.ast, line.length, scope);
        var fOld = inner.fnType, fNew = newInner.fnType;
        if (fNew && (fNew.name == fOld.name || !fOld.name)) {
          for (var i = 0, e = Math.min(fOld.args.length, fNew.args.length); i < e; ++i)
            fOld.args[i].propagate(fNew.args[i]);
          fOld.self.propagate(fNew.self);
          fNew.retval.propagate(fOld.retval);
        }
      }
    }
    return file;
  }

  function findCompletions(file, query) {
    var wordStart = query.position, wordEnd = wordStart, text = file.text;
    while (wordStart && /\w$/.test(text.charAt(wordStart - 1))) --wordStart;
    while (wordEnd < text.length && /\w$/.test(text.charAt(wordEnd))) ++wordEnd;
    var word = text.slice(wordStart, wordEnd), completions;

    // FIXME deal with whitespace before/after dot
    if (text.charAt(wordStart - 1) == ".") { // Property completion
      var expr = infer.findExpression(file.ast, null, wordStart - 1, file.scope);
      var tp = expr && infer.expressionType(expr);
      if (tp)
        completions = infer.propertiesOf(tp, word);
      else
        completions = [];
    } else {
      completions = infer.localsAt(file.ast, query.position, word);
    }
    return {from: wordStart, to: wordEnd, completions: completions};
  }

  function findTypeAt(file, query) {
    var expr = infer.findExpression(file.ast, query.start, query.end, file.scope);
    if (!expr) return {type: null, name: null, message: "No expression at the given position"};
    var type = infer.expressionType(expr);
    window.tp = type; // FIXME debug statement
    if (query.preferFunction)
      type = type.getFunctionType() || type.getType();
    else
      type = type.getType();

    if (expr.node.type == "Identifier")
      var exprName = expr.node.name;
    else if (expr.node.type == "MemberExpression" && !expr.node.computed)
      var exprName = expr.node.property.name;

    var name = type && type.name;
    if (name && typeof name != "string") name = name.name;

    return {type: infer.toString(type, query.depth),
            name: name || null,
            exprName: exprName || null};
  }

})(typeof exports == "undefined" ? window.tern || (window.tern = {}) : exports);
