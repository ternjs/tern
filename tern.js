(function(exports) {
  var infer, condense;
  if (typeof require != "undefined") {
    infer = require("./infer.js");
    condense = require("./condense.js");
  } else {
    infer = condense = exports;
  }

  var plugins = Object.create(null);
  exports.registerPlugin = function(name, init) { plugins[name] = init; };

  var Server = exports.Server = function(callbacks) {
    this.cx = null;
    this.callbacks = callbacks;
    this.environment = [];
    this.filesToLoad = [];
    this.handlers = {};

    this.pendingFiles = [];
    this.files = this.uses = 0;
  };
  Server.prototype = {
    addEnvironment: function(data) {
      this.environment.push(data);
      var plugin = data["!plugin"];
      if (plugin && plugin in plugins) plugins[plugin](this);
    },
    addFile: function(file) {
      if (this.filesToLoad.indexOf(file) < 0) this.filesToLoad.push(file);
    },

    // Used from inside the analyzer to load, for example, a
    // `require`-d file.
    require: function(filename) {
      this.pendingFiles.push(filename);
    },

    request: function(doc, c) {
      // FIXME somehow validate doc's structure

      var self = this, files = doc.files || [];
      // FIXME better heuristic for when to reset. And try to reset when the client is not waiting
      if (!this.cx || this.uses > 20) reset(this);
      ++this.cx.uses;
      doRequest(this, doc, c);
    },

    findFile: function(name) {
      return this.files && findFile(this.files, name);
    },

    on: function(type, f) {
      (this.handlers[type] || (this.handlers[type] = [])).push(f);
    },
    off: function(type, f) {
      var arr = this.handlers[type];
      if (arr) for (var i = 0; i < arr.length; ++i)
        if (arr[i] == f) { arr.splice(i, 1); break; }
    },
    signal: function(type, v1, v2, v3, v4) {
      var arr = this.handlers[type];
      if (arr) for (var i = 0; i < arr.length; ++i) arr[i].call(this, v1, v2, v3, v4);
    }
  };

  function reset(srv) {
    srv.cx = new infer.Context(srv.environment, srv);
    srv.uses = 0;
    srv.files = [];
    srv.pendingFiles = srv.filesToLoad.slice(0);
    srv.signal("reset");
  }

  function doRequest(srv, doc, c) {
    var files = doc.files || [];
    for (var i = 0; i < files.length; ++i) {
      var file = files[i];
      if (file.type == "full") loadFile(srv, file.name, file.text);
    }

    finishPending(srv, function(err) {
      if (err) return c(err);
      infer.withContext(srv.cx, function() {
        var file = resolveFile(srv, files, doc.query.file);
        // FIXME reinstate this when the code stops crashing all the time
        // try {
        switch (doc.query.type) {
        case "completions":
          return c(null, findCompletions(file, doc.query));
        case "type":
          return c(null, findTypeAt(file, doc.query));
        case "definition":
          if (file.type == "part") throw new Error("Can't run a definition query on a file fragment");
          return c(null, findDef(file, doc.query));
        default:
          c("Unsupported query type: " + doc.query.type);
        }
        // } catch (e) { c(e.message || e); }
      });
    });
  }

  function loadFile(srv, filename, text) {
    infer.withContext(srv.cx, function() {
      var file = {name: filename, text: text};
      srv.signal("beforeLoad", file);
      var result = infer.analyze(file.text, filename);
      var known = findFile(srv.files, filename);
      if (!known) srv.files.push(known = {name: filename});
      known.text = file.text;
      known.ast = result.ast;
      srv.signal("afterLoad", known);
    });
  }

  function finishPending(srv, c) {
    var next;
    while (next = srv.pendingFiles.pop())
      if (!findFile(srv.files, next)) break;
    if (!next) return c();

    srv.callbacks.getFile(next, function(err, text) {
      if (err) return c(err);
      loadFile(srv, next, text);
      finishPending(srv, c);
    });
  }

  function findFile(arr, name) {
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

  function resolveFile(srv, localFiles, name) {
    var file, isRef = name.match(/^#(\d+)$/);
    if (isRef)
      file = localFiles[isRef[1]];
    else
      file = findFile(srv.files, name);
    if (!file) throw new Error("Reference to unknown file " + name);

    if (file.type == "part") {
      var realFile = findFile(srv.files, file.name);
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
    var wordStart = query.end, wordEnd = wordStart, text = file.text;
    while (wordStart && /\w$/.test(text.charAt(wordStart - 1))) --wordStart;
    while (wordEnd < text.length && /\w$/.test(text.charAt(wordEnd))) ++wordEnd;
    var word = text.slice(wordStart, wordEnd), completions, guessing = false;

    infer.resetGuessing();
    // FIXME deal with whitespace before/after dot
    if (text.charAt(wordStart - 1) == ".") { // Property completion
      var expr = infer.findExpressionAt(file.ast, null, wordStart - 1, file.scope);
      var tp = expr && infer.expressionType(expr);
      if (tp)
        completions = infer.propertiesOf(tp.type, word);
      else
        completions = [];
    } else {
      completions = infer.localsAt(file.ast, query.end, word);
    }
    return {from: wordStart, to: wordEnd,
            completions: completions,
            guess: infer.didGuess()};
  }

  function findExpr(file, query) {
    var expr = infer.findExpressionAt(file.ast, query.start, query.end, file.scope);
    if (expr) return expr;
    expr = infer.findExpressionAround(file.ast, query.start, query.end, file.scope);
    if (expr && (query.start == null || query.start - expr.node.start < 10) &&
        expr.node.end - query.end < 10) return expr;
    throw new Error("No expression at the given position.");
  }

  function findTypeAt(file, query) {
    var expr = findExpr(file, query);
    infer.resetGuessing();
    var type = infer.expressionType(expr);
    if (typeof window != "undefined") window.tp = type; // FIXME debug statement
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
            exprName: exprName || null,
            guess: infer.didGuess()};
  }

  function findDef(file, query) {
    var expr = findExpr(file, query), def, file, guess = false;
    if (expr.node.type == "Identifier") {
      var found = expr.state.findVar(expr.node.name);
      if (found && typeof found.name == "object") {
        def = found.name;
        file = found.origin;
      }
    }
    if (!def) {
      infer.resetGuessing();
      var type = tern.expressionType(expr);
      if (type.types) for (var i = 0; i < type.types.length; ++i) {
        var tp = type.types[i];
        if (tp.originNode) { type = tp; break; }
      }
      def = type.originNode;
      if (/^Function/.test(def.type) && def.id) def = def.id;
      file = type.origin;
      guess = infer.didGuess();
    }
    if (!def) throw new Error("Could not find a definition for the given expression");
    return {start: def.start, end: def.end, file: file, guess: guess};
  }
})(typeof exports == "undefined" ? window.tern || (window.tern = {}) : exports);
