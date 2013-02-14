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

  function endOfLine(text, pos) {
    return Math.m
  }

  function findFilePosition(line, file, near) {
    var pos = 0, closest = null;
    if (!/^\s*$/.test(line)) for (;;) {
      var found = file.indexOf(line, pos);
      if (found < 0) break;
      if (closest == null || Math.abs(closest - near) > Math.abs(found - near))
        closest = found;
      pos = found + line.length;
    }
    if (closest != null) return closest;
    return Math.max(0, file.lastIndexOf("\n", near));
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
      var pos = findFilePosition(firstLine(file.text), realFile.text, file.position);

      var scope = infer.scopeAt(realFile.ast, pos);
      file.ast = infer.analyze(file.text, file.name, scope).ast;
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
      var expr = infer.findExpression(file.ast, null, wordStart - 1);
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
    var expr = infer.findExpression(file.ast, query.start, query.end);
    if (!expr) return {typeName: null, message: "No expression at the given position"};
    var type = infer.expressionType(expr);
    return {typeName: infer.toString(type.getType(), query.depth)};
  }

})(typeof exports == "undefined" ? window.tern || (window.tern = {}) : exports);

/*
{
  query: {type: "completions",
          position: 100,
          file: "foobar.js" | "#0"},
  files: [{type: "full",
           name: "foobar.js",
           text: "....."},
          {type: "part",
           name: "baz.js",
           text: "function foo(x) {....",
           offset: 88}]
}
*/