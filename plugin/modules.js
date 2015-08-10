(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    return mod(require("../lib/infer"), require("../lib/tern"), require("../lib/signal"), require)
  if (typeof define == "function" && define.amd) // AMD
    return define(["../lib/infer", "../lib/tern", "../lib/signal"], mod)
  mod(tern, tern, tern.signal)
})(function(infer, tern, signal, require) {
  "use strict"

  function Modules(server, options) {
    this.server = server
    this.options = options || {}
    this.modules = Object.create(null)
    this.nonRelative = Object.create(null)
    this.resolvers = []
    this.modNameTests = []
  }

  Modules.prototype = signal.mixin({
    buildWrappingScope: function(parent, origin, node) {
      var scope = new infer.Scope(parent, node)
      scope.origin = origin
      this.signal("wrapScope", scope)
      return scope
    },

    maybeOverride: function(name) {
      if (!this.options.modules || !this.options.modules.hasOwnProperty(name))
        return false
      if (this.modules[name]) return this.modules[name]

      var override = this.options.modules[name]
      if (typeof(override) == "string" && override.charAt(0) == "=")
        return infer.def.parsePath(override.slice(1))

      var scope = this.buildWrappingScope(infer.cx().topScope, name)
      infer.def.load(override, scope)
      return this.modules[name] = scope.exports
    },
    
    resolveModuleInner: function(name, parentFile) {
      var over = this.maybeOverride(name)
      if (over) return over
      if (this.options.dontLoad == true ||
          this.options.dontLoad && new RegExp(this.options.dontLoad).test(name) ||
          this.options.load && !new RegExp(this.options.load).test(name))
        return infer.ANull

      var resolved
      for (var i = 0; !resolved && i < this.resolvers.length; i++)
        resolved = this.resolvers[i](name, parentFile)
      if (!resolved) return infer.ANull
      if (typeof resolved != "string") return resolved

      var known = this.modules[resolved]
      if (known) return known

      if (/\.js$|(?:^\/)[^\.]+$/.test(resolved))
        this.server.addFile(resolved, null, parentFile)
      return this.modules[resolved] = new infer.AVal
    },

    resolveModule: function(name, parentFile) {
      var resolved = this.resolveModuleInner(name, parentFile)
      if (!/^\.\.?\//.test(name) && resolved != infer.ANull) this.nonRelative[name] = true
      return resolved
    },

    isModName: function(node) {
      for (var i = 0; i < this.modNameTests.length; i++) {
        var name = this.modNameTests[i](node)
        if (name != null) return name
      }
    },

    get: function(name) {
      return this.modules[name] || (this.modules[name] = new infer.AVal)
    }
  })

  function preCondenseReach(state) {
    var mods = infer.cx().parent.mod.modules.modules
    var node = state.roots["!modules"] = new infer.Obj(null)
    for (var name in mods) {
      var mod = mods[name]
      var id = mod.origin || name
      var prop = node.defProp(id.replace(/\./g, "`"))
      mod.propagate(prop)
      prop.origin = mod.origin
    }
  }

  function postLoadDef(data) {
    var cx = infer.cx(), mods = cx.definitions[data["!name"]]["!modules"]
    var me = cx.parent.mod.modules
    if (mods) for (var name in mods.props) {
      var origin = name.replace(/`/g, ".")
      var mod = me.get(origin)
      mod.origin = origin
      mods.props[name].propagate(mod)
    }
  }

  function findTypeAt(_file, _pos, expr, type) {
    var me = infer.cx().parent.mod.modules
    var modName = me.isModName(expr.node)
    if (!modName) return type

    var modType = me.resolveModule(modName, expr.node.sourceFile.name).getType()
    if (!modType) return type

    // The `type` is a value shared for all string literals.
    // We must create a copy before modifying `origin` and `originNode`.
    // Otherwise all string literals would point to the last jump location
    type = Object.create(type)
    type.origin = modType.origin
    type.originNode = modType.originNode
    if (modType.doc) type.doc = modType.doc
    if (modType.url) type.url = modType.url
    return type
  }

  // Complete previously seen module names when completing in strings passed to require
  function findCompletions(file, query) {
    var wordEnd = tern.resolvePos(file, query.end)
    var me = infer.cx().parent.mod.modules
    var lit = infer.findExpressionAround(file.ast, null, wordEnd, file.scope, "Literal")
    var modName = lit && me.isModName(lit.node)
    if (modName == null) return

    var argNode = lit.node
    if (argNode.type != "Literal" || typeof argNode.value != "string" ||
        argNode.start > wordEnd || argNode.end < wordEnd) return

    var word = argNode.raw.slice(1, wordEnd - argNode.start), quote = argNode.raw.charAt(0)
    if (word && word.charAt(word.length - 1) == quote)
      word = word.slice(0, word.length - 1)
    if (query.caseInsensitive) word = word.toLowerCase()

    var completions
    if (/^\.\.\//.test(word)) completions = completeFileName(query, file, word)
    else completions = completeModuleName(query, word)

    if (argNode.end == wordEnd + 1 && file.text.charAt(wordEnd) == quote)
      ++wordEnd
    return {
      start: tern.outputPos(query, file, argNode.start),
      end: tern.outputPos(query, file, wordEnd),
      isProperty: false,
      completions: completions.map(function(rec) {
        var name = typeof rec == "string" ? rec : rec.name
        var string = JSON.stringify(name)
        if (quote == "'") string = quote + string.slice(1, string.length -1).replace(/'/g, "\\'") + quote
        if (typeof rec == "string") return string
        rec.displayName = name
        rec.name = string
        return rec
      })
    }
  }

  function completeModuleName(query, word) {
    var completions = []
    var cx = infer.cx()

    function fromObj(obj) {
      for (var name in obj) {
        if (query.filter === false || !word ||
            (query.caseInsensitive ? name.toLowerCase() : name).indexOf(word) == 0) {
          var val = obj[name]
          tern.addCompletion(query, completions, name, typeof val == "object" ? val : null)
        }
      }
    }

    fromObj(cx.definitions.node)
    fromObj(cx.parent.mod.modules.nonRelative)
    return completions
  }

  function completeFileName() { return [] }
  if (require) (function() {
    var fs = require("fs"), path = require("path")

    completeFileName = function(query, file, word) {
      var completions = [], server = infer.cx().parent, pDir = server.projectDir
      var pt = path.resolve(pDir, file.name, word)
      var filePart = path.basename(word)
      var dir = /\/$/.test(pt) ? pt : path.dirname(pt)

      fs.readdirSync(dir).forEach(function(file) {
        if (/^\./.test(file)) return
        if (query.filter === false || !filePart ||
            (query.caseInsensitive ? file.toLowerCase() : file).indexOf(filePart) == 0) {
          var value = server.mod.modules.modules[path.relative(pDir, path.resolve(dir, file))]
          if (/\.js$/.test(file)) file = file.slice(0, file.length - 3)
          tern.addCompletion(query, completions, file, value)
        }
      })
      return completions
    }
  }())

  tern.registerPlugin("modules", function(server, options) {
    server.mod.modules = new Modules(server, options)

    server.on("beforeLoad", function(file) {
      file.scope = this.mod.modules.buildWrappingScope(file.scope, file.name, file.ast)
    })

    server.on("afterLoad", function(file) {
      var mod = this.mod.modules.get(file.name)
      mod.origin = file.name
      this.mod.modules.signal("getExports", file, mod)
    })

    server.on("reset", function() {
      this.mod.modules.modules = Object.create(null)
    })

    return {passes: {preCondenseReach: preCondenseReach,
                     postLoadDef: postLoadDef,
                     typeAt: findTypeAt,
                     completion: findCompletions}}
  })
})
