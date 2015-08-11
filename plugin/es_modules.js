(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    return mod(require("../lib/infer"), require("../lib/tern"), require("acorn/dist/walk"), require("./modules"))
  if (typeof define == "function" && define.amd) // AMD
    return define(["../lib/infer", "../lib/tern", "acorn/dist/walk", "./modules"], mod)
  mod(tern, tern, acorn.walk)
})(function(infer, tern, walk) {
  "use strict"

  function connectModule(file, out) {
    var modules = infer.cx().parent.mod.modules
    var outObj = null
    function exp(prop, type, originNode) {
      if (!outObj) {
        outObj = new infer.Obj(true)
        out.addType(outObj)
      }
      type.propagate(outObj.defProp(prop, originNode))
    }

    walk.simple(file.ast, {
      ImportDeclaration: function(node) {
        var input = modules.resolveModule(node.source.value, file.name)
        for (var i = 0; i < node.specifiers.length; i++) {
          var spec = node.specifiers[i]
          var aval = file.scope.getProp(spec.local.name)
          if (spec.type == "ImportNamespaceSpecifier") {
            input.propagate(aval)
          } else {
            var propName = spec.type == "ImportDefaultSpecifier" ? "default" : spec.imported.name
            input.getProp(propName).propagate(aval)
          }
        }
      },
      ExportAllDeclaration: function(node) {
        var input = modules.resolveModule(node.source.value, file.name)
        input.forAllProps(function(prop, val, local) {
          if (local) exp(prop, val, val.originNode)
        })
      },
      ExportDefaultDeclaration: function(node) {
        var decl = node.declaration.id || node.declaration
        exp("default", infer.expressionType({node: decl, scope: file.scope}), decl)
      },
      ExportNamedDeclaration: function(node) {
        var decl = node.declaration
        if (decl) {
          if (decl.type == "VariableDeclaration") {
            for (var i = 0; i < decl.declarations.length; ++i) {
              var cur = decl.declarations[i]
              if (cur.id.type == "Identifier")
                exp(cur.id.name, file.scope.getProp(cur.id.name), cur.id)
            }
          } else {
            exp(decl.id.name, file.scope.getProp(decl.id.name), decl.id)
          }
        }
        if (node.specifiers.length) {
          var src = node.source ? modules.resolveModule(node.source.value, file.name) : file.scope
          for (var i = 0; i < node.specifiers.length; i++) {
            var spec = node.specifiers[i]
            exp(spec.exported.name, src.getProp(spec.local.name), spec.local)
          }
        }
      }
    })
  }

  function isModuleName(node) {
    if (node.type != "Literal" || typeof node.value != "string") return false

    var decl = infer.findExpressionAround(node.sourceFile.ast, null, node.end, null, function(_, node) {
      return node.type == "ImportDeclaration" || /Export(All|Named)Declaration/.test(node.type)
    })
    if (!decl || decl.source != node) return false
    return node.value
  }

  tern.registerPlugin("es_modules", function(server) {
    server.loadPlugin("modules")
    server.mod.modules.on("getExports", connectModule)
    server.mod.modules.modNameTests.push(isModuleName)
  })
})
