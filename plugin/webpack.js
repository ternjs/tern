if (typeof exports != "object" || typeof module != "object")
  throw new Error("This plugin works only in a CommonJS environment")

var infer = require("../lib/infer");
var tern = require("../lib/tern");
require("./commonjs");
require("./es_modules");

var fs = require('fs');
var path = require("path");
var ResolverFactory = require("enhanced-resolve").ResolverFactory;
var SyncNodeJsInputFileSystem = require("enhanced-resolve/lib/SyncNodeJsInputFileSystem");

var file
if (process.argv[1] && /test$/.test(process.argv[1])) {
 file = path.resolve(__dirname, '../test/cases/webpack/webpack.config.js')
} else {
 file = path.resolve('./webpack.config.js')
}
var resolveConfig = fs.existsSync(file) ? require(file).resolve : null

var config = {
  unsafeCache: true,
  modules: ["node_modules"],
  extensions: [".js", ".jsx", ".json"],
  aliasFields: ["browser"],
  mainFields: ["browser", "web", "browserify", "main"],
  fileSystem: new SyncNodeJsInputFileSystem()
}

if (resolveConfig) {
  Object.keys(resolveConfig).forEach(function (key) {
    if (key === 'packageMains') {
      config.mainFields = resolveConfig[key]
    } else if (key === 'root') {
      config.modules.unshift(resolveConfig[key])
    } else {
      config[key] = resolveConfig[key]
    }
  })
}

var resolver = ResolverFactory.createResolver(config);


function resolve(name, parentFile) {
  var resolved = resolveToFile(name, parentFile)
  return resolved && infer.cx().parent.normalizeFilename(resolved)
}

function resolveToFile(name, parentFile) {
    var projectDir = infer.cx().parent.projectDir;
    var fullParent = path.resolve(projectDir, parentFile);
    try {
      return resolver.resolveSync({}, path.dirname(fullParent), name);
    } catch(e) {
      console.log(e.stack)
      return ''
    }
}

tern.registerPlugin("webpack", function(server) {
  server.loadPlugin("commonjs")
  server.loadPlugin("es_modules")
  server.mod.modules.resolvers.push(resolve)
})
