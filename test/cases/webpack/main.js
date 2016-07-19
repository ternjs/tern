// plugin=webpack {"configPath": "./webpack.config.js"}

require("foo") //:: {browser: bool}

require("foo/index") //:: {index: bool}

require("./component/foo") //:: {index: bool}

require("component/foo") //:: {index: bool}

require("xyz") //:: {index: bool}

require("esnext") //:: {default: {index: bool}}
