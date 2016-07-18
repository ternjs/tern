var path = require('path')

module.exports = {
  resolve: {
    packageMains: ['jsnext:main', 'browser', 'browserify', 'main'],
    root: __dirname,
    alias: {
      xyz: "modu"
    }
  }
}
