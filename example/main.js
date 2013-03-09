var fs = require('fs')
var tern = require('../')
var acorn = require('acorn')
var path = require('path')

var environment = [
  JSON.parse(fs.readFileSync('ecma5.json')),
  JSON.parse(fs.readFileSync('browser.json'))
]

var files = {
  'name_of_the_file.js': fs.readFileSync(path.join(__dirname, 'name_of_the_file.js'), 'utf8'),
  'name_of_other_file.js': fs.readFileSync(path.join(__dirname, 'name_of_other_file.js'), 'utf8')
}

var getFile = function (name, callback) {
  callback(null, files[name])
}

var server = new tern.Server({getFile: getFile}, environment)

server.addFile('name_of_the_file.js')
server.addFile('name_of_other_file.js')



server.request({
  query: {
    type: 'completions',
    end: 8,
    file: 'name_of_the_file.js'
  },
  files: [
    {
      type: 'full',
      name: 'name_of_the_file.js',
      text: files['name_of_the_file.js']
    }
  ]
}, function (e, data) {
  if(e) throw e
  console.log(require('util').inspect(data.completions));
})


server.request({
  query: {
    type: 'type',
    end: 39,
    file: 'name_of_other_file.js'
  },
  files: []
}, function (e, data) {
  if(e) throw e
  console.log(require('util').inspect(data));
})