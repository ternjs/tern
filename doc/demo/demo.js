var defs = Object.create(null), project

function Project(place, config, docs) {
  this.docs = Object.create(null)
  this.curDoc = null
  this.tabs = place.appendChild(document.createElement("ul"))
  this.tabs.className = "tabs"
  var self = this
  CodeMirror.on(this.tabs, "click", function(e) {
    var target = e.target || e.srcElement
    if (target.nodeName.toLowerCase() != "li") return
    var doc = self.findDoc(target.textContent)
    if (doc) self.selectDoc(doc)
  })

  var myConf = {
    switchToDoc: function(name) { self.selectDoc(self.findDoc(name)) },
    useWorker: false
  }
  for (var prop in config) myConf[prop] = config[prop]
  var server = this.server = new CodeMirror.TernServer(myConf)

  var firstDoc
  for (var name in docs) {
    var data = this.registerDoc(name, new CodeMirror.Doc(docs[name], "javascript"))
    if (!firstDoc) firstDoc = data
  }

  var keyMap = {
    "Ctrl-I": function(cm) { server.showType(cm); },
    "Ctrl-O": function(cm) { server.showDocs(cm); },
    "Ctrl-Space": function(cm) { server.complete(cm); },
    "Alt-.": function(cm) { server.jumpToDef(cm); },
    "Alt-,": function(cm) { server.jumpBack(cm); },
    "Ctrl-Q": function(cm) { server.rename(cm); }
  }
  this.editor = new CodeMirror(place, {
    lineNumbers: true,
    extraKeys: keyMap,
    matchBrackets: true,
    value: firstDoc.doc
  })
  this.editor.on("cursorActivity", function(cm) { server.updateArgHints(cm); })
}

Project.prototype = {
  findDoc: function(name) { return this.docs[name] },

  registerDoc: function(name, doc) {
    this.server.addDoc(name, doc)
    var data = this.docs[name] = {
      name: name,
      doc: doc,
      tab: this.tabs.appendChild(document.createElement("li"))
    }
    data.tab.textContent = name
    return data
  },

  unregisterDoc: function(doc) {
    this.server.delDoc(doc.name)
    delete this.docs[doc.name]
    this.tabs.removeChild(doc.tab)
    if (this.curDoc == doc) for (var n in this.docs) return this.selectDoc(this.docs[n])
  },

  setSelectedTab: function(doc) {
    for (var n = this.tabs.firstChild; n; n = n.nextSibling)
      n.className = n.textContent == doc.name ? "selected" : ""
  },

  selectDoc: function(doc) {
    if (doc == this.curDoc) return
    this.server.hideDoc(this.curDoc)
    this.setSelectedTab(doc)
    this.curDoc = doc
    this.editor.swapDoc(doc.doc)
  }
}

// Initialization

function load(file, c) {
  var xhr = new XMLHttpRequest();
  xhr.open("get", file, true);
  xhr.send();
  xhr.onreadystatechange = function() {
    if (xhr.readyState == 4) c(xhr.responseText, xhr.status);
  };
}

CodeMirror.on(window, "load", function() {
  var toLoad = ["ecma5", "ecma6", "browser", "jquery"], loaded = 0
  for (var i = 0; i < toLoad.length; ++i) (function(i) {
    load("../../defs/" + toLoad[i] + ".json", function(json) {
      defs[toLoad[i]] = JSON.parse(json)
      if (++loaded == toLoad.length) initProject("default")
    })
  })(i)

  var cmds = document.getElementById("commands")
  CodeMirror.on(cmds, "change", function() {
    if (!project || cmds.selectedIndex == 0) return
    var found = commands[cmds.value]
    cmds.selectedIndex = 0
    project.editor.focus()
    if (found) found()
  })
})

function loadFiles(project, c) {
  var found = {}
  function inner(node) {
    while (node && node.nodeName != "PRE") node = node.nextSibling
    if (!node) return c(found)
    if (node.hasAttribute("file")) {
      load(node.getAttribute("file"), function(data) {
        found[node.id] = data
        inner(node.nextSibling)
      })
    } else {
      found[node.id] = node.textContent
      inner(node.nextSibling)
    }
  }
  inner(project.firstChild)
}

function words(str) {
  return str ? str.split(" ") : []
}

function initProject(name) {
  var node = document.getElementById(name)
  var plugins = {}, myDefs = []
  words(node.getAttribute("data-plugins")).forEach(function(name) { plugins[name] = true })
  words(node.getAttribute("data-libs")).forEach(function(name) { myDefs.push(defs[name]) })
  
  loadFiles(node, function(files) {
    var place = document.getElementById("place")
    place.textContent = ""

    project = new Project(place, {
      defs: myDefs,
      plugins: plugins
    }, files)
  })
}

var commands = {
  complete: function() { project.server.complete(project.editor) },
  jumptodef: function() { project.server.jumpToDef(project.editor) },
  finddocs: function() { project.server.showDocs(project.editor) },
  findtype: function() { project.server.showType(project.editor) },
  rename: function() { project.server.rename(project.editor) },
  addfile: function() {
    var name = prompt("Name of the new buffer", "")
    if (name == null) return
    if (!name) name = "test"
    var i = 0
    while (project.findDoc(name + (i || ""))) ++i
    project.selectDoc(project.registerDoc(name + (i || ""), new CodeMirror.Doc("", "javascript")))
  },
  delfile: function() {
    var hasDoc = false
    for (var _ in project.docs) { hasDoc = true; break }
    if (hasDoc) project.unregisterDoc(project.curDoc)
  }
}

