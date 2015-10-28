var project

function Project(name, place, config, docs) {
  this.name = name
  this.docs = Object.create(null)
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
  this.curDoc = firstDoc
  this.setSelectedTab(firstDoc)

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
  var cmds = document.getElementById("commands")
  CodeMirror.on(cmds, "change", function() {
    if (!project || cmds.selectedIndex == 0) return
    var found = commands[cmds.value]
    cmds.selectedIndex = 0
    project.editor.focus()
    if (found) found()
  })

  var projects = document.getElementById("projects")
  var projectNames = []
  var projectTags = document.querySelectorAll("project")
  for (var i = 0; i < projectTags.length; i++) {
    var opt = projects.appendChild(document.createElement("option"))
    projectNames.push(opt.textContent = projectTags[i].id)
  }
  CodeMirror.on(projects, "change", function() {
    if (projects.selectedIndex != 0)
      initProject(projects.value, function() {
        projects.selectedIndex = 0
        project.editor.focus()
      })
  })
  function updateFromHash() {
    var name = location.hash.slice(1)
    if (projectNames.indexOf(name) > -1 &&
        (!project || project.name != name)) {
      initProject(name)
      return true
    }
  }
  CodeMirror.on(window, "hashchange", updateFromHash)

  updateFromHash() || initProject("simple")
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

var defsLoaded = Object.create(null)
function loadDefs(defs, c) {
  var result = [], loaded = 0
  for (var i = 0; i < defs.length; ++i) (function(i) {
    var name = defs[i]
    if (defsLoaded[name]) {
      result[i] = defsLoaded[name]
      if (++loaded == defs.length) c(result)
    } else {
      load("../../defs/" + name + ".json", function(json) {
        defsLoaded[name] = result[i] = JSON.parse(json)
        if (++loaded == defs.length) c(result)
      })
    }
  })(i)
}

function words(str) {
  return str ? str.split(" ") : []
}

function initProject(name, c) {
  var node = document.getElementById(name)
  loadDefs(words(node.getAttribute("data-libs")), function(defs) {
    var plugins = {}
    words(node.getAttribute("data-plugins")).forEach(function(name) { plugins[name] = true })

    if (plugins.requirejs) plugins.requirejs = {override: {"jquery": "=$"}}

    loadFiles(node, function(files) {
      var place = document.getElementById("place")
      place.textContent = ""

      if (project) project.server.destroy()

      project = new Project(name, place, {
        defs: defs,
        plugins: plugins
      }, files)
      location.hash = "#" + name
      c && c()
    })
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
