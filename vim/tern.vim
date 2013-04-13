py << endpy

import vim, os, platform, subprocess, urllib2, json, re

def displayError(err):
  vim.command("echoerr '" + str(err) + "'")

def makeRequest(port, doc):
  req = urllib2.urlopen("http://localhost:" + str(port) + "/", json.dumps(doc), 1)
  data = req.read()
  if req.getcode() >= 300:
    raise IOError(data)
  return json.loads(data)

def projectDir():
  cur = vim.eval("b:ternProjectDir")
  if cur: return cur

  projectdir = ""
  mydir = vim.eval("expand('%:p:h')")

  if mydir:
    projectdir = mydir
    while True:
      parent = os.path.dirname(mydir[:-1])
      if not parent:
        break
      if os.path.isfile(os.path.join(mydir, ".tern-project")):
        projectdir = mydir
        break
      mydir = parent

  vim.command("let b:ternProjectDir = \"" + projectdir + "\"")
  return projectdir

def findServer(ignorePort=False):
  cur = int(vim.eval("b:ternPort"))
  if cur != 0 and cur != ignorePort: return cur

  dir = projectDir()
  if not dir: return None
  portFile = os.path.join(dir, ".tern-port")
  if os.path.isfile(portFile):
    port = int(open(portFile, "r").read())
    if port != ignorePort:
      vim.command("let b:ternPort = " + str(port))
      return port
  return startServer()

def startServer():
  win = platform.system() == "Windows"
  proc = subprocess.Popen(vim.eval("g:tern#command"), cwd=projectDir(),
                          stdout=subprocess.PIPE, shell=win)
  status = proc.stdout.readline()
  match = re.match("Listening on port (\\d+)", status)
  if match:
    port = int(match.group(1))
    vim.command("let b:ternPort = " + str(port))
    return port
  return None

def relativeFile():
  filename = vim.eval("expand('%:p')")
  return filename[len(projectDir()) + 1:]

def bufferSlice(buf, pos, end):
  text = ""
  while pos < end:
    text += buf[pos] + "\n"
    pos += 1
  return text

def fullBuffer():
  return {"type": "full",
          "name": relativeFile(),
          "text": bufferSlice(vim.current.buffer, 0, len(vim.current.buffer))};

def bufferFragment():
  curRow, curCol = vim.current.window.cursor
  line = curRow - 1
  buf = vim.current.buffer
  minIndent = None
  start = None

  for i in range(max(0, line - 50), line):
    if not re.match("\\bfunction\\b", buf[i]): continue
    indent = len(re.match("^\\s*", buf[i]).group(0))
    if minIndent is None or indent <= minIndent:
      minIndent = indent
      start = i

  if start is None: start = max(0, line - 50)
  end = min(len(buf) - 1, line + 20)
  return {"type": "part",
          "name": relativeFile(),
          "text": bufferSlice(buf, start, end),
          "offsetLines": start};

def runCommand(query, pos, mode=None):
  if isinstance(query, str): query = {"type": query}
  port = findServer()
  if not port: return
  data = None
  offset = 0
  curSeq = vim.eval("undotree()['seq_cur']")

  doc = {"query": query, "files": []}
  if curSeq == vim.eval("b:ternBufferSentAt"):
    fname, sendingFile = (relativeFile(), False)
  elif len(vim.current.buffer) > 250:
    f = bufferFragment()
    doc["files"].append(f)
    offset = f["offsetLines"]
    pos = {"line": pos["line"] - offset, "ch": pos["ch"]}
    fname, sendingFile = ("#0", False)
  else:
    doc["files"].append(fullBuffer())
    fname, sendingFile = ("#0", True)
  query["file"] = fname
  query["end"] = pos
  query["lineCharPositions"] = True

  try:
    data = makeRequest(port, doc)
  except:
    pass

  if not data:
    try:
      port = findServer(port)
      data = makeRequest(port, doc)
    except Exception as e:
      displayError(e)

  if sendingFile:
    vim.command("let b:ternBufferSentAt = " + str(curSeq))
  return (data, offset)

ternCompleteConfig = {'types' : True, 'docs': True }

def ensureCompletionCached():
  cached = vim.eval("b:ternLastCompletionPos")
  curRow, curCol = vim.current.window.cursor

  if (curRow == int(cached["row"]) and curCol >= int(cached["end"]) and
      vim.current.buffer[curRow-1][int(cached["start"]):int(cached["end"])] == cached["word"]):
    return

  data, offset = runCommand({"type" : "completions"
                            ,"types": ternCompleteConfig["types"]
                            ,"docs" : ternCompleteConfig["docs"]}
                           ,{"line": curRow - 1, "ch": curCol})
  if data is None: return

  setLast = "let b:ternLastCompletion = ["
  if ternCompleteConfig["types"] or ternCompleteConfig["docs"]:
    for cmpl in data["completions"]:
      setLast += "{ 'word' : \"" + cmpl["name"] + "\""
      if "type" in cmpl and not cmpl["type"] is None:
        setLast += ", 'menu' : " + repr(str(cmpl["type"]))
      if "doc" in cmpl and not cmpl["doc"] is None:
        setLast += ", 'info' : " + repr(str(cmpl["doc"]))
      setLast += "},"
      # TODO: string escaping of cmpl components?
  else:
    for cmpl in data["completions"]:
      setLast += "{ 'word' : \"" + cmpl + "\"},"
  vim.command(setLast + "]")
  start, end = (data["start"]["ch"], data["end"]["ch"])
  vim.command("let b:ternLastCompletionPos = {'row': " + str(curRow) +
              ", 'start': " + str(start) +
              ", 'end': " + str(end) +
              ", 'word': '" + vim.current.buffer[curRow-1][start:end] + "'}")

def ternQuery(query):
  curRow, curCol = vim.current.window.cursor
  data, offset = runCommand(query, {"line": curRow - 1, "ch": curCol})
  return data

endpy

if !exists('g:tern#command')
  let g:tern#command = ["node", expand('<sfile>:h') . '/../bin/tern']
endif

" show info in preview window
function! tern#PreviewInfo(info)
  silent! wincmd P
  if &previewwindow
    silent 1,$d
  else
    new +setlocal\ previewwindow|setlocal\ buftype=nofile|setlocal\ noswapfile
    exe "normal z" . &previewheight . "\<cr>"
  endif
  call append(0,split(a:info,"\n"))
  wincmd p
endfunction

" just print server responses
" TernRefs tends to time out..
command! TernQueryType py print(ternQuery({"type":"type"}))
command! TernQueryDoc  py print(ternQuery({"type":"documentation"}))
command! TernQueryDef  py print(ternQuery({"type":"definition"}))
command! TernQueryRefs py print(ternQuery({"type":"refs"}))

" lookup documentation and show in preview window
command! TernDoc call tern#Documentation()
function! tern#Documentation()
py <<endpy
data = ternQuery({"type":"documentation"})
if "doc" in data and not data["doc"] is None:
  vim.command("call tern#PreviewInfo(\""+data["doc"]+"\")")
else:
  vim.command("echo 'no documentation found'")
endpy
endfunction

" lookup definition and jump to/open in window
command! TernDef call tern#Definition("edit")
command! TernDefPreview call tern#Definition("pedit")
command! TernDefSplit call tern#Definition("split")
command! TernDefTab call tern#Definition("tabe")
function! tern#Definition(cmd)
py <<endpy
data = ternQuery({"type":"definition"})
if "file" in data and not data["file"] is None:
  vim.command("{0} +{1} {2}".format(vim.eval("a:cmd")
                                   ,data["start"]["line"]+1
                                   ,data["file"]))
else:
  vim.command("echo 'no definition found'")
endpy
endfunction

function! tern#Complete(findstart, complWord)
  if a:findstart
    python ensureCompletionCached()
    return b:ternLastCompletionPos['start']
  elseif b:ternLastCompletionPos['end'] - b:ternLastCompletionPos['start'] == len(a:complWord)
    return b:ternLastCompletion
  else
    let rest = []
    for entry in b:ternLastCompletion
      if stridx(entry["word"], a:complWord) == 0
        call add(rest, entry)
      endif
    endfor
    return rest
  endif
endfunction

function! tern#Enable()
  let b:ternPort = 0
  let b:ternProjectDir = ''
  let b:ternLastCompletion = []
  let b:ternLastCompletionPos = {'row': -1, 'start': 0, 'end': 0}
  let b:ternBufferSentAt = -1
  setlocal omnifunc=tern#Complete
endfunction

autocmd FileType javascript :call tern#Enable()

" FIXME String escaping in commands
