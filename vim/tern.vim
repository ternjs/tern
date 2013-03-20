py << endpy

import vim, os, subprocess, urllib2, json, re

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

ternCommand = ["tern"]

def startServer():
  proc = subprocess.Popen(ternCommand, cwd=projectDir(), stdout=subprocess.PIPE)
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

def pointAtStart(row):
  buf = vim.current.buffer
  pos = 0
  cur = 1
  while cur < row:
    pos += len(buf[cur - 1]) + 1
    cur = cur + 1
  return pos

def point():
  (row, col) = vim.current.window.cursor
  return col + pointAtStart(row)

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
          "offset": pointAtStart(start + 1)};

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
    offset = f["offset"]
    fname, sendingFile = ("#0", False)
  else:
    doc["files"].append(fullBuffer())
    fname, sendingFile = ("#0", True)
  query["file"] = fname
  query["end"] = pos - offset

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

def ensureCompletionCached():
  cached = vim.eval("b:ternLastCompletionPos")
  curRow, curCol = vim.current.window.cursor

  if (curRow == int(cached["row"]) and curCol >= int(cached["end"]) and
      vim.current.buffer[curRow-1][int(cached["start"]):int(cached["end"])] == cached["word"]):
    return

  data, offset = runCommand("completions", point())
  if data is None: return

  setLast = "let b:ternLastCompletion = ["
  for cmpl in data["completions"]:
    setLast += "\"" + cmpl + "\","
  vim.command(setLast + "]")
  lineStart = pointAtStart(curRow)
  start = (data["start"] + offset) - lineStart
  end = (data["end"] + offset) - lineStart
  vim.command("let b:ternLastCompletionPos = {'row': " + str(curRow) +
              ", 'start': " + str(start) +
              ", 'end': " + str(end) +
              ", 'word': '" + vim.current.buffer[curRow-1][start:end] + "'}")

endpy

function! tern#Complete(findstart, complWord)
  if a:findstart
    python ensureCompletionCached()
    return b:ternLastCompletionPos['start']
  elseif b:ternLastCompletionPos['end'] - b:ternLastCompletionPos['start'] == len(a:complWord)
    return b:ternLastCompletion
  else
    let rest = []
    for word in b:ternLastCompletion
      if stridx(word, a:complWord) == 0
        call add(rest, word)
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
  set omnifunc=tern#Complete
endfunction

autocmd FileType javascript :call tern#Enable()

" FIXME String escaping in commands
