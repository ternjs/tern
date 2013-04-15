py << endpy

import vim, os, platform, subprocess, urllib2, json, re

def tern_displayError(err):
  vim.command("echoerr '" + str(err) + "'")

def tern_makeRequest(port, doc):
  req = urllib2.urlopen("http://localhost:" + str(port) + "/", json.dumps(doc), 1)
  data = req.read()
  if req.getcode() >= 300:
    raise IOError(data)
  return json.loads(data)

def tern_projectDir():
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

  vim.command("let b:ternProjectDir = " + json.dumps(projectdir))
  return projectdir

def tern_findServer(ignorePort=False):
  cur = int(vim.eval("b:ternPort"))
  if cur != 0 and cur != ignorePort: return cur

  dir = tern_projectDir()
  if not dir: return None
  portFile = os.path.join(dir, ".tern-port")
  if os.path.isfile(portFile):
    port = int(open(portFile, "r").read())
    if port != ignorePort:
      vim.command("let b:ternPort = " + str(port))
      return port
  return tern_startServer()

def tern_startServer():
  win = platform.system() == "Windows"
  proc = subprocess.Popen(vim.eval("g:tern#command"), cwd=tern_projectDir(),
                          stdout=subprocess.PIPE, shell=win)
  status = proc.stdout.readline()
  match = re.match("Listening on port (\\d+)", status)
  if match:
    port = int(match.group(1))
    vim.command("let b:ternPort = " + str(port))
    return port
  return None

def tern_relativeFile():
  filename = vim.eval("expand('%:p')")
  return filename[len(tern_projectDir()) + 1:]

def tern_bufferSlice(buf, pos, end):
  text = ""
  while pos < end:
    text += buf[pos] + "\n"
    pos += 1
  return text

def tern_fullBuffer():
  return {"type": "full",
          "name": tern_relativeFile(),
          "text": tern_bufferSlice(vim.current.buffer, 0, len(vim.current.buffer))}

def tern_bufferFragment():
  curRow, curCol = vim.current.window.cursor
  line = curRow - 1
  buf = vim.current.buffer
  minIndent = None
  start = None

  for i in range(max(0, line - 50), line):
    if not re.match(".*\\bfunction\\b", buf[i]): continue
    indent = len(re.match("^\\s*", buf[i]).group(0))
    if minIndent is None or indent <= minIndent:
      minIndent = indent
      start = i

  if start is None: start = max(0, line - 50)
  end = min(len(buf) - 1, line + 20)
  return {"type": "part",
          "name": tern_relativeFile(),
          "text": tern_bufferSlice(buf, start, end),
          "offsetLines": start}

def tern_runCommand(query, pos=None, mode=None):
  if isinstance(query, str): query = {"type": query}
  if (pos is None):
    curRow, curCol = vim.current.window.cursor
    pos = {"line": curRow - 1, "ch": curCol}
  port = tern_findServer()
  if not port: return
  data = None
  curSeq = vim.eval("undotree()['seq_cur']")

  doc = {"query": query, "files": []}
  if curSeq == vim.eval("b:ternBufferSentAt"):
    fname, sendingFile = (tern_relativeFile(), False)
  elif len(vim.current.buffer) > 250:
    f = tern_bufferFragment()
    doc["files"].append(f)
    pos = {"line": pos["line"] - f["offsetLines"], "ch": pos["ch"]}
    fname, sendingFile = ("#0", False)
  else:
    doc["files"].append(tern_fullBuffer())
    fname, sendingFile = ("#0", True)
  query["file"] = fname
  query["end"] = pos
  query["lineCharPositions"] = True

  try:
    data = tern_makeRequest(port, doc)
  except:
    pass

  if not data:
    try:
      port = tern_findServer(port)
      data = tern_makeRequest(port, doc)
    except Exception as e:
      tern_displayError(e)

  if sendingFile:
    vim.command("let b:ternBufferSentAt = " + str(curSeq))
  return data

def tern_sendBuffer():
  port = tern_findServer()
  if not port: return False
  try:
    tern_makeRequest(port, {"files": [tern_fullBuffer()]})
    return True
  except:
    return False

def tern_sendBufferIfDirty():
  curSeq = vim.eval("undotree()['seq_cur']")
  if curSeq > vim.eval("b:ternBufferSentAt") and tern_sendBuffer():
    vim.command("let b:ternBufferSentAt = " + str(curSeq))

def tern_asCompletionIcon(type):
  if type is None or type == "?": return "(?)"
  if type.startswith("fn("): return "(fn)"
  if type.startswith("["): return "([])"
  if type == "number": return "(num)"
  if type == "string": return "(str)"
  if type == "bool": return "(bool)"
  return "(obj)"

def tern_ensureCompletionCached():
  cached = vim.eval("b:ternLastCompletionPos")
  curRow, curCol = vim.current.window.cursor
  curLine = vim.current.buffer[curRow - 1]

  if (curRow == int(cached["row"]) and curCol >= int(cached["end"]) and
      curLine[int(cached["start"]):int(cached["end"])] == cached["word"] and
      (not re.match(".*\\W", curLine[int(cached["end"]):curCol]))):
    return

  data = tern_runCommand({"type": "completions", "types": True, "docs": True},
                         {"line": curRow - 1, "ch": curCol})
  if data is None: return

  completions = []
  for rec in data["completions"]:
    completions.append({"word": rec["name"],
                        "menu": tern_asCompletionIcon(rec.get("type")),
                        "info": rec.get("doc", "")})
  vim.command("let b:ternLastCompletion = " + json.dumps(completions))
  start, end = (data["start"]["ch"], data["end"]["ch"])
  vim.command("let b:ternLastCompletionPos = " + json.dumps({
    "row": curRow,
    "start": start,
    "end": end,
    "word": curLine[start:end]
  }))

def tern_lookupDocumentation():
  data = tern_runCommand("documentation")
  if data is None: return

  doc = data.get("doc")
  url = data.get("url")
  if url:
    doc = ((doc and doc + "\n\n") or "") + "See " + url
  if doc:
    vim.command("call tern#PreviewInfo(" + json.dumps(doc) + ")")
  else:
    vim.command("echo 'no documentation found'")

def tern_lookupType():
  data = tern_runCommand("type")
  if data: vim.command("echo " + json.dumps(data.get("type", "not found")))

def tern_lookupDefinition(cmd):
  data = tern_runCommand("definition")
  if data is None: return

  if "file" in data:
    vim.command(cmd + " +" + str(data["start"]["line"] + 1) + " " + data["file"])
  elif "url" in data:
    vim.command("echo " + json.dumps("see " + data["url"]))
  else:
    vim.command("echo 'no definition found'")

endpy

if !exists('g:tern#command')
  let g:tern#command = ["node", expand('<sfile>:h') . '/../bin/tern']
endif

function! tern#PreviewInfo(info)
  pclose
  new +setlocal\ previewwindow|setlocal\ buftype=nofile|setlocal\ noswapfile
  exe "normal z" . &previewheight . "\<cr>"
  call append(0, split(a:info, "\n"))
  wincmd p
endfunction

function! tern#Complete(findstart, complWord)
  if a:findstart
    python tern_ensureCompletionCached()
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

command! TernDoc py tern_lookupDocumentation()
command! TernType py tern_lookupType()
command! TernDef py tern_lookupDefinition("edit")
command! TernDefPreview py tern_lookupDefinition("pedit")
command! TernDefSplit py tern_lookupDefinition("split")
command! TernDefTab py tern_lookupDefinition("tabe")

function! tern#Enable()
  let b:ternPort = 0
  let b:ternProjectDir = ''
  let b:ternLastCompletion = []
  let b:ternLastCompletionPos = {'row': -1, 'start': 0, 'end': 0}
  let b:ternBufferSentAt = -1
  setlocal omnifunc=tern#Complete
endfunction

autocmd FileType javascript :call tern#Enable()
autocmd BufLeave *.js :py tern_sendBufferIfDirty()
