import sublime, sublime_plugin
import os, platform, subprocess, urllib2, webbrowser, json, re, select, time

windows = platform.system() == "Windows"
settings = sublime.load_settings("Preferences.sublime-settings")

def is_js_file(view):
  return view.score_selector(0, "source.js") > 0

files = {}

arghints_enabled = settings.get("tern_argument_hints", True)

class Listeners(sublime_plugin.EventListener):
  def on_close(self, view):
    files.pop(view.file_name(), None)

  def on_deactivated(self, view):
    pfile = files.get(view.file_name(), None)
    if pfile and pfile.dirty:
      send_buffer(pfile, view)

  def on_modified(self, view):
    pfile = files.get(view.file_name(), None)
    if pfile: pfile.modified(view)

  def on_selection_modified(self, view):
    if not is_js_file(view) or not arghints_enabled: return
    pfile = get_pfile(view)
    if pfile is not None: show_argument_hints(pfile, view)

  def on_query_completions(self, view, prefix, _locations):
    if not is_js_file(view): return
    pfile = get_pfile(view)
    if pfile is None: return (None, False)

    completions, fresh = ensure_completions_cached(pfile, view)
    if completions is None: return None

    if not fresh:
      completions = [c for c in completions if c[1].startswith(prefix)]
    return completions

class ProjectFile(object):
  def __init__(self, name, view, project):
    self.project = project
    self.name = name
    self.dirty = view.is_dirty()
    self.cached_completions = None
    self.cached_arguments = None

  def modified(self, view):
    self.dirty = True
    if self.cached_completions and view.sel()[0].a < self.cached_completions[0]:
      self.cached_completions = None
    if self.cached_arguments and view.sel()[0].a < self.cached_arguments[0]:
      self.cached_arguments = None

class Project(object):
  def __init__(self, dir):
    self.dir = dir
    self.port = None

def get_pfile(view):
  fname = view.file_name()
  if fname is None: return None
  if files.has_key(fname): return files[fname]

  pdir = project_dir(fname)
  if pdir is None: return None

  project = None
  for f in files.values():
    if f.project.dir == pdir:
      project = p
      break
  pfile = files[fname] = ProjectFile(fname, view, project or Project(pdir))
  return pfile

def project_dir(fname):
  dir = os.path.dirname(fname)
  if not os.path.isdir(dir): return None

  cur = dir
  while True:
    parent = os.path.dirname(cur[:-1])
    if not parent:
      break
    if os.path.isfile(os.path.join(cur, ".tern-project")):
      return cur
    cur = parent
  return dir

def server_port(project, ignored=None):
  if project.port is not None and project.port != ignored:
    return (project, True)

  port_file = os.path.join(project.dir, ".tern-port")
  if os.path.isfile(port_file):
    port = int(open(port_file, "r").read())
    if port != ignored:
      return (port, True)

  started = start_server(project)
  if started is not None:
    project.port = started
  return (started, False)

plugin_dir = os.path.abspath(os.path.dirname(__file__))
tern_command = settings.get("tern_command", None) or ["node",  os.path.join(plugin_dir, "../bin/tern")]

def start_server(project):
  proc = subprocess.Popen(tern_command, cwd=project.dir,
                          stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=windows)
  output = ""

  while True:
    line = proc.stdout.readline()
    if not line:
      sublime.error_message("Failed to start server" + (output and ":\n" + output))
      return None
    match = re.match("Listening on port (\\d+)", line)
    if match:
      return int(match.group(1))
    else:
      output += line

def relative_file(pfile):
  return pfile.name[len(pfile.project.dir) + 1:]

def buffer_fragment(view, pos):
  cur = start = view.line(max(0, pos - 1000)).a
  min_indent = 10000
  while True:
    next = view.find("\\bfunction\\b", cur)
    if next is None or next.b > pos: break
    line = view.line(next.a)
    indent = count_indentation(view.substr(line))
    if indent < min_indent:
      min_indent = indent
      start = line.a
    cur = line.b
  return sublime.Region(start, min(pos + 500, view.size()))

def count_indentation(line):
  count, pos = (0, 0)
  while pos < len(line):
    ch = line[pos]
    if ch == " ": count += 1
    elif ch == "\t": count += 4
    else: break
    pos += 1
  return count

def make_request(port, doc):
  try:
    req = urllib2.urlopen("http://localhost:" + str(port) + "/", json.dumps(doc), 1)
    return json.loads(req.read())
  except urllib2.HTTPError, error:
    sublime.error_message(error.read())
    return None

def run_command(view, query, pos=None, fragments=True):
  if isinstance(query, str): query = {"type": query}
  if (pos is None): pos = view.sel()[0].b

  pfile = get_pfile(view)
  if pfile is None: return
  port, port_is_old = server_port(pfile.project)
  if port is None: return

  doc = {"query": query, "files": []}

  if not pfile.dirty:
    fname, sending_file = (relative_file(pfile), False)
  if fragments and view.size() > 8000:
    region = buffer_fragment(view, pos)
    doc["files"].append({"type": "part",
                         "name": relative_file(pfile),
                         "offset": region.a,
                         "text": view.substr(region)})
    pos -= region.a
    fname, sending_file = ("#0", False)
  else:
    doc["files"].append({"type": "full",
                         "name": relative_file(pfile),
                         "text": view.substr(sublime.Region(0, view.size()))})
    fname, sending_file = ("#0", True)
  query["file"] = fname
  query["end"] = pos

  data = None
  try:
    data = make_request(port, doc)
    if data is None: return None
  except:
    pass

  if data is None and port_is_old:
    try:
      port = server_port(pfile.project, port)[0]
      if port is None: return
      data = make_request(port, doc)
      if data is None: return None
    except Exception as e:
      sublime.error_message(str(e))

  if sending_file: pfile.dirty = False
  return data

def send_buffer(pfile, view):
  port = server_port(pfile.project)[0]
  if port is None: return False
  try:
    make_request(port, {"files": [{"type": "full",
                                   "name": relative_file(pfile),
                                   "text": view.substr(sublime.Region(0, view.size()))}]})
    pfile.dirty = False
    return True
  except:
    return False

def completion_icon(type):
  if type is None or type == "?": return " (?)"
  if type.startswith("fn("): return " (fn)"
  if type.startswith("["): return " ([])"
  if type == "number": return " (num)"
  if type == "string": return " (str)"
  if type == "bool": return " (bool)"
  return " (obj)"

def ensure_completions_cached(pfile, view):
  pos = view.sel()[0].b
  if pfile.cached_completions is not None:
    c_start, c_word, c_completions = pfile.cached_completions
    if c_start <= pos:
      slice = view.substr(sublime.Region(c_start, pos))
      if slice.startswith(c_word) and not re.match(".*\\W", slice):
        return (c_completions, False)

  data = run_command(view, {"type": "completions", "types": True})
  if data is None: return (None, False)

  completions = []
  for rec in data["completions"]:
    completions.append((rec.get("name") + completion_icon(rec.get("type", None)), rec.get("name")))
  pfile.cached_completions = (data["start"], view.substr(sublime.Region(data["start"], pos)), completions)
  return (completions, True)

def locate_call(view):
  sel = view.sel()[0]
  if sel.a != sel.b: return (None, 0)
  context = view.substr(sublime.Region(max(0, sel.b - 500), sel.b))
  pos = len(context)
  depth = argpos = 0
  while pos > 0:
    pos -= 1
    ch = context[pos]
    if ch == "}" or ch == ")" or ch == "]":
      depth += 1
    elif ch == "{" or ch == "(" or ch == "[":
      if depth > 0: depth -= 1
      elif ch == "(": return (pos + sel.b - len(context), argpos)
      else: return (None, 0)
    elif ch == "," and depth == 0:
      argpos += 1
  return (None, 0)

def show_argument_hints(pfile, view):
  call_start, argpos = locate_call(view)
  if call_start is None: return
  if pfile.cached_arguments is not None and pfile.cached_arguments[0] == call_start:
    return render_argument_hints(pfile.cached_arguments[1], argpos)

  data = run_command(view, {"type": "type", "preferFunction": True}, call_start)
  if data is None: return

  parsed = parse_function_type(data)
  if parsed is None: return
  pfile.cached_arguments = (call_start, parsed)
  render_argument_hints(parsed, argpos)

def parse_function_type(data):
  type = data["type"]
  if not re.match("fn\\(", type): return None
  pos = 3
  args, retval = ([], None)
  while pos < len(type) and type[pos] != ")":
    colon = type.find(":", pos)
    name = "?"
    if colon != -1:
      name = type[pos:colon]
      if not re.match("[\\w_$]+$", name): name = "?"
      else: pos = colon + 2
    type_start = pos
    depth = 0
    while pos < len(type):
      ch = type[pos]
      if ch == "(" or ch == "[" or ch == "{":
        depth += 1
      elif ch == ")" or ch == "]" or ch == "}":
        if depth > 0: depth -= 1
        else: break
      elif ch == "," and depth == 0:
        break
      pos += 1
    args.append((name, type[type_start:pos]))
    if type[pos] == ",": pos += 2
  if type[pos:pos + 5] == ") -> ":
    retval = type[pos + 5:]
  return {"name": data.get("exprName", None) or data.get("name", None) or "fn",
          "args": args,
          "retval": retval}

def render_argument_hints(ftype, argpos):
  msg = ftype["name"] + "("
  i = 0
  for name, type in ftype["args"]:
    if i > 0: msg += ", "
    if i == argpos: msg += "*"
    msg += name + ("" if type == "?" else ": " + type)
    i += 1
  msg += ")"
  if ftype["retval"] is not None:
    msg += " -> " + ftype["retval"]
  sublime.status_message(msg)
