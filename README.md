# Tern

This is [Tern][1]. Tern is a stand-alone, editor-independent
JavaScript analyzer that can be used to improve the JavaScript
integration of existing editors. It currently comes only with an
[online demo][1], but I am hard at work on Emacs and Vim plug-ins.

Thanks to a group of generous [crowd funders][2], Tern is open-source
software, under an MIT license.

[1]: http://ternjs.net
[2]: http://www.indiegogo.com/projects/tern-intelligent-javascript-editing

(Disclaimer: This is a work in progress. The interface will change,
and it has bugs. But it just might occasionally work.)

## Setting up the Emacs mode

 1. Make sure you are using Emacs 24 or later. The Tern mode requires
    lexical scoping.

 1. Clone this repository somewhere. Do `npm install` to get the
    dependencies.

 1. Make Emacs aware of `emacs/tern.el`. For example by adding this to
    your `.emacs` file:

        (add-to-list 'load-path "/path/to/tern/emacs/")
        (autoload 'tern-mode "tern.el" nil t)

 1. Optionally set `tern-mode` to be automatically enabled for your
    JavaScript mode of choice. Here's the snippet for `js-mode`:

        (add-hook 'js-mode-hook (lambda () (tern-mode t)))

Buffers in `tern-mode` add a `completion-at-point` function that
activates Tern's completion. So, unless you rebound the command,
`M-tab` will trigger completion.

When the point is in an argument list, Tern will show argument names
and types at the bottom of the screen.

The following keys are bound:

 * `M-.` to jump to the definition of the thing under the cursor.
 * `M-,` brings you back to last place you were when you pressed `M-.`.
 * `C-c C-r` to rename the variable under the cursor.
 * `C-c C-c` to find the type of the thing under the cursor.
 * `C-c C-d` to find docs of the thing under the cursor. Press again to
   open the associated URL (if any).

## Setting up the Vim plugin

 1. Clone this repository somewhere. Do `npm install` to get the
    dependencies.

 1. Make sure your Vim has Python support.

 1. Source the `vim/tern.vim` script in your `.vimrc`. Don't copy it
    anywhere, leave it in the Tern checkout so that it can find the
    Tern server code.
    
        so /path/to/tern/vim/tern.vim

When editing a JavaScript (`.js`) file, your omni-complete will be
hooked into Tern. Use `C-x C-o` in insert mode to activate it.

## Configuring Tern

The Tern server used by the Emacs and Vim plugins will look for a
`.tern-project` file in the current directory or any of its parents,
to determine the scope of the project. If it doesn't find one, it'll
assume the local directory is the scope, and use its default settings.

A `.tern-project` file is a JSON file in a format like this:

    {
      "libs": [
        "browser",
        "jquery"
      ],
      "loadEagerly": [
        "importantfile.js"
      ],
      "plugins": {
        "requirejs: {
          "baseURL": "./",
          "paths": {}
        }
      }
    }

The `libs` property refers to the JSON type descriptions that should
be loaded into the environment for this project. See the `defs/`
directory for examples. The strings given here will be suffixed with
`.json`, and searched for first in the project's own dir, and then in
the `defs/` directory of the Tern distribution.

By default, local files are loaded into the Tern server when they are
opened by the editor. `loadEagerly` allows you to force some files to
always be loaded.

Finally, `plugins` is an object used to load and configure Tern
plugins. The names of the properties refer to files (suffixed with
`.js`) or directories, either in the project dir or under `plugin/` in
the Tern directory. Their values are configuration objects that will
be passed to the plugins. You can leave them at `{}` when you don't
need to pass any options.

## Generating definitions

Tern includes two utilities that can help generate `.json` definition
files. `bin/condense` simply loads code into Tern, and then tries to
dump out the types it found. It takes a list of files to load, and a
few switches (`--plugin`, `--def`) to load plugins and other
definition files. Here's how you might get a definition of the Acorn
parser library:

    bin/condense --plugin node node_modules/acorn/acorn.js

To get accurate results, it often helps to load in other files, either
things that the library you're trying to condense depends on, or files
that use the library (such as its test suite). This 'feeds' in more
types, for example by showing Tern calls to the functions the library
defines, which will help come up with types for their arguments.

Pass files prefixed by a `+` to `bin/condense` in order to load them
in, but not actually include their types in the output. For example:

    bin/condense --plugin node lib/infer.js +lib/tern.js

The second way to derive definition files it to take a [Typescript]
`.d.ts` file (as, for example, found in the [DefinitelyTyped]
repository on github), and feed it to `bin/from_ts`. You can pass it a
filename or a URL, and it will do its best to parse it and transform
the definitions it finds into a `.json` definition file.

[Typescript]: http://www.typescriptlang.org/
[DefinitelyTyped]: https://github.com/borisyankov/DefinitelyTyped

## Programming API

(*Proper* docs will be written soon, after the code stabilizes.)

There are two modules in the library that are useful for external
code. Firstly, `lib/infer.js` is the inference engine. This can be
used in a stand-alone way. For example:

```javascript
var infer = require("tern/lib/infer"), fs = require("fs");
var definitionsToInclude = [
  JSON.parse(fs.readFileSync("/path/to/ecma5.json", "utf8"))
];

var context = new infer.Context(definitionsToInclude);
infer.withContext(context, function() {
  var ast = infer.parse(fs.readFileSync("/path/to/script.js", "utf8"));
  infer.analyze(ast, "script.js");
  var globalVar = context.topScope.getProp("myvar");
  console.log(infer.toString(globalVar.getType()));
});
```

The `infer` module provide various functions for finding AST nodes at
a given position, enumerating the properties of a type, the variables
in a given scope, etcetera.

Built on top of it is `lib/tern.js`, which implements a server that
performs analysis on a set of files (preferably in the background),
and answers queries about these files. The Tern server (`bin/tern`) is
a thin HTTP wrapper around this.

Here's a crude example (see `demo/demo.js` for a more refined one):

```javascript
var tern = require("tern/lib/tern");

var server = new tern.Server({});
server.addFile("foo.js", "function foo() { return 'foo'; }");

var query = {type: "definition",
             file: "bar.js",
             end: 15};
var includedFile = {type: "full",
                    name: "bar.js",
                    text: "console.log(foo());"};
server.request({query: query, files: [includedFile]}, function(err, data) {
  console.log(data.file + " character " + data.start);
});
```

This will load a file `foo.js` into the server, and then make a
request asking for the definition of the variable at character 15 in
file `bar.js`, which is included in the request.

This architecture is aimed at making is easy for an editor to
communicate with a Tern server, sending updated versions of files
along with its request as the files are being edited. There is also
support for sending partial files, so that when you're editing a
3000-line file, Tern analyzes the whole thing once (in the
background), and then when you need information near line 2500, only
some nearby code is sent over, and combined with information from the
earlier full run, to quickly get you a response.
