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
