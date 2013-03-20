# Tern

This is [Tern][1]. Tern is a stand-alone, editor-independent
JavaScript analyzer that can be used to improve the JavaScript
integration of existing editors. It currently comes only with an
[online demo][1], but I am hard at work on Emacs and Vim plug-ins.

Thanks to a group of generous [crowd funders][2], Tern is open-source
software, under an MIT license.

[1]: http://ternjs.net
[2]: http://www.indiegogo.com/projects/tern-intelligent-javascript-editing

## Setting up the Emacs mode

(Disclaimer: This is a work in progress. The interface will change,
and it has bugs. But it just might occasionally work.)

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
