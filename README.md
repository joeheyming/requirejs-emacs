# requirejs.el [![MELPA](https://melpa.org/packages/requirejs-badge.svg)](https://melpa.org/#/requirejs) [![Build Status](https://secure.travis-ci.org/joeheyming/requirejs-emacs.png)](http://travis-ci.org/joeheyming/requirejs-emacs)

Uses js2-mode to make Emacs interact with your RequireJS project.
Provides requirejs-mode which is a minor mode for js2-mode.

## Features:
* Allows you to effortlessly manage a define or require block
 * Sorts/organizes paths and parameters for you
 * Allows for hooks to define rules for special files
* Provides a way to automatically add a path to your define block
* You can navigate to a file matching the variable under your cursor

## Screenshot

![requirejs-emacs](requirejs.gif)

## Installation

`requirejs-emacs` is available on [MELPA](https://melpa.org).

You can install `requirejs` with the following command.

<kbd>M-x package-install [RET] requirejs [RET]</kbd>

## Customization 
* To help requirejs find files under your project, you need to set requirejs-require-base.
 This is used to locate files under that path.
 * Sometimes multiple paths are included: project/src/test/js, project/src/main/js
  * In this case, you should make requirejs-require-base the root (project/src)
   Then you create a hook: 
```lisp
(defun formatRequireJsPath(found)
   ;; found is something like: test/js/your/path/to/jsfile   (without .js extension)
  (replace-regexp-in-string "\\(test\\|main\\)/js/" "" found)
  ;; we output your/path/to/jsfile, which will go inside your define block.
  )
(setq requirejs-path-formatter 'formatRequireJsPath)
```

* If you have special requirejs-config rules, we handle these rules as aliases:
```lisp
(requirejs-add-alias "jquery" "$" "path/to/jquery-<version>")
(requirejs-add-alias "knockout" "ko" "path/to/knockout-<version>")
```

The first argument to requirejs-add-alias is the path you want inside your define block, 
second argument is the parameter to put in the function declaration,
the third argument is the path to the actual file (under requirejs-require-base)

## Key mappings
I like to add requirejs-mode when in js2-mode:
```lisp
 (add-hook 'js2-mode-hook
           '(lambda ()
              (requirejs-mode)
              ))
```

This provides the following keys:
C-c s r => requirejs-sort-require-paths
C-c a r => requirejs-add-to-define
C-c r j => requirejs-jump-to-module

## Snippets

When in requirejs-mode, we will expand ```def``` as a  ```define([], function(){})``` block.

You can customize this to add a header by overriding this variable with a function: ```requirejs-define-header-hook```
