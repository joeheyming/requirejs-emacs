# rjs
Uses js2-mode to make Emacs interact with your RequireJS project

Features:
* Allows you to effortlessly manage a define or require block
 * Sorts/organizes paths and parameters for you
 * Allows for hooks to define rules for special files
* Provides a way to automatically add a path to your define block
* You can navigate to a file matching the variable under your cursor

# Installation
Put rjs.el inside a lisp path.
Then put this inside your .emacs file
```lisp
(require rjs.el)
```

# Customization 
* To help rjs find files under your project, you need to set rjs-require-base.
 This is used to locate files under that path.
 * Sometimes multiple paths are included: project/src/test/js, project/src/main/js
  * In this case, you should make rjs-require-base the root (project/src)
   Then you create a hook: 
```lisp
(defun formatRequireJsPath(found)
   ;; found is something like: test/js/your/path/to/jsfile   (without .js extension)
  (replace-regexp-in-string "\\(test\\|main\\)/js/" "" found)
  ;; we output your/path/to/jsfile, which will go inside your define block.
  )
(setq rjs-path-formatter 'formatRequireJsPath)
```

* If you have special requirejs-config rules, we handle these rules as aliases:
```lisp
(rjs-add-alias "jquery" "$" "path/to/jquery-<version>")
(rjs-add-alias "knockout" "ko" "path/to/knockout-<version>")
```

The first argument to rjs-add-alias is the path you want inside your define block, 
second argument is the parameter to put in the function declaration,
the third argument is the path to the actual file (under rjs-require-base)

# Key mappings
I like to define these local keys when in js2-mode:
```lisp
 (add-hook 'js2-mode-hook
           '(lambda ()
              (local-set-key [(super a) ?s ?r ] 'rjs-sort-require-paths)
              (local-set-key [(super a) ?a ?r ] 'rjs-add-to-define)
              (local-set-key [(super a) ?r ?j ] 'rjs-jump-to-module)
              ))
```
