;;; rjs.el --- Requirejs import manipulation and source traversal.

;; Author: Joe Heyming <joeheyming@gmail.com>
;; Version: 1.1
;; Keywords: javascript, requirejs
;; Package-Requires: ((js2-mode "20141118")(expand-region "0.10.0")(popup "20150116.1223"))

;;; Commentary:
;; This module allows you to:
;;  - Sort a define block
;;   - define rules about how to sort shims/aliases
;;  - Helps you add new define paths to a define block
;;   - which uses the above sorting functionality when any arbitrary line is added to the path array.
;;  - Allows you to jump to a module under your cursor as long as it exists in the same requirejs project.

;; installation: put this file under an emacs lisp directory, then include this file (require 'rjs)
;;  Usage:
;; Here is a sample configuration that may be helpful to get going.
;; 
;; (setq rjs-require-base "~/path/to/your/project")
;; (rjs-add-alias "jquery" "$" "path/to/jquery-<version>.js")
;;
;; (add-hook 'js2-mode-hook
;;           '(lambda ()
;;              (local-set-key [(super a) ?s ?r ] 'rjs-sort-require-paths)
;;              (local-set-key [(super a) ?a ?r ] 'rjs-add-to-define)
;;              (local-set-key [(super a) ?r ?j ] 'rjs-jump-to-module)
;;              ))

(provide 'rjs)
(require 'cl)
(require 'popup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js2-mode utilities
;;  These utilities use the js2-mode abstract syntax tree to do useful operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun js2-node-quoted-contents (node)
  "Get the inner text of a quoted string. e.g 'foo' -> yeilds foo"
  (let ((node-pos (js2-node-abs-pos node)))
    (buffer-substring-no-properties (+ 1 node-pos) (- (+ node-pos (js2-node-len node)) 1))))

(defun js2-goto-node (fn)
  (let* ( (node (js2-node-at-point))
          (child-node (funcall fn node)) )
    (if child-node
        (goto-char (js2-node-abs-pos child-node)))
    child-node)
  )

(defun js2-goto-first-child-node()
  "Goes to the first child, returns that node, if any."
  (interactive)
  (js2-goto-node 'js2-node-first-child))

(defun js2-goto-last-child-node()
  "Goes to the first child, returns that node, if any."
  (interactive)
  (js2-goto-node 'js2-node-last-child))

(defun js2-goto-parent-node()
  "Goes to your parent node, returns that node."
  (interactive)
  (let ((curnode (js2-node-at-point))
        (node (js2-goto-node 'js2-node-parent)))
    (if (eq (js2-node-abs-pos curnode) (js2-node-abs-pos node))
        (progn
          (setq node (js2-node-at-point))
          (goto-char (- (js2-node-abs-pos node) 1))
          (setq node (js2-node-at-point))
          (goto-char (js2-node-abs-pos node))
          node) node)))

(defun js2-goto-next-node()
  "Goes to the next sibling, if any.  Returns the sibling"
  (interactive)
  (js2-goto-node 'js2-node-next-sibling))

(defun js2-goto-prev-node()
  "Goes to the previous sibling, if any.  Returns the sibling"
  (interactive)
  (js2-goto-node 'js2-node-prev-sibling))

(defun js2-goto-nth-child(n)
  "Goes to the nth child, if any.  Returns the child"
  (interactive (list (string-to-number (read-string "Goto child: "))))
  (let* ((current (js2-node-at-point))
         (parent (js2-node-parent current))
         (child (nth n (js2-node-child-list parent))))
    (if child (goto-char (js2-node-abs-pos child))) child))

;; The following functions were shamelessly stolen from https://github.com/ScottyB/ac-js2
(defun js2-root-or-node ()
  "Return the current node or js2-ast-root node."
  (let ((node (js2-node-at-point)))
    (if (js2-ast-root-p node)
        node
      (js2-node-get-enclosing-scope node))))

(defun js2-get-function-name (fn-node)
  "Return the name of the function FN-NODE.
Value may be either function name or the variable name that holds
the function."
  (let ((parent (js2-node-parent fn-node)))
    (if (js2-function-node-p fn-node)
        (or (js2-function-name fn-node)
            (if (js2-var-init-node-p parent)
                (js2-name-node-name (js2-var-init-node-target parent)))))))

(defun js2-get-function-node (name scope)
  "Return node of function named NAME in SCOPE."
  (catch 'function-found
    (js2-visit-ast
     scope
     (lambda (node end-p)
       (when (and (not end-p)
                  (string= name (js2-get-function-name node)))
         (throw 'function-found node))
       t))
    nil))

(defun js2-get-function-call-node (name scope)
  "Return node of function named NAME in SCOPE."
  (catch 'function-found
    (js2-visit-ast
     scope
     (lambda (node end-p)
       (when (and (not end-p)
                  (= (js2-node-type node) js2-NAME)
                  (string= name (js2-name-node-name node))
                  (= (js2-node-type (js2-node-parent node)) js2-CALL)
                  )
         (throw 'function-found node))
       t))
    nil))

(defun js2-name-declaration (name)
  "Return the declaration node for node named NAME."
  (let* ((node (js2-root-or-node))
         (scope-def (js2-get-defining-scope node name))
         (scope (if scope-def (js2-scope-get-symbol scope-def name) nil))
         (symbol (if scope (js2-symbol-ast-node scope) nil)))
    (if (not symbol)
        (js2-get-function-node name scope-def)
      symbol)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rjs-define-or-require "define" "`rjs-define-or-require' Stores weather your project uses require([], function(){}) style imports or define([], function() {})")

(defun rjs-goto-define()
  "Jump to the define at the beginning of the buffer"
  (interactive)
  (let* ( (define-node (js2-get-function-call-node rjs-define-or-require (js2-node-root (js2-node-at-point)) ))
          (define-call (js2-node-parent define-node)) )
    (goto-char (+ (js2-node-abs-pos define-call) (js2-call-node-lp define-call) ))
    (search-forward "[")
    (backward-char 1)))

(defun rjs-get-require-paths()
  "Gets all the require paths and returns them without quotes."
  (interactive)
  (save-excursion
    (rjs-goto-define)
    (let ((node (js2-goto-first-child-node))
          (require-paths '()))
      (while (not (null node))
        (push (js2-node-quoted-contents node) require-paths)
        (setq node (js2-node-next-sibling node)))
      require-paths
      )))

(defun rjs-clear-list()
  "Look at your parent node and clear out all child nodes"
  (interactive)
  (er/mark-outside-pairs)
  (kill-region (+ 1(region-beginning)) (- (region-end) 1) )
  (forward-char 1))

;; Define a structure for storing an alias
(defstruct rjs-alias
  "A rule for a special shim in your requirejs conf"
  label ;; The lookup value of an alias
  variableName ;; The desired variable name to place in a define function.
  path) ;; The path that we will sort by.


(defvar rjs-aliases (make-hash-table :test 'equal)
  "`rjs-aliases' is a table that stores user defined aliases.
An alias is added with `rjs-add-alias'")

(defvar rjs-alias-var-lookup (make-hash-table :test 'equal)
  "`rjs-alias-var-lookup' Defines a reverse lookup of variable name to rjs alias")

(defun rjs-add-alias (label variableName path)
  "Adds a requirejs config shim to `rjs-aliases'.

Example 1:
 If you want to expose that lookup: knockout, but name the variable ko:
 (rjs-add-alias \"knockout\" \"ko\" \"knockout\")
 This says that when you sort require paths, we will interpret knockout as the variable ko:
 define(['knockout'], function(ko) { ... }
 The third string is used to determine how to sort

Example 2: if you include lodash, but you want to sort by 'Lodash', not '_':
 (rjs-add-alias \"_\", \"_\" \"lodash\")
 This will make sure the define block looks like this:
 define(['knockout', '_'], function(ko, _) { ... }  instead of this:
 define(['_', 'knockout'], function(_, ko) { ... }  because K comes before L"
  (interactive)
  (let ((alias (make-rjs-alias :label label :variableName variableName :path path)))
    (puthash label alias rjs-aliases)
    (puthash variableName alias rjs-alias-var-lookup)
    ))

(defun rjs-path-compare(a b)
  "Compare paths ignore case"
  (string< (downcase a) (downcase b)))

(defun rjs-alias-compare(a b)
  "Compare paths ignore case"
  (let* ((alias-a (gethash a rjs-aliases))
         (alias-b (gethash b rjs-aliases))
         (shim-a (if alias-a (rjs-alias-path alias-a) a))
         (shim-b (if alias-b (rjs-alias-path alias-b) b)))
    (string< shim-a shim-b)))


(defvar rjs-var-formatter '(lambda (path basename) nil)
  "Override `rjs-var-formatter' to add special rules for variable formatting.
If this returns nil, then we use the basename of the path as the default variable name.
If `rjs-var-formatter' returns a string, then we will use that string for the variable name.")

(defvar rjs-text-suffix "TextStr" "Default behavior of text! paths is to take the basename of the path file
and tack on `rjs-text-suffix'.  Override this if TextStr doesn't work for you.")

(defun rjs-get-variable-name(item)
  (interactive)
  (let* ((path (car item))
         (basename (car (last (split-string path "/" )) ))
         (shim (gethash path rjs-aliases))
         (formatted (funcall rjs-var-formatter path basename))
         )
    (cond
     ;; if path is a alias
     (shim (rjs-alias-variableName shim))
     ;; if path is a text! path
     ((string-match "^text!" path)
      (concat (file-name-sans-extension basename) rjs-text-suffix))
     ;; if path was formatted by rjs-var-formatter
     (formatted formatted)
     
     ;; default return the basename
     (t basename)
     )))

(defvar rjs-tail-path '(lambda (path) nil)
  "Override this function to specify if you want a path to be put at the end of the requirejs function block.
Certain teams/companies have guidelines where they always put text! paths at the end of the function declaration.
In this case, you would (setq rjs-tail-path 'your-team-requirements)
`rjs-tail-path' takes a path and if it should be put at the end, returns a non-nil value.")

(defun rjs-sort-require-paths(&optional other)
  "Sorts the paths inside define, then injects variable names into the corresponding function declaration."
  (interactive)
  (let ( (require-paths (rjs-get-require-paths))
         (standard-paths '())
         (tail-paths '())
         (final-list '()) )

    (if other
        (push other require-paths))
    (setq require-paths (remove-duplicates require-paths :test #'equal))
    
    ;; Create two lists, standard-paths go at the beginning
    ;;  tail-paths go at the end
    (dolist (elt require-paths)
      (progn
        (if (or (gethash elt rjs-aliases) (funcall rjs-tail-path elt))
            (push elt tail-paths)
          (push elt standard-paths))))

    ;; final-list stores the list we will inject into the define block
    (setq final-list
          (append (sort standard-paths 'rjs-path-compare)
                  (sort tail-paths 'rjs-alias-compare)))
    (save-excursion
      (rjs-goto-define)
      (js2-goto-next-node)
      (js2-goto-nth-child 1)
      (js2-goto-first-child-node)

      (rjs-clear-list)

      ;; inject the function variable params
      (insert (mapconcat 'identity (maplist #'rjs-get-variable-name  final-list) ", "))

      (rjs-goto-define)
      (js2-goto-first-child-node)
      (let ((end))
        (rjs-clear-list)
        (insert "\n\n")
        (backward-char 1)

        ;; inject the newline separated variables
        (insert (mapconcat 'identity (maplist #'(lambda(a) (concat "'" (car a) "'")) final-list) ",\n"))

        ;; indent the define block
        (setq end  (+ 1 (point)))
        (rjs-goto-define)
        (js2-indent-region (point) end)
        )
      (js2-goto-next-node)
      (js2-goto-first-child-node)
      (js2-mode-wait-for-parse 'js2-eightify-list)
      )
    ))

(defun rjs-is-define-call (node)
  "Returns true if the node is a CALL node and it equals 'define'"
  (and
   (= (js2-node-type node) js2-CALL)
   (equal rjs-define-or-require (buffer-substring (js2-node-abs-pos define) (+ 6 (js2-node-abs-pos define)) )) ))

(defun rjs-jump-to()
  "Goes to the variable declaration of the node under the cursor.  If you are inside a define block's function parameters, `rjs-jump-to' attempts to call `rjs-jump-to-module' to go to the corresponding file."
  (interactive)
  (let* ( (node (js2-node-at-point))
          (name (js2-name-node-name node))
          (declaration (js2-name-declaration name))
          (define) )
    (if declaration
        (progn
          (setq define (js2-node-parent (js2-node-parent node)))
          (if (rjs-is-define-call define)
              ;; navigate to corresponding path
              (rjs-jump-to-module))
          
          ;; jump to the declaration
          (goto-char (js2-node-abs-pos declaration)) )) ))

(defvar rjs-require-base nil
  "`rjs-require-base' is the base path for looking for javascript files.")

(defvar rjs-path-formatter '(lambda (a) a)
  "`rjs-path-formatter' takes a found javascript file and formats it so it can go in a define([...] block")

(defvar rjs-popup-keymap
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap popup-menu-keymap)
    (define-key keymap [return] 'popup-select )
    (define-key keymap [tab] 'popup-select )
    keymap)
  "*A keymap for `rjs-jump-to-module' of `rjs'.")

(defun rjs-validate-project ()
  "Determines if your requireJS project is valid to be able to run filesystem actions."
  (if (not rjs-require-base) (error "Please set rjs-require-base to your requireJS project root.")))

(defun rjs-navigate (filename)
  "Navigates to a file, if it exists.  Errors out if filename is a directory"
  (let ((filepath (concat rjs-require-base filename)))
    (if (not (file-directory-p filepath))
        (find-file-existing filepath)
      (error (format "Won't navigate, %s is a directory." filepath)))))

(defun rjs-find-filepath (variableName)
  "Invokes a find command under `rjs-require-base' and looks for variableName"
  (message (format "Finding variableName: '%s' ..." variableName))
  (rjs-valid-project)
  (let* ((command (format "cd %s; find . -name \"%s.js\"" rjs-require-base variableName))
        (files (split-string (s-trim (shell-command-to-string command)) "\n")))
    ;; (message (format "command = %s" command))
    ;; (message (format "files = %s %s" files (length files)))
    (cond ((= (length files) 1) (s-trim (first files)))
          ((if (> (length files) 1) (popup-menu* files :keymap rjs-popup-keymap)))) ))

(defun rjs-jump-to-module ()
  "Tries to jump to the file that is behind the variable name under your cursor.
If more than one file matches your variable name, rjs provides a menu to select the appropriate file.
`rjs-require-base' Must be set in order to execute this function."
  (interactive)
  (rjs-validate-project)
  (let* (
         (node (js2-node-at-point))
         (name (buffer-substring (js2-node-abs-pos node) (js2-node-abs-end node)))
         (alias (gethash name rjs-alias-var-lookup))
         (result)
         )
    (message (format "alias = %s" alias))
    (if alias
        (progn
          (setq result (rjs-alias-path alias))
          (message (format "Jumping to rjs alias = %s" result))
          (rjs-navigate result))
      (progn
        (setq result (rjs-find-filepath name))
        (message (format "Jumping to: %s" result))
        (rjs-navigate result)
        ))))

(defun rjs-add-to-define ()
  "Tries to add the path behind the variable name under your cursor to the above define block.
If more than one file matches your variable name, rjs provides a menu to prompt for the appropriate file path.
`rjs-require-base' Must be set in order to execute this function."
  (interactive)
  (rjs-validate-project)
  (let* ( (node (js2-node-at-point))
          (name (buffer-substring (js2-node-abs-pos node) (js2-node-abs-end node)))
          (alias (gethash name rjs-alias-var-lookup))
          (result) )
    (message (format "alias = %s" alias))
    (if alias (progn
                (setq result (rjs-alias-label alias))
                (message (format "Adding rjs alias = %s" result))
                (rjs-sort-require-paths result))
      (progn
        (setq result
              (replace-regexp-in-string "^\.\/" ""
                                        (replace-regexp-in-string "\.js$" ""
                                                                  (rjs-find-filepath name))))
        (message (format "Adding: %s" result))
        (rjs-sort-require-paths (funcall rjs-path-formatter result))
        ))))

(defun rjs-reset-project ()
  "Resets all project specific variables."
  (interactive)
  (setq rjs-require-base nil)
  (setq rjs-path-formatter '(lambda (a) a))
  (clrhash rjs-aliases)
  (clrhash rjs-alias-var-lookup)
  (setq rjs-var-formatter '(lambda (path basename) nil))
  (setq rjs-tail-path '(lambda (path) nil)))

(defun js2-eightify-list (&optional line-break)
  "Formats a list (fn parameters or a plain []) to be at most 80 characters wide"
  (interactive)
  (save-excursion
    (let* ((parent (js2-goto-parent-node))
           (orig-pos (point))
           (nodes (js2-node-child-list parent))
           (node-length (length nodes))
           (n (- node-length 1))
           (node (nth n nodes))
           (next-column-spot))
      (goto-char (js2-node-abs-end node))
      (kill-region (point) (progn
			 (search-forward-regexp ")\\|]")
			 (- (point) 1) ))
      ;; Remove whitespace around nodes
      (while (>= n 0)
        (goto-char (js2-node-abs-pos node))
        (setq next-column-spot (point))
        (kill-region (progn
                       (search-backward-regexp "[,(\[]")
                       (+ (point) 1)) next-column-spot)
        (setq n (- n 1))
        (setq node (nth n nodes)))
      (if (looking-at "(")
          (forward-char 1))
      (setq n (+ n 1))
      (if line-break
          (progn
            (insert "\n")
            (js2-indent-line) ))
      (while (< n node-length)
        (setq next-column-spot (current-column)) ;;(+ (current-column) (js2-node-len node)))
        (if (> next-column-spot 80)
            (progn
              (backward-delete-char 1) ;; remove the added space
              (insert "\n")
              (js2-indent-line)))
        (goto-char (+ (+(point) (js2-node-len node)) 1))
        (if (looking-at ",")
            (forward-char 1))
        (insert " ")
        (setq n (+ n 1))
        (setq node (nth n nodes)) )
      (delete-backward-char 1)
      (if line-break
          (progn
            (backward-char 1)
            (insert "\n"))) )))
