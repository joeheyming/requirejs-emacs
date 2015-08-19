;;; rjs.el --- Requirejs import manipulation and source traversal.

;; Author: Joe Heyming <joeheyming@gmail.com>
;; Version: 1.0
;; Keywords: javascript, requirejs
;; Package-Requires: ((js2-mode "20141118")(expand-region "0.10.0"))

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
;; (rjs-add-alias "jquery" "$" "path/to/jquery-<version>")
;;
;; (add-hook 'js2-mode-hook
;;           '(lambda ()
;;              (local-set-key [(super a) ?s ?r ] 'rjs-sort-require-paths)
;;              (local-set-key [(super a) ?a ?r ] 'rjs-add-to-define)
;;              (local-set-key [(super a) ?r ?j ] 'rjs-jump-to-module)
;;              ))

(provide 'rjs)
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js2-mode utilities
;;  These utilities use the js2-mode abstract syntax tree to do useful operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun js2-node-quoted-contents (node)
  "Get the inner text of a quoted string. e.g 'foo' -> yeilds foo"
  (let ((node-pos (js2-node-abs-pos node)))
    (buffer-substring-no-properties (+ 1 node-pos) (- (+ node-pos (js2-node-len node)) 1))))

(defun js2-goto-first-child-node()
  "Goes to the first child, returns that node, if any."
  (interactive)
  (let* ( (node (js2-node-at-point))
          (child-node (js2-node-first-child node)) )
    (if child-node
        (goto-char (js2-node-abs-pos child-node)))
    child-node))

(defun js2-goto-parent-node()
  "Goes to your parent node, returns that node."
  (interactive)
  (let* ((curnode (js2-node-at-point))
         (parentnode (js2-node-parent curnode)))
    (if (eq (js2-node-abs-pos curnode) (js2-node-abs-pos parentnode))
        (progn
          (setq node (js2-node-at-point))
          (goto-char (- (js2-node-abs-pos node) 1))
          (setq node (js2-node-at-point))
          (goto-char (js2-node-abs-pos node))
          node)
      (progn
        (goto-char (js2-node-abs-pos parentnode))
        parentnode))))

(defun js2-goto-next-node()
  "Goes to the next sibling, if any.  Returns the sibling"
  (interactive)
  (let*  ((current (js2-node-at-point))
          (sibling (js2-node-next-sibling current))
          (parent) (parentparent) (siblings))
    (if sibling
        (goto-char (js2-node-abs-pos sibling)))
    sibling))

(defun js2-goto-prev-node()
  "Goes to the previous sibling, if any.  Returns the sibling"
  (interactive)
  (let* ((current (js2-node-at-point))
         (sibling (js2-node-prev-sibling current)))
    (if sibling
        (goto-char (js2-node-abs-pos sibling)))
    sibling))

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

(defvar rjs-define-or-require "define")

(defun rjs-goto-define()
  "Jump to the define at the beginning of the buffer"
  (interactive)
  (let* ( (define-node (js2-get-function-call-node rjs-define-or-require (js2-node-root (js2-node-at-point)) ))
          (define-call (js2-node-parent define-node)) )
    (goto-char (+ (js2-node-abs-pos define-call) (js2-call-node-lp define-call) ))
    (search-forward "[")
    (backward-char 1)))

(defun rjs-get-require-paths()
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

;; A table that stores user defined aliases.
;; An alias is added velow with rjs-add-alias
(defvar rjs-aliases (make-hash-table :test 'equal))

;; Examples, if you want to expose that lookup: knockout, but name the variable ko:
;;  (rjs-add-alias "knockout" "ko" "knockout")
;;  This says that when you sort require paths, we will interpret knockout as the variable ko:
;;  define(['knockout'], function(ko) { ... }
;;  The third string is used to determine how to sort
;; Example: if you include lodash, but you want to sort by 'Lodash', not '_':
;;  (rjs-add-alias "_", "_" "lodash")
;;  This will make sure the define block looks like this:
;;  define(['knockout', '_'], function(ko, _) { ... }  instead of this:
;;  define(['_', 'knockout'], function(_, ko) { ... }  because K comes before L
(defun rjs-add-alias (label variableName path)
  "Add a requirejs config shim"
  (interactive)
  (puthash label
           (make-rjs-alias :label label :variableName variableName :path path)
           rjs-aliases))

(defun rjs-path-compare(a b)
  "Compare paths ignore case"
  (string< (downcase a) (downcase b)))

(defun rjs-alias-compare(a b)
  "Compare paths ignore case"
  (let ((shim-a (rjs-alias-path (gethash a rjs-aliases)))
        (shim-b (rjs-alias-path (gethash b rjs-aliases))))
    (string< shim-a shim-b)))

(defun rjs-get-variable-name(item)
  (interactive)

  ;; TODO: handle text! paths
  (let* ( (path (car item))
          (basename (car (last (split-string path "/" )) ))
          (shim (gethash path rjs-aliases)) )
    (if shim
        (rjs-alias-variableName shim)
      basename) ))

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
        (if (gethash elt rjs-aliases)
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

      ;; ;; finally inject the function variable params
      (insert (mapconcat 'identity (maplist #'rjs-get-variable-name  final-list) ", "))

      (rjs-goto-define)
      (js2-goto-first-child-node)
      (let ((end))
        (rjs-clear-list)
        (insert "\n\n")
        (backward-char 1)

        ;; inject the newline separated variables
        (insert (mapconcat 'identity (maplist #'(lambda(a) (concat "'" (car a) "'")) final-list) ",\n"))

        ;; indent
        (setq end  (+ 1 (point)))
        (rjs-goto-define)
        (js2-indent-region (point) end)
        )      
      )
    ))

(defun rjs-is-define-call (node)
  "Returns true if the node is a CALL node and it equals 'define'"
  (and
   (= (js2-node-type node) js2-CALL)
   (equal rjs-define-or-require (buffer-substring (js2-node-abs-pos define) (+ 6 (js2-node-abs-pos define)) )) ))

(defun rjs-jump-to()
  "Go to the declaration of the node under the cursor"
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

;; This is the base path for looking for javascript files.
(defvar rjs-require-base "")

;; This takes a found javascript file and formats it so it can go in a define([...] block
(defvar rjs-path-formatter '(lambda (a) a))

(defun rjs-jump-to-module ()
  (interactive)
  (let* (
         (node (js2-node-at-point))
         (name (buffer-substring (js2-node-abs-pos node) (js2-node-abs-end node)))
         (command (format "find %s -name \"%s.js\"" rjs-require-base name))
         (result)
         )
    (setq result (car (split-string (first (split-string (shell-command-to-string command) "\n")) " ")))
    (if result
        (find-file result))))

(defun rjs-add-to-define ()
  (interactive)
  (let* ( (node (js2-node-at-point))
          (name (buffer-substring (js2-node-abs-pos node) (js2-node-abs-end node)))
          (command (format "cd %s && find . -name \"%s.js\" | sed 's/\.[^\.]*$//'" rjs-require-base name))
          (require-paths (rjs-get-require-paths))
          (result)
          )
    (setq result (car (cdr (split-string (first (split-string (shell-command-to-string command) "\n")) "^\./"))) )
    (if result
        (progn
          (rjs-sort-require-paths (funcall rjs-path-formatter result)) ))))
