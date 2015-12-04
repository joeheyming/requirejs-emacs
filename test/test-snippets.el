(require 'ert)
(require 'ert-x)
(require 'requirejs)

(yas-global-mode 1)

(defun yas--buffer-contents ()
  (buffer-substring-no-properties (point-min) (point-max)))

;;; Helpers
;;;
(defun yas-should-expand (keys-and-expansions)
  (dolist (key-and-expansion keys-and-expansions)
    (yas-exit-all-snippets)
    (narrow-to-region (point) (point))
    (insert (car key-and-expansion))
    (let ((yas-fallback-behavior nil))
      (ert-simulate-command '(yas-expand)))
    (message "buffer: %s" (yas--buffer-contents))
    (unless (string= (yas--buffer-contents) (cdr key-and-expansion))
      (ert-fail (format "\"%s\" should have expanded to \"%s\" but got \"%s\""
                        (car key-and-expansion)
                        (cdr key-and-expansion)
                        (yas--buffer-contents)))))
  (yas-exit-all-snippets))

(ert-deftest requirejs--yas-define-block ()
  "define snippet should expand"
  (with-temp-buffer
    (js2-mode)
    (requirejs-mode)
      
    (insert "def")
    (let ((yas-fallback-behavior nil))
      (ert-simulate-command '(yas-expand)))
    (should (string= (yas--buffer-contents) "define([], function() {
    'use strict';
    
});"))
    )
  )
