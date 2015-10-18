(require 'ert)
(require 'requirejs)

(ert-deftest requirejs--empty-define ()
  "Test get define, basic"
  (with-temp-buffer
    (insert "define([], function() {
});")
    (js2-mode)
    (let ((node (requirejs-get-first-define-node)))
      (should (string= (js2-node-string (js2-call-node-target (js2-expr-stmt-node-expr node))) "define"))
      )))


(ert-deftest requirejs--empty-define-with-outer-expressions ()
  "Test get define, basic"
  (with-temp-buffer
    (insert "var foo;
// this is a comment
define([], function() {
});")
    (js2-mode)
    (let ((node (requirejs-get-first-define-node)))
      (should (string= (js2-node-string (js2-call-node-target (js2-expr-stmt-node-expr node))) "define"))
      )))
