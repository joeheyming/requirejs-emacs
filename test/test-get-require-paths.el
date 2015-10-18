(require 'ert)
(require 'requirejs)

(ert-deftest requirejs--get-empty-paths ()
  "Test when the define block is empty"
  (with-temp-buffer
    (insert "define([], function() {
});")
    (js2-mode)
    (should (= (length (requirejs-get-require-paths)) 0))
    (search-backward "]")
    (insert "\n") ;; add newline in between []
    (js2-parse)
    (should (= (length (requirejs-get-require-paths)) 0))
    ))

(ert-deftest requirejs--test-non-empty-paths ()
  "Test when you add a quoted string to the list of defines."
  (with-temp-buffer
    (insert "define(['a'], function() {
});")
    (js2-mode)
    (should (equal '("a") (requirejs-get-require-paths)))
    (search-backward "'a'")
    (forward-char 3)
    (insert ",'c'")
    (js2-parse)
    (should (equal '("a" "c") (requirejs-get-require-paths)))
    (search-backward "'c'")
    (insert "'a/b/c/z',")
    (js2-parse)
    (should (equal '("a" "a/b/c/z" "c") (requirejs-get-require-paths)))
    ))
