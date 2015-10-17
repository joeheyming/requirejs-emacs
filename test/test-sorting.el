(require 'ert)
(require 'requirejs)

(ert-deftest requirejs--sort-empty ()
  "Test sorting when the define block is empty"
  (with-temp-buffer
    (insert "define([], function() {
});")
    (js2-mode)
    (requirejs-sort-require-paths)
    (should (string= (buffer-substring (point-min) (point-max)) "define([], function() {
});"))
    (search-backward "]")
    (insert "\n") ;; add newline in between []
    (js2-parse)
    (requirejs-sort-require-paths)
    (should (string= (buffer-substring (point-min) (point-max)) "define([], function() {
});"))))

(ert-deftest requirejs--add-a-require ()
  "Test sorting when you add a quoted string to the list of defines.
It should add the parameter"
  (with-temp-buffer
    (insert "define(['a'], function() {
});")
    (js2-mode)
    (requirejs-sort-require-paths)
    (should (string= (buffer-substring (point-min) (point-max)) "define([
    'a'
], function(a) {
});"))
    (search-backward "'a'")
    (forward-char 3)
    (insert ",\n'c'")
    (js2-parse)
    (requirejs-sort-require-paths)
    (should (string= (buffer-substring (point-min) (point-max)) "define([
    'a',
    'c'
], function(a, c) {
});"))
    
    (search-forward "'a',")
    (insert "\n'z',")
    (js2-parse)
    (requirejs-sort-require-paths)
    ;; note that the z parameter was inserted after the 'a' parameter, but sorting ensures 'z' comes after 'c'
    (should (string= (buffer-substring (point-min) (point-max)) "define([
    'a',
    'c',
    'z'
], function(a, c, z) {
});"))

    (search-forward "'z'")
    (insert ",\n'b'")
    (js2-parse)
    (requirejs-sort-require-paths)
    ;; note that the b parameter was inserted after the 'z' parameter, but sorting ensures 'b' comes after 'a'
    (should (string= (buffer-substring (point-min) (point-max)) "define([
    'a',
    'b',
    'c',
    'z'
], function(a, b, c, z) {
});"))))
