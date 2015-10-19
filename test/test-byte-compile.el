(require 'ert)
(require 'requirejs)

(ert-deftest requirejs--no-byte-compile-warnings ()
  "Byte-compile should not emit warnings"
  (byte-compile-file "requirejs.el")
  (switch-to-buffer "*Compile-Log*")
  (let ((lines (buffer-substring (point-min) (point-max))))
    (should (not (string-match "Warning:" lines)) )
    )
  )
