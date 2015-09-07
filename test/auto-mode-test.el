(ert-deftest scratch-test/auto-mode ()
  "Named scratch buffers should be created with the relevant
major mode, depending on the file extension in their name."
  (should (not (get-buffer "foo.el")))
  (with-current-buffer (scratch-create "foo.el")
    (should (eq major-mode 'emacs-lisp-mode))))
