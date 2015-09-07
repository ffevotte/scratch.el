(defmacro should-create-scratch-buffer (form name)
  "Test whether running FORM does create a scratch buffer named
NAME."
  `(progn
     (should (not (get-buffer ,name)))
     ,form
     (should (get-buffer ,name))
     (with-current-buffer (get-buffer ,name)
       (should scratch-mode)
       (scratch-test/kill-buffer (current-buffer)))))

(ert-deftest scratch-test/create ()
  "`scratch-create'"
  (should-create-scratch-buffer
   (call-interactively #'scratch-create)
   scratch-default-name)
  (should-create-scratch-buffer
   (scratch-create "foo")
   "foo"))

(ert-deftest scratch-test/switch-to-buffer ()
  "`scratch-switch-to-buffer'"
  (should-create-scratch-buffer
   (scratch-switch-to-buffer "")
   scratch-default-name)
  (should-create-scratch-buffer
   (scratch-switch-to-buffer "foo")
   "foo"))

(ert-deftest scratch-test/pop-to-buffer ()
  "`scratch-pop-to-buffer'"
  (should-create-scratch-buffer
   (scratch-pop-to-buffer "")
   scratch-default-name)
  (should-create-scratch-buffer
   (scratch-pop-to-buffer "foo")
   "foo"))


(ert-deftest scratch-test/deactivate ()
  "deactivate `scratch-mode' after save"
  (switch-to-buffer (scratch-create "foo.el"))
  (should scratch-mode)
  (write-file "/tmp/foo.el")
  (should (null scratch-mode)))
