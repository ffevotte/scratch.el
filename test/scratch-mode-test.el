(defmacro should-create-scratch-buffer (form name)
  `(progn
     (should (not (get-buffer ,name)))
     ,form
     (should (get-buffer ,name))
     (with-current-buffer (get-buffer ,name)
       (should scratch-mode)
       (scratch-mode -1)
       (kill-buffer (current-buffer)))))

(ert-deftest scratch-test/create1 ()
  (should-create-scratch-buffer
   (call-interactively #'scratch-create)
   "*scratch*<2>"))

(ert-deftest scratch-test/create2 ()
  (should-create-scratch-buffer
   (scratch-create "foo")
   "foo"))

(ert-deftest scratch-test/switch-to-buffer1 ()
  (should-create-scratch-buffer
   (scratch-switch-to-buffer "")
   "*scratch*<2>"))

(ert-deftest scratch-test/switch-to-buffer2 ()
  (should-create-scratch-buffer
   (scratch-switch-to-buffer "foo")
   "foo"))

(ert-deftest scratch-test/pop-to-buffer1 ()
  (should-create-scratch-buffer
   (scratch-pop-to-buffer "")
   "*scratch*<2>"))

(ert-deftest scratch-test/pop-to-buffer2 ()
  (should-create-scratch-buffer
   (scratch-pop-to-buffer "foo")
   "foo"))
