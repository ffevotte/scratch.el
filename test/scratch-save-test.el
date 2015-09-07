(defun on-kill (buffer)
  (letf (((symbol-function 'yes-or-no-p)
          (lambda (&rest args)
            (throw 'scratch-test 'ask))))
    (with-current-buffer buffer
      (set-buffer-modified-p t)
      (let ((event (catch 'scratch-test
                     (ignore (kill-buffer (current-buffer))))))
        (when event
          (scratch-mode -1)
          (kill-buffer (current-buffer)))
        event))))

(ert-deftest scratch-test/save-on-kill ()
  (should (eq 'ask (on-kill (scratch-create "foo"))))
  (should (null    (on-kill (get-buffer-create "foo")))))


(defun on-exit (buffer)
  (letf (((symbol-function 'read-event)
          (lambda (&rest args)
            (throw 'scratch-test 'ask)))
         ((symbol-function 'kill-emacs)
          (lambda (&rest args)
            (throw 'scratch-test 'exit))))
    (with-current-buffer buffer
      (insert "foo")
      (prog1 (catch 'scratch-test
               (ignore (call-interactively #'save-buffers-kill-terminal)))
        (scratch-mode -1)
        (kill-buffer (current-buffer))))))

(ert-deftest scratch-test/save-on-exit ()
  (should (eq 'ask  (on-exit (scratch-create "foo"))))
  (should (eq 'exit (on-exit (get-buffer-create "foo")))))
