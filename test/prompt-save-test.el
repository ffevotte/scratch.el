(defmacro scratch-test/catch (buffer &rest body)
  "Execute BODY in BUFFER, and arrange for `scratch-test' signals
to be caught. Return the value signal if one was caught, or nil otherwise."
  (declare (indent 1))
  (let ((buffer-sym (make-symbol "buffer")))
    `(letf (((symbol-function 'yes-or-no-p)
             (lambda (&rest args)
               (throw 'scratch-test 'ask)))
            ((symbol-function 'read-event)
             (lambda (&rest args)
               (throw 'scratch-test 'ask)))
            ((symbol-function 'kill-emacs)
             (lambda (&rest args)
               (throw 'scratch-test 'exit))))
       (let ((,buffer-sym ,buffer))
         (with-current-buffer ,buffer-sym
           (prog1 (catch 'scratch-test
                    (ignore ,@body))
             (scratch-test/kill-buffer ,buffer-sym)))))))

(defun on-kill (buffer)
  "Return the event happening when modifying BUFFER and then
trying to kill it."
  (scratch-test/catch buffer
    (set-buffer-modified-p t)
    (kill-buffer (current-buffer))))

(ert-deftest scratch-test/save-on-kill ()
  "A prompt should appear when trying to kill a scratch
buffer. Not when trying to kill a normal non-file buffer."
  (should (eq 'ask (on-kill (scratch-create "foo"))))
  (should (null    (on-kill (get-buffer-create "foo")))))



(defun on-exit (buffer)
  "Return the event happening when modifying BUFFER and then
trying to exit Emacs."
  (scratch-test/catch buffer
    (insert "foo")
    (call-interactively #'save-buffers-kill-terminal)))

(ert-deftest scratch-test/save-on-exit ()
  "A prompt should appear when trying to exit with a modified
scratch buffers. Not for a normal non-file buffer."
  (should (eq 'ask  (on-exit (scratch-create "foo"))))
  (should (eq 'exit (on-exit (get-buffer-create "foo")))))



(defun on-compile (buffer)
  "Return the event happening when modifying BUFFER and then
trying to run a compilation command."
  (scratch-test/catch buffer
    (insert "foo")
    (compile "true")))

(ert-deftest scratch-test/save-on-compile ()
  "A prompt should appear when compiling with an unsaved file
buffer. Not with a modified scratch buffer."
  (should (null    (on-compile (scratch-create "foo"))))
  (should (eq 'ask (on-compile (find-file "foo")))))
