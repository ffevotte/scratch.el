;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I bind key \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$"
       (lambda (key fn-name)
         (global-set-key (kbd key) (intern fn-name))))

(Given "^No buffers are open$"
       (lambda ()
         (switch-to-buffer "*Messages*")
         (mapc (lambda (buffer)
                 (unless (or (s-starts-with-p " " (buffer-name buffer))
                             (s-starts-with-p "*" (buffer-name buffer))
                             (buffer-file-name buffer))
                   (with-current-buffer buffer (scratch-mode -1))
                   (kill-buffer buffer)))
               (buffer-list))))

(When "^I write the buffer to \"\\([^\"]+\\)\"$"
      (lambda (name)
        (write-file name)))

(When "^I create a new buffer$"
      (lambda ()
        (switch-to-buffer "foo")))

(When "^I activate \"\\(.+\\)\"$"
      (lambda (name)
        (funcall (intern name) +1)))

(When "^I kill the current buffer$"
      (lambda ()
        (setq scratch-test--ask nil)
        (letf (((symbol-function 'yes-or-no-p)
                (lambda (&rest args)
                  (setq scratch-test--ask 'yes-or-no-p)
                  (throw 'scratch-test t))))
          (catch 'scratch-test
            (kill-buffer (current-buffer))))))

(When "^I exit emacs$"
      (lambda ()
        (setq scratch-test--ask nil)
        (letf (((symbol-function 'read-event)
                (lambda (&rest args)
                  (setq scratch-test--ask 'read-event)
                  (throw 'scratch-test t)))
               ((symbol-function 'kill-emacs)
                (lambda (&rest args)
                  (setq scratch-test--exit 'kill-emacs)
                  (throw 'scratch-test t))))
          (catch 'scratch-test
            (save-buffers-kill-terminal)))))

(Then "^\"\\(.+\\)\" should be active$"
      (lambda (mode)
        (cl-assert (symbol-value (intern mode)) nil
                   "Expected `%s' to be non-nil."
                   (intern mode) )))

(Then "^\"\\(.+\\)\" should not be active$"
      (lambda (mode)
        (cl-assert (null (symbol-value (intern mode))) nil
                   "Expected `%s' to be nil, found value `%s'"
                   (intern mode) (symbol-value (intern mode)))))

(Then "^The current buffer should be in \"\\(.+\\)\"$"
      (lambda (mode)
        (cl-assert (eq major-mode (intern mode)) nil
                   "Expected `major-mode' to be `%s', found `%s' instead."
                   (intern mode) major-mode)))

(Then "^I should be prompted to save the buffer$"
      (lambda ()
        (cl-assert scratch-test--ask nil
                   "Expected `scratch-test--ask' to be non-nil.")))

(Then "^I should not be prompted to save the buffer$"
      (lambda ()
        (cl-assert (null scratch-test--ask) nil
                   "Expected `scratch-test--ask' to be nil, found `%s'."
                   scratch-test--ask)))
