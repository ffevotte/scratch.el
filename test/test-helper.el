(require 'f)
(require 'cl)

(defvar test-path
  (f-dirname (f-this-file)))

(defvar code-path
  (f-parent test-path))

(require 'undercover)
(undercover "*.el")

(require 'scratch (f-expand "scratch.el" code-path))


(setq scratch-default-name "scratch-default-name")

(defun scratch-test/kill-buffer (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (scratch-mode -1)
      (kill-buffer (current-buffer)))))
