(require 'f)
(require 'cl)

(defvar test-path
  (f-dirname (f-this-file)))

(defvar code-path
  (f-parent test-path))

(require 'scratch (f-expand "scratch.el" code-path))
