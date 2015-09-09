(require 'f)

(defvar scratch-support-path
  (f-dirname load-file-name))

(defvar scratch-features-path
  (f-parent scratch-support-path))

(defvar scratch-root-path
  (f-parent scratch-features-path))

(add-to-list 'load-path scratch-root-path)

(require 'undercover)
(undercover "*.el")

(require 'scratch)
(require 'espuds)
(require 'ert)
(require 'cl)


(Setup
 (setq use-dialog-box nil))

(Before)

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
