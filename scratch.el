;;; scratch.el --- Avoid data loss for scratch buffers

;; Package-Requires: ((emacs "24"))
;; Version: 0.2

;;; Commentary:
;;

;;; Code:

;; * Custom variables

(defgroup scratch nil
  "Manage scratch buffers"
  :group 'editing)

(defcustom scratch/default-name "*scratch*"
  "Default name for scratch buffers"
  :type  'string
  :group 'scratch)

(defcustom scratch/create-when-blank-name t
  "If non-nil, leaving the buffer name blank in
  `scratch/switch-to-buffer' creates a new scratch buffer.
  Otherwise, switch to the default scratch buffer."
  :type  'boolean
  :group 'scratch)


;; * Entry points

(defun scratch/create (buffer-name)
  (interactive (list (if current-prefix-arg
                         (read-from-minibuffer "Create scratch buffer: ")
                       scratch/default-name)))
  (switch-to-buffer
   (scratch--get-buffer-create (generate-new-buffer-name buffer-name))))

(defmacro scratch--define-wrapper (fun doc prompt &rest args)
  (declare (debug (commandp stringp stringp &rest form)))
  `(progn
     (defun ,(intern (format "scratch/%s" fun)) (&optional buffer-or-name)
       ,(concat doc "\n"
                (format "See `%s' for details.\n" (symbol-name fun))
                "\n"
                "If BUFFER-OR-NAME does not identify an existing buffer, create a\n"
                "new buffer with that name. Any buffer created this way is\n"
                "considered a scratch buffer: it is put in `scratch-mode' to\n"
                "avoid data loss, and inherits the current value of \n"
                "`default-directory'.")
       (interactive ,(concat "B" prompt))
       (let ((buffer (scratch--get-buffer-create buffer-or-name)))
         (,fun buffer ,@args)))))

(scratch--define-wrapper
 switch-to-buffer
 "Display buffer BUFFER-OR-NAME in the selected window."
 "Switch to buffer: "
 nil t)

(scratch--define-wrapper
 pop-to-buffer
 "Select buffer BUFFER-OR-NAME in some window, preferably a different one."
 "Pop to buffer: "
 (if current-prefix-arg t))


;; * Internal machinery

;; ** Minor mode for scratch buffers

(define-minor-mode scratch-mode
  "Minor mode for temporary buffers.

Emacs prompts to save modified buffers in this mode before
killing them."
  :lighter " scratch"
  ;; Offer saving scratch buffers when exiting emacs
  (setq buffer-offer-save scratch-mode))

;; Changing major mode doesn't kill `scratch-mode'
(put 'scratch-mode 'permanent-local t)

;; Saving a scratch buffer should deactivate `scratch-mode'
(defun scratch--deactivate-after-save ()
  "Deactivate `scratch-mode' after a buffer has been saved."
  (when (and scratch-mode
             (buffer-file-name))
    (scratch-mode -1)))

(add-hook 'after-save-hook 'scratch--deactivate-after-save)

;; ** Save scratch buffers before killing them

(defun scratch--save-before-kill ()
  "Offer to save modified `scratch-mode' buffers before killing them."
  (when (and scratch-mode
             (not (buffer-file-name))
             (buffer-modified-p)
             (yes-or-no-p
              (format
               "Buffer `%s' has not been saved. Save it now? "
               (buffer-name))))
    (call-interactively #'save-buffer)))

(add-hook 'kill-buffer-hook 'scratch--save-before-kill)

;; ** Create scratch buffers

(defun scratch--get-buffer-create (&optional buffer-or-name)
  "Return the buffer specified by BUFFER-OR-NAME, creating a new one if needed.

If BUFFER-OR-NAME is nil or blank and
`scratch/create-when-blank-name' is non-nil, it defaults to
`scratch/default-name'.

If BUFFER-OR-NAME doesn't specify an existing buffer, create a
new buffer with that name. Any buffer created this way is
considered a scratch buffer: it is put in `scratch-mode' to avoid
data loss, and inherits the current value of
`default-directory'."
  (setq buffer-or-name (or buffer-or-name ""))
  (let ((buffer (get-buffer buffer-or-name))
        (dd default-directory))
    (unless buffer
      (when (and scratch/create-when-blank-name
                 (string= buffer-or-name ""))
        (setq buffer-or-name scratch/default-name))
      (setq buffer (generate-new-buffer buffer-or-name))
      (with-current-buffer buffer
        (scratch-mode 1)
        (setq default-directory dd)
        (let ((buffer-file-name (concat dd buffer-or-name)))
          (set-auto-mode))))
    buffer))

;; * Footer

(provide 'scratch)

;;; scratch.el ends here
