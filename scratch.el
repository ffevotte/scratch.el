;;; scratch.el --- Manage scratch buffers

;; Package-Requires: ((names "0.5") (emacs "24"))

;;; Commentary:
;;

;;; Code:

;; * Custom variables

(defgroup scratch nil
  "Manage scratch buffers"
  :group 'editing)

(defcustom scratch/directory "/scratch-buffer/"
  "Prefix path for scratch buffers.
It is safer if this directory doesn't exist."
  :type  'string
  :group 'scratch)

(defcustom scratch/default-name "*scratch*"
  "Default name for scratch buffers"
  :type  'string
  :group 'scratch)

(defcustom scratch/create-when-blank-name nil
  "If non-nil, leaving the buffer name blank in
  `scratch/switch-to-buffer' creates a new scratch buffer.
  Otherwise, switch to the default scratch buffer."
  :type  'boolean
  :group 'scratch)


;; * Entry points

(defun scratch/create (buffer-name)
  (interactive (list
                (generate-new-buffer-name
                 (if current-prefix-arg
                     (read-from-minibuffer "Create scratch buffer: ")
                   scratch/default-name))))
  (switch-to-buffer (scratch--get-buffer-create buffer-name)))

(defmacro scratch--define-wrapper (fun doc prompt &rest args)
  (declare (debug (commandp stringp stringp &rest form)))
  `(progn
     (defun ,(intern (format "scratch/%s" fun)) (&optional buffer-or-name)
       ,(concat doc "\n"
                (format "See `%s' for details.\n" (symbol-name fun))
                "\n"
                "If BUFFER-OR-NAME does not identify an existing buffer, create a\n"
                "new buffer with that name. Any buffer created this way is\n"
                "considered a scratch buffer: it is associated to a non-existent\n"
                "file, is put in `scratch-mode' to avoid unwanted save, and\n"
                "inherits the current value of `default-directory'.")
       (interactive ,(concat "B" prompt))
       (when (and (not scratch/create-when-blank-name)
                  (string= buffer-or-name ""))
         (setq buffer-or-name scratch/default-name))
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

When in this minor mode, `save-buffer' always prompts for a new
file name before saving the buffer."
  :lighter " scratch")

(defun scratch--enable-maybe ()
  "Enable `scratch-mode' if needed.

Scratch buffers are those associated to files within
`scratch-mode-directory'."
  (when (string= scratch/directory
                 (file-name-directory
                  (or (buffer-file-name) "")))
    (scratch-mode 1)))

(add-hook 'after-change-major-mode-hook #'scratch--enable-maybe)


;; ** Create scratch buffers

(defun scratch--setup ()
  "Declare the current buffer as a scratch buffer.

Activate `scratch-mode' in the current buffer and give it a file
name in `scratch-directory'."
  (scratch-mode 1)
  (setq buffer-file-name
        (concat
         (file-name-as-directory scratch/directory)
         (buffer-name))))

(defun scratch--get-buffer-create (&optional buffer-or-name)
  "Return the buffer specified by BUFFER-OR-NAME, creating a new one if needed.

If BUFFER-OR-NAME is nil or blank, it defaults to \"*scratch*\".

If BUFFER-OR-NAME doesn't specify an existing buffer, create a
new buffer with that name. Any buffer created this way is
considered a scratch buffer: it is associated to a non-existent
file, is put in `scratch-mode' to avoid unwanted save, and
inherits the current value of `default-directory'."
  (setq buffer-or-name (or buffer-or-name ""))
  (let ((buffer (get-buffer buffer-or-name))
        (dd default-directory))
    (unless buffer
      (when (string= buffer-or-name "")
        (setq buffer-or-name scratch/default-name))
      (setq buffer (generate-new-buffer buffer-or-name))
      (with-current-buffer buffer
        (scratch--setup)
        (setq default-directory dd)
        (set-auto-mode)))
    buffer))


;; ** Always treat scratch buffers as non-file when saving

(defun scratch--save-buffer--ad-around (orig-fun &rest args)
  "Prompt for a new file name when saving buffers in `scratch-mode'."
  (if scratch-mode
      (let ((buffer-file-name nil))
        (apply orig-fun args))
    (apply orig-fun args)))
(advice-add 'save-buffer :around #'scratch--save-buffer--ad-around)


;; ** Temporarily consider scratch buffers as non-file

(defun scratch--ignore--ad-around (orig-fun &rest args)
  "Temporarily consider scratch buffers as non-file buffers."
  (let ((scratch--ignore t))
    (apply orig-fun args)))

(defun scratch--bfn--ad-around (orig-fun &rest args)
  "Temporarily consider scratch buffers as non-file buffers.

This only happens when `-ignore' is set."
  (let ((buffer (or (car args) (current-buffer))))
    (if (and (bound-and-true-p scratch--ignore)
             (with-current-buffer buffer scratch-mode))
        nil
      (apply orig-fun args))))
(advice-add 'buffer-file-name :around #'scratch--bfn--ad-around)

(defun scratch/ignore (fun)
  "Arrange for FUN to consider scratch buffers as non-file buffers."
  (advice-add fun :around #'scratch--ignore--ad-around))

(with-current-buffer "*scratch*"
  (scratch--setup))

(scratch/ignore 'compile)
(scratch/ignore 'recompile)
(scratch/ignore 'magit-status)

(provide 'scratch)

;;; scratch.el ends here
