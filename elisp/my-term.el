;;; my-magit.el --- My personal ansi-term customization

;;; Commentary:
;; This file contains my customization to make ansi-term more usable.

(require 'term)
(require 'my-magit)
(require 'cl)

;;;###autoload
(defun my-term (&optional arg)
  "Create or switch to the terminal for the current perspective.

A numeric prefix arg (as in ‘C-u 42 M-x eshell RET’) switches to
the session with that number, creating it if necessary. A
nonnumeric prefix arg means to create a new session.

When ARG isn't passed and this is invoked multiple times, cycles
through shells."
  (interactive "P")
  (let* ((term-buffer-base (concat "shell: " (persp-name persp-curr)))
         (term-buffer-name (concat "*" term-buffer-base "*"))
         (magit-buffer (my-magit-status-buffer))
         (default-directory (or (and magit-buffer
                                     (with-current-buffer magit-buffer
                                       default-directory))
                                (magit-toplevel default-directory)
                                default-directory)))

    (cond
     (arg
      (let* ((name
              (if (integerp arg) (format "%s<%d>" term-buffer-name arg)
                (generate-new-buffer-name term-buffer-name)))
             (buffer (get-buffer name)))
        (if buffer (switch-to-buffer buffer)
          (setq name (term-ansi-make-term name "/bin/bash"))
          (set-buffer name)
          (term-mode)
          (term-char-mode)
          (term-set-escape-char ?\C-x)
          (switch-to-buffer name))))

     ((eq last-command this-command)
      (let ((term-buffer-re (concat "^" (regexp-quote term-buffer-name)))
            first found-current)
        (dolist (buffer (reverse (persp-buffers persp-curr)))
          (when (buffer-name buffer)
            (cond
             (found-current
              (when (string-match term-buffer-re (buffer-name buffer))
                (switch-to-buffer buffer)
                (return-from my-term)))
             ((eq buffer (current-buffer))
              (setq found-current t))
             ((and (not first) (string-match term-buffer-re (buffer-name buffer)))
              (setq first buffer)))))

        (when first (switch-to-buffer first))))

     (t
      (if (get-buffer term-buffer-name)
          (switch-to-buffer term-buffer-name)
        (ansi-term "/bin/bash" term-buffer-base))))))

(defun term-in-prompt-p (&optional only-in-text)
  "Returns whether the cursor is in the editable prompt line.
If ONLY-IN-TEXT is non-nil, returns whether the cursor is in the
editable portion of the line."
  ;; Check if we're on the last line of text as a heuristic so we don't have to
  ;; do a regexp search.
  (or (save-excursion (forward-line) (eobp))
      (save-excursion
        (end-of-line)
        (not (re-search-forward term-prompt-regexp nil t)))))

(defun term-if-in-prompt (command fallback)
  "Returns a function that executes COMMAND if the terminal is in the prompt.
Otherwise, executes FALLBACK."
  `(lambda (&rest args)
     ,(interactive-form fallback)
     (if (term-in-prompt-p t) (,command) (apply ',fallback args))))

(defun term-next-line (&rest args)
  "Like `next-line', but moves to the cursor position on the input line."
  (interactive "^p\np")
  (let ((was-in-prompt (term-in-prompt-p)))
    (apply 'next-line args)
    (when (and (not was-in-prompt) (term-in-prompt-p))
      (goto-char (process-mark (get-buffer-process (current-buffer)))))))

(defun term-yank (&optional arg)
  "Like `yank', but works in `term-mode'."
  (interactive "P")
  (term-yank-insert (current-kill
                     (cond
                      ((listp arg) 0)
                      ((eq arg '-) -2)
                      (t (1- arg))))))

(defun term-yank-pop (&optional arg)
  "Like `yank-pop', but works in `term-mode'."
  (interactive "p")
  (unless (and (consp last-command)
               (eq (car last-command) 'term-yank))
    (user-error "Previous command was not a yank"))

  (let ((delete-command (if (< (point) (mark t))
                            'term-send-del
                          'term-send-backspace)))
    (dotimes (_ (cdr last-command))
      (funcall delete-command)))

  (term-yank-insert (current-kill arg)))

(defun term-yank-insert (text)
  "Inserts text into the terminal as quoted characters."
  (when text
    (push-mark)
    (let ((length
           (loop for char across text
                 ;; Only emit the first line of the copied text so we don't put
                 ;; the terminal into a broken state.
                 until (eq char ?\n)
                 ;; Always quoted-insert the character in case we've copied a
                 ;; control character somehow.
                 do (term-send-raw-string (format "\e[2~%c" char))
                 count t)))
     (setq this-command (cons 'term-yank length)))))

(advice-add 'term-emulate-terminal
            :around
            (lambda (old &rest args)
              (let ((inhibit-read-only t))
                (apply old args))))

(defvar my-term-keymap (make-keymap)
  "A keymap containing my custom term keybindings.")

(define-minor-mode my-term-keys-mode
  "A minor mode that overrides the term-mode keybindings."
  :keymap my-term-keymap)

(defun term-use-default (key)
  "Use the default keybinding for KEY in term-mode."
  (define-key my-term-keymap (kbd key) (lookup-key my-default-keymap (kbd key))))

(defun term-in-prompt (key command &optional fallback)
  "Bind KEY to COMMAND in term-mode when the cursor is in the prompt.
Otherwise, bind key to FALLBACK or its default binding."
  (define-key my-term-keymap (kbd key)
    (term-if-in-prompt command
                       (or fallback (lookup-key my-default-keymap (kbd key))))))

(add-hook 'term-mode-hook
          (lambda ()
            (read-only-mode 1)
            (setq term-prompt-regexp "^[^ ]+@[^ ]+:.*\\$ ")
            (my-term-keys-mode)))

(term-use-default "C-u")
(term-use-default "C-h")
(term-use-default "C-r")
(term-use-default "C-s")
(term-use-default "M-o")
(term-use-default "M-i")

(term-in-prompt "M-j" 'term-send-left)
(term-in-prompt "M-J" (lambda () (interactive) (term-send-raw-string "\eb")))
(term-in-prompt "M-:" (lambda () (interactive) (term-send-raw-string "\ef")))
(term-in-prompt "M-;" 'term-send-right)
(term-in-prompt "M-u" 'term-send-home 'term-bol)
(term-in-prompt "M-p" 'term-send-end)

(term-in-prompt "M-n" 'term-send-backspace)
(term-in-prompt "M-." 'term-send-del)
(term-in-prompt "M-N" (lambda () (interactive)
                        (copy-region-as-kill (point) (progn (forward-word -1) (point)))
                        (term-send-raw-string "\e[3;3~")))
(term-in-prompt "M->" (lambda () (interactive)
                        (copy-region-as-kill (point) (progn (forward-word) (point)))
                        (term-send-raw-string "\ed")))
(term-in-prompt "C-k" (lambda () (interactive)
                        (copy-region-as-kill (point) (progn (end-of-line) (point)))
                        (term-send-raw-string "\C-k")))

(define-key my-term-keymap (kbd "M-k") 'term-next-line)
(define-key my-term-keymap (kbd "M-O") 'term-send-up)
(define-key my-term-keymap (kbd "M-I") 'term-send-down)
(define-key my-term-keymap (kbd "M-?") (lambda () (interactive) (term-send-raw-string "\C-_")))
(define-key my-term-keymap (kbd "M-S-SPC") 'term-yank)
(define-key my-term-keymap (kbd "M-C-S-SPC") 'term-yank-pop)

(provide 'my-term)

;;; my-term.el ends here
