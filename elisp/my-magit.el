;;; my-magit.el --- My personal Magit customization

;;; Commentary:
;; This file contains my own Magit rebindings and tweaks.
;; It obviously depends heavily on Magit itself.
;; It's loaded automatically once Magit is.

(require 'magit)
(require 'cl)

(defun my-magit-status-buffer ()
  "Return the Magit status buffer for this perspective.
Return nil if there is no such buffer.  If there are more than
one Magit status buffers active, this returns an arbitrary one."
  (dolist (buffer (persp-buffers persp-curr))
    (when (and (buffer-name buffer)
               (string-match "^\*magit: " (buffer-name buffer)))
      (return-from nil buffer))))

;;;###autoload
(defun my-magit-status (dir)
  "Like `magit-status', but use this perspective's Magit buffer if possible.
DIR is the Git-managed directory for which to set up the Magit buffer.

When called interactively, use `my-magit-status-buffer' to find
the correct status buffer.  Fall back on the default directory or
simply prompting the user."
  (interactive (list
                (let ((buffer (my-magit-status-buffer)))
                  (or (if buffer (buffer-local-value 'default-directory buffer))
                      (unless (string-match "^\\*scratch\\*" (buffer-name))
                        (magit-get-top-dir default-directory))
                      (magit-read-top-dir (and (consp current-prefix-arg)
                                               (> (car current-prefix-arg) 4)))))))
  (magit-status dir))

(define-key magit-mode-map (kbd "M-I") 'magit-goto-next-section)
(define-key magit-mode-map (kbd "M-O") 'magit-goto-previous-section)


(provide 'my-magit)

;;; my-magit.el ends here
