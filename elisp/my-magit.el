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
(defun my-magit-status ()
  "Like `magit-status', but use this perspective's Magit buffer if possible.

Uses `my-magit-status-buffer' to find the correct status buffer."
  (interactive)
  (let ((buffer (my-magit-current-buffer)))
    (magit-status (if buffer (buffer-local-value 'default-directory buffer)
                    (magit-get-top-dir default-directory)))))

(provide 'my-magit)

;;; my-magit.el ends here

