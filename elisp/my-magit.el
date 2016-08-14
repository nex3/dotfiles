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
                        (magit-toplevel default-directory))
                      (magit-read-repository (and (consp current-prefix-arg)
                                                  (> (car current-prefix-arg) 4)))))))
  (switch-to-buffer
   (save-window-excursion
     (magit-status dir)
     (current-buffer))))

;; Automatically enable --all when no files are staged.
(defvar-local my-magit-staged-all nil
  "Whether the hook to auto-stage has been run.")
(add-hook 'magit-refresh-popup-buffer-hook (lambda ()
  (when (and (eq magit-this-popup 'magit-commit-popup)
             (not (magit-anything-staged-p))
             (not my-magit-staged-all))
    (setq my-magit-staged-all t)
    (magit-invoke-popup-switch ?a))))

;; Don't prompt to save buffers.
(setq magit-save-repository-buffers nil)

(defun my-magit-grep (regexp)
  "Like `vc-git-grep', but doesn't prompt for files or dirs."
  (interactive
   (progn
     (grep-compute-defaults)
     (if (equal current-prefix-arg '(16))
         (list (read-from-minibuffer "Run: " "git grep"
                                     nil nil 'grep-history))
       (list (grep-read-regexp)))))
  (vc-git-grep regexp ""
               (let ((buffer (my-magit-status-buffer)))
                 (if buffer
                     (buffer-local-value 'default-directory buffer)
                   default-directory))))

(define-key magit-mode-map (kbd "G") 'my-magit-grep)

(provide 'my-magit)

;;; my-magit.el ends here
