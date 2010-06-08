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
                (let ((buffer (my-magit-current-buffer)))
                  (or (if buffer (buffer-local-value 'default-directory buffer))
                      (magit-get-top-dir default-directory)
                      (magit-read-top-dir (and (consp current-prefix-arg)
                                               (> (car current-prefix-arg) 4)))))))
  (magit-status dir))

;;;###autoload
(defun my-magithub-clone (username repo &optional srcp)
  "Clone GitHub repo USERNAME/REPO.
The clone is placed in ~/code, or with SRCP in ~/src.

Creates and switches to a new perspective named like the repo.

Interactively, prompts for the repo name, and by default creates
the repo in ~/code. With a prefix argument, creates the repo in
~/src."
  (interactive
   (destructuring-bind (username . repo) (magithub-read-repo)
     (list username repo current-prefix-arg)))
  ;; The trailing slash is necessary for Magit to be able to figure out
  ;; that this is actually a directory, not a file
  (let ((dir (concat (getenv "HOME") "/" (if srcp "src" "code") "/" repo "/")))
    (magit-run-git "clone" (concat "http://github.com/" username "/" repo ".git") dir)
    (persp-switch repo)
    (magit-status dir)))

(provide 'my-magit)

;;; my-magit.el ends here

