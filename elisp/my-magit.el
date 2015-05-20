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

(advice-add 'magit-key-mode-popup-committing :after (lambda ()
  (when (not (magit-anything-staged-p))
    (magit-key-mode-toggle-option 'committing "--all"))))

(define-key magit-mode-map (kbd "M-I") 'magit-goto-next-section)
(define-key magit-mode-map (kbd "M-O") 'magit-goto-previous-section)

;; This is copied from the Magit 1.4.x branch prior to release. Once 1.4.2 is
;; released, it should be fine to remove it.
(defun magit-revert-buffers ()
  (let ((topdir (magit-get-top-dir)))
    (when topdir
      (let ((gitdir  (magit-git-dir))
            (tracked (magit-git-lines "ls-tree" "-r" "--name-only" "HEAD")))
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (let ((file (buffer-file-name)))
              (and file (string-prefix-p topdir file)
                   (not (string-prefix-p gitdir file))
                   (member (file-relative-name file topdir) tracked)
                   (let ((remote-file-name-inhibit-cache t))
                     (when (buffer-stale--default-function)
                       (setq auto-revert-notify-modified-p nil)
                       (when auto-revert-verbose
                         (message "Reverting buffer `%s'." (buffer-name)))
                       (let ((buffer-read-only buffer-read-only))
                         (revert-buffer 'ignore-auto 'dont-ask 'preserve-modes)))
                     (vc-find-file-hook)
                     (run-hooks 'magit-revert-buffer-hook))))))))))

(defun my-magit-grep (regexp &optional dir)
  "Like `vc-git-grep', but doesn't prompt for files."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep"
				   nil nil 'grep-history)
	     nil))
      (t (let* ((regexp (grep-read-regexp))
                (buffer (my-magit-status-buffer))
                (default-dir
                  (if buffer
                      (buffer-local-value 'default-directory buffer)
                    default-directory))
		(dir (read-directory-name "In directory: "
					  nil default-dir t)))
	   (list regexp dir))))))
  (vc-git-grep regexp "" dir))

(magit-key-mode-insert-action 'dispatch "G" "Grep" #'my-magit-grep)
(define-key magit-mode-map (kbd "G") 'my-magit-grep)

(provide 'my-magit)

;;; my-magit.el ends here
