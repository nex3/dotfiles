(setq my-elisp-dir
  (if (file-directory-p "~/.elisp") "~/.elisp/"
       ;; On Windows, we can't symlink in ~/.elisp so we have to load
       ;; straight from the dotfiles directory.
       (concat
         (file-name-parent-directory (file-truename "~/.emacs"))
         (file-name-as-directory "elisp"))))
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path "/usr/share/emacs-snapshot/site-lisp")
(add-to-list 'load-path my-elisp-dir)
(add-to-list 'load-path "~/share/emacs/site-lisp")

(setq package-user-dir (concat my-elisp-dir "elpa"))
(load "my-loaddefs")
