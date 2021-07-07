(let ((make-backup-files nil)
      (generated-autoload-file "~/.elisp/my-loaddefs.el")
      (byte-compile-warnings nil)
      (byte-compile-verbose nil)
      (noninteractive t)
      (dirs '("~/.elisp"))
      (elpa-dirs (directory-files "~/.elisp/elpa" 'full "-\\([0-9]\\.?\\)+")))
  (setq load-path (append dirs elpa-dirs load-path))
  (dolist (dir dirs) (update-directory-autoloads dir))
  (byte-recompile-directory "~/.elisp" 0))