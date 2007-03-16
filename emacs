;; -*- mode: emacs-lisp; -*-

(set-frame-font "-Misc-Fixed-Medium-R-SemiCondensed--13-120-75-75-C-60-ISO8859-1")
(setq make-backup-files nil)
(server-start)
(setq-default indent-tabs-mode nil)

;; Column numbering
(setq column-number-mode t)

;; -- Terminal Mouse Stuff -- ;;

;; Enable mouse mode
(xterm-mouse-mode 't)

;; Enable scrolling
(mouse-wheel-mode 't)

;; -- Random customizations --

;; C-return also comments and indents
(define-key global-map [(control return)] 'comment-indent-new-line)

;; M-right switches frame
(define-key global-map [(meta right)]
  '(lambda ()
     (interactive)
     (select-window (next-window))))

;; M-left switches frame backwards
(define-key global-map [(meta left)]
  '(lambda ()
     (interactive)
     (select-window (previous-window))))

;; No welcome screen
(setq inhibit-startup-message t)

;; -- Loading Modes -- ;;

(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path "~/.elisp")

(autoload 'javascript-mode "javascript" nil t)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)

(require 'haml-mode nil 't)
(require 'rhtml-mode nil 't)
(require 'yaml-mode nil 't)

(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.nlsp$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml$" . html-mode))
