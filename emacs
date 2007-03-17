;; -*- mode: emacs-lisp; -*-
;; Nathan Weizenbaum's .emacs file

;; -- Useful Reference Variables --

;; Whether or not we're running in a terminal
(setq is-a-tty (not (eq (tty-color-alist) nil)))

;; ----------
;; -- Random Customizations and Configurations
;; ----------

;; Set the font.
;; TODO: Get other fonts for other computers.
(if (and (not is-a-tty)
         (x-list-fonts "-Misc-Fixed-Medium-R-SemiCondensed--13-120-75-75-C-60-ISO8859-1"))
    (set-frame-font "-Misc-Fixed-Medium-R-SemiCondensed--13-120-75-75-C-60-ISO8859-1")
  nil)

;; Stupid annoying backups.
(setq make-backup-files nil)

;; We like to be able to open new files in the same emacs.
(server-start)

;; No welcome screen
(setq inhibit-startup-message t)

;; Ignore extensions for stuff I don't care about
(setq completion-ignored-extensions
      '(".a" ".so" ".o" "~" ".bak"))

;; 'Tupid toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; I hate text wrap
(toggle-truncate-lines 't)

;; -- Set default minor modes --

;; I hate hard tabs!
(setq-default indent-tabs-mode nil)

;; Column numbering
(setq column-number-mode t)

;; I like my backspace key working
(setq normal-erase-is-backspace-mode 0)

;; ----------
;; -- Useful Functions
;; ----------

;; Kill All Buffers without prompting.
;; Modified from kill-some-buffers in files.el, which prompts too much.
;; Created by Akkana.
(defun kill-all-buffers ()
  "Kill all buffers without prompting."
  (interactive)
  (let ((list (buffer-list)))
    (while list
      (let* ((buffer (car list))
             (name (buffer-name buffer)))
             (kill-buffer buffer))
      (setq list (cdr list)))))

;; ----------
;; -- Keybindings
;; ----------

;; Wow I hate these.
(global-unset-key "\C-xp")
(global-unset-key "\C-xn")

;; C-return also comments and indents
(define-key global-map [(control return)] 'comment-indent-new-line)
(global-set-key "O1;5D" 'backward-kill-word)
(global-set-key "[3;5~" 'kill-word)
(global-set-key "O1;5C" 'forward-word)
(global-set-key "O1;5D" 'backward-word)
(global-set-key "O1;5A" 'backward-paragraph)
(global-set-key "O1;5B" 'forward-paragraph)

;; M-right switches frame
(define-key global-map [(meta right)]
  '(lambda ()
     "Switch to the next frame"
     (interactive)
     (select-window (next-window))))

;; M-left switches frame backwards
(define-key global-map [(meta left)]
  '(lambda ()
     "Switch to the previous frame"
     (interactive)
     (select-window (previous-window))))

;; ----------
;; -- Loading Modes -- ;;
;; ----------

(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path "~/.elisp")

(autoload 'javascript-mode "javascript" nil t)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)

(require 'haml-mode nil 't)
(require 'sass-mode nil 't)
(require 'rhtml-mode nil 't)
(require 'yaml-mode nil 't)

(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.nlsp$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml$" . html-mode))
