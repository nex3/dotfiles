;; -*- mode: emacs-lisp; -*-
;; Nathan Weizenbaum's .emacs file

;; -- Useful Reference Variables --

;; ----------
;; -- Random Customizations and Configurations
;; ----------

;; Set the font.
;; TODO: Get other fonts for other computers.
(if window-system
    (cond ((>= emacs-major-version 23)
           (set-frame-font "Monospace-8"))
          ((x-list-fonts "-Misc-Fixed-Medium-R-SemiCondensed--13-120-75-75-C-60-ISO8859-1")
           (set-frame-font "-Misc-Fixed-Medium-R-SemiCondensed--13-120-75-75-C-60-ISO8859-1")))
  nil)

;; Stupid annoying backups.
(setq make-backup-files nil)

;; No welcome screen
(setq inhibit-startup-message t)

;; Ignore extensions for stuff I don't care about
(setq completion-ignored-extensions
      '(".a" ".so" ".o" "~" ".bak" ".class" ".hi"))

;; Don't wrap lines
(setq default-truncate-lines t)

;; 'Tupid toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

; Yes-or-no questions accept y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; -- Set default minor modes --

;; I hate hard tabs!
(setq-default indent-tabs-mode nil)

;; Column numbering
(setq column-number-mode t)

;; I like my backspace key working
(setq normal-erase-is-backspace-mode 0)

;; Syntax highlighting rocks.
(global-font-lock-mode 1)

;; ----------
;; -- Useful Functions
;; ----------

(defvar blog-url "http://nex3.leeweiz.net"
  "The URL of my blog.")

(defmacro http-try-post (&rest args)
  `(let ((proc (http-post ,@args)))
     (while (eq (process-status proc) 'open) (sit-for 0.1))
     (let ((result (buffer-string))
           (status http-status-code))
       (kill-buffer (process-buffer proc))
       (if (>= status 400) nil result))))

(defun blog-try-post (title)
  (let ((result
         (http-try-post (concat blog-url "/posts")
                        (list (cons "post[title]" title)
                              (cons "post[content]" (buffer-string))
                              (cons "admin[pass]" (read-passwd "Password: "))
                              '("admin[name]" . "Nathan"))
                        'utf-8)))
    (and result
         (progn
           (string-match "<a href=\"\\([^\"]+\\)\">" result)
           (match-string 1 result)))))

(defun blog-post-entry (&optional title)
  "Post an entry to my blog"
  (interactive)
  (setq title (or title (read-from-minibuffer "Post title: ")))
  (let ((link (blog-try-post title)))
    (if (not link)
        (progn
          (message "Invalid password.")
          (sit-for 1)
          (blog-post-entry title))
      (progn
        (shell-command (concat "firefox " link))
        (message "Successfully posted.")))))

(defun blog-preview ()
  "Preview an entry for my blog"
  (interactive)
  (let ((result
         (http-try-post (concat blog-url "/posts/new.html")
                        (list  (cons "post[content]" (buffer-string))
                               (cons "admin[pass]" (read-passwd "Password: "))
                               '("admin[name]" . "Nathan"))
                        'utf-8)))
    (if (not result)
        (progn
          (message "Invalid password.")
          (sit-for 1)
          (blog-preview))
      (progn
        (while (string-match "\\(href\\|src\\)=\\(\"\\|'\\)/" result)
          (setq result (replace-match (concat "\\1=\\2" blog-url "/") t nil result)))
        (let ((tmp (concat (make-temp-file "blog") ".html")))
          (with-temp-file tmp
            (insert result))
          (shell-command (concat "firefox " tmp)))))))

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
      (setq list (cdr list))))
  (delete-other-windows))

(defun .emacs ()
  "Open up the .emacs configuration file."
  (interactive)
  (find-file "~/.emacs" t))

;; ----------
;; -- Loading Modes -- ;;
;; ----------

(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path "~/.elisp")

(autoload 'javascript-mode "javascript" nil t)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(load "~/.elisp/haskell-mode/haskell-site-file")

(require 'http-post nil 't)
(require 'textile-mode nil 't)
(require 'haml-mode nil 't)
(require 'sass-mode nil 't)
(require 'rhtml-mode nil 't)
(require 'yaml-mode nil 't)
(require 'ruby-mode nil 't)
(require 'css-mode nil 't)
(require 'rhtml-mode nil 't)
(require 'color-theme)

(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.nlsp$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'$" . d-mode))
(add-to-list 'auto-mode-alist '("blog$" . textile-mode))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(if window-system
    (progn (color-theme-initialize)
           (load-theme 'alexandres)
           (my-color-theme-dark))
  nil)

(defun set-four-tabs ()
  (setq tab-width 4))
(add-hook 'd-mode-hook 'set-four-tabs)

;; ----------
;; -- Keybindings
;; ----------

;; Wow I hate these.
(global-unset-key "\C-xp")
(global-unset-key "\C-xn")

;; -- Useful Arrow Key / Deletion Bindings --

(if window-system
    (global-set-key "O1;5D" 'backward-kill-word)
  (progn
    (global-set-key "[3;5~" 'kill-word)
    (global-set-key "O1;5C" 'forward-word)
    (global-set-key "O1;5D" 'backward-word)
    (global-set-key "O1;5A" 'backward-paragraph)
    (global-set-key "O1;5B" 'forward-paragraph)))
  
;; -- Pager Keybindings --

(require 'pager)
(global-set-key (kbd "<next>") 'pager-page-down)
(global-set-key (kbd "<prior>") 'pager-page-up)
(global-set-key "O1;3A" 'pager-row-up)
(global-set-key "O1;3B" 'pager-row-down)

;; -- Other Random Keybindings --

;; C-return also comments and indents
(define-key global-map (if window-system
                           [C-return] (kbd "C-j"))
  'comment-indent-new-line)

(defun select-next-frame ()
  "Switch to the next frame"
     (interactive)
     (select-window (next-window)))

(defun select-previous-frame ()
  "Switch to the next frame"
     (interactive)
     (select-window (previous-window)))

;; M-right switches frame
(define-key global-map (if window-system 
                           [(meta right)] "O1;3C")
  'select-next-frame)

;; M-left switches frame backwards
(define-key global-map (if window-system 
                           [(meta left)] "O1;3D")
  'select-previous-frame)
