;; -*- mode: emacs-lisp -*-
;; Nathan Weizenbaum's .emacs file

;; ----------
;; -- Do This First
;; ----------

;; No welcome screen
(setq inhibit-startup-message t)

;; 'Tupid toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

;; Set the font.
(if window-system
    (cond ((>= emacs-major-version 23)
           (set-frame-font "Monospace-8"))
          ((x-list-fonts "-Misc-Fixed-Medium-R-SemiCondensed--13-120-75-75-C-60-ISO8859-1")
           (set-frame-font "-Misc-Fixed-Medium-R-SemiCondensed--13-120-75-75-C-60-ISO8859-1"))))

(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path "~/.elisp")
(require 'color-theme)

(defun my-color-theme-mods ()
  (interactive)
  (color-theme-install
   '(my-color-theme-mods
     (())
     (mode-line ((t (:background "gray80" :foreground "gray20" :box (:line-width -1 :style "released-button")))))
     (textile-link-face ((t (:foreground "#398EE6"))))
     (textile-ul-bullet-face ((t (:foreground "#398EE6")))))))

(color-theme-initialize)
(load "alexandres-theme")
(my-color-theme-dark)
(setq color-theme-is-cumulative t)
(my-color-theme-mods)

;; ----------
;; -- Loading Modules
;; ----------

(defun try-require (name &rest rest)
  "Same as require, but catches file errors.
If the module can't be loaded, sets <name>-required to nil.
Otherwise, sets it to t."
  (let ((var (intern (concat (symbol-name name) "-required"))))
    (condition-case err
        (progn (apply 'require (cons name rest))
               (set-variable var t))
      (file-error (set-variable var nil)))))

(try-require 'erc)
(if erc-required (load "erc-page-me"))

(require 'http-post)
(require 'psvn)
(require 'pager)

(require 'textile-mode)
(require 'haml-mode)
(require 'sass-mode)
(require 'rhtml-mode)
(require 'yaml-mode)
(require 'ruby-mode)
(require 'css-mode)
(require 'rhtml-mode)

(autoload 'javascript-mode "javascript"  "Major mode for editing Javascript code." t)
(autoload 'csharp-mode     "csharp-mode" "Major mode for editing C# code." t)
(autoload 'd-mode          "d-mode"      "Major mode for editing D code." t)

(load "~/.elisp/haskell-mode/haskell-site-file")

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

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(defun d-mode-hook ()
  (c-set-style "gnu")
  (c-set-offset 'substatement-open '0))
(add-hook 'd-mode-hook 'd-mode-hook)

(if erc-required
    (setq erc-keywords '("nex3" "Nathan")))

;; ----------
;; -- Random Customizations and Configurations
;; ----------

;; Stupid annoying backups.
(setq make-backup-files nil)

;; No line highlighting for console mode
(if (not window-system) (global-hl-line-mode))

;; Ignore extensions for stuff I don't care about
(setq completion-ignored-extensions
      '(".a" ".so" ".o" "~" ".bak" ".class" ".hi"))

;; Don't wrap lines
(setq default-truncate-lines t)

; Yes-or-no questions accept y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; I hate hard tabs!
(setq-default indent-tabs-mode nil)

;; Column numbering
(setq column-number-mode t)

;; I like my backspace key working
(setq normal-erase-is-backspace-mode 0)

;; Annoying quit message
(setq save-abbrevs nil)

;; Syntax highlighting rocks.
(global-font-lock-mode 1)

;; ----------
;; -- Useful Functions
;; ----------

(defvar blog-url "http://nex-3.com"
  "The URL of my blog.")

(defmacro http-try-post (&rest args)
  `(let ((proc (http-post ,@args)))
     (while (eq (process-status proc) 'open) (sit-for 0.1))
     (let ((result (buffer-string))
           (status http-status-code))
       (kill-buffer (process-buffer proc))
       (if (>= status 400) nil result))))

(defun blog-try-post (title tags)
  (let ((result
         (http-try-post (concat blog-url "/posts")
                        (list (cons "post[title]" title)
                              (cons "post[tag_string]" tags)
                              (cons "post[content]" (buffer-string))
                              (cons "admin[pass]" (read-passwd "Password: "))
                              '("admin[name]" . "Nathan"))
                        'utf-8)))
    (and result
         (progn
           (string-match "<a href=\"\\([^\"]+\\)\">" result)
           (match-string 1 result)))))

(defun blog-post-entry (&optional title tags)
  "Post an entry to my blog"
  (interactive)
  (setq title (or title (read-from-minibuffer "Post title: ")))
  (setq tags  (or tags  (read-from-minibuffer "Tags: ")))
  (let ((link (blog-try-post title tags)))
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

(defun blog ()
  (find-file "/tmp/blog")
  (textile-mode))

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

(defun x-clipboard-only-yank ()
  "Insert the clipboard contents (but never killed text) at the mark"
  (interactive)
  (insert (x-get-clipboard)))

(defun nex3-erc ()
  "Open an ERC client with my credentials"
  (interactive)
  (let ((passwd (read-passwd "Password: ")))
    (erc :server "irc.freenode.net" :port "6667" :nick "nex3" :password passwd :full-name "Nathan Weizenbaum")))

(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))

;; ----------
;; -- Keybindings
;; ----------

(setq real-keyboard-keys
      '(("M-<up>"        . "[1;3A")
        ("M-<down>"      . "[1;3B")
        ("M-<right>"     . "[1;3C")
        ("M-<left>"      . "[1;3D")
        ("C-<return>"    . "\C-j")
        ("C-<delete>"    . "[3;5~")
        ("C-<up>"        . "[1;5A")
        ("C-<down>"      . "[1;5B")
        ("C-<right>"     . "[1;5C")
        ("C-<left>"      . "[1;5D")))
(defun key (desc)
  (or (and window-system (read-kbd-macro desc))
      (or (cdr (assoc desc real-keyboard-keys))
          (read-kbd-macro desc))))

(global-unset-key (key "C-x p"))
(global-unset-key (key "C-x n"))
(global-unset-key (key "C-x C-z"))

(if window-system (global-set-key (key "C-<backspace>") 'backward-kill-word))

(global-set-key (key "C-<delete>")    'kill-word)
(global-set-key (key "C-<left>")      'backward-word)
(global-set-key (key "C-<right>")     'forward-word)
(global-set-key (key "C-<up>")        'backward-paragraph)
(global-set-key (key "C-<down>")      'forward-paragraph)

(global-set-key (key "<next>")   'pager-page-down)
(global-set-key (key "<prior>")  'pager-page-up)
(global-set-key (key "M-<up>")   'pager-row-up)
(global-set-key (key "M-<down>") 'pager-row-down)

(global-set-key (key "C-v") 'x-clipboard-only-yank)
(global-set-key (key "C-z") 'clipboard-kill-region)

(global-set-key (key "M-<right>") 'select-next-window)
(global-set-key (key "M-<left>")  'select-previous-window)

(global-set-key (key "C-<return>") 'comment-indent-new-line)

;; My Keymap

(define-prefix-command 'nex3 'nex3-map)
(global-set-key (key "C-n") nex3-map)

(global-set-key (key "C-n .") '.emacs)
(global-set-key (key "C-n e") 'nex3-erc)
(global-set-key (key "C-n b") 'blog)
