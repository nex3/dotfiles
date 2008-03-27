;; -*- mode: emacs-lisp -*-
;; Nathan Weizenbaum's .emacs file

;; ----------
;; -- Do This First
;; ----------

;; No welcome screen
(setq inhibit-startup-message t)

;; 'Tupid toolbar
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)

(defun init-frame ()
  "Initialize a frame with my preferences."
  ;; Set my font
  (when window-system
    (cond ((>= emacs-major-version 23)
           (set-frame-font "Monospace-8"))
          ((and (eq window-system 'mac)
                (x-list-fonts "-apple-bitstream vera sans mono-medium-r-normal--0-0-0-0-m-0-mac-roman"))
           (set-default-font "-apple-bitstream vera sans mono-medium-r-normal--0-0-0-0-m-0-mac-roman"))
          ((x-list-fonts "-Misc-Fixed-Medium-R-SemiCondensed--13-120-75-75-C-60-ISO8859-1")
           (set-frame-font "-Misc-Fixed-Medium-R-SemiCondensed--13-120-75-75-C-60-ISO8859-1")))
    (toggle-scroll-bar -1)))

(init-frame)
(push (lambda (frame)
        (with-selected-frame frame
          (init-frame)))
      after-make-frame-functions)

(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path "/usr/share/emacs-snapshot/site-lisp")
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

(require 'pager)
(require 'perspective)

(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process, input and output via buffer *ruby*." t)
(autoload 'rdebug "rdebug" "Run the Ruby debugger." t)
(autoload 'maximize-frame "maxframe" "Maximize the Emacs frame." t)
(autoload 'blog "blog-mode" "Open up my blog file." t)
(autoload 'run-arc "inferior-arc" "Run an inferior Arc process, input and output via buffer *arc*." t)
(autoload 'gitsum "gitsum" "Entry point into gitsum-diff-mode." t)

(autoload 'pastie-region "pastie"
"Post the current region as a new paste at pastie.caboo.se.
Copies the URL into the kill ring." t)
(autoload 'pastie-buffer "pastie"
"Post the current buffer as a new paste at pastie.caboo.se.
Copies the URL into the kill ring." t)
(autoload 'pastie-get "pastie"
"Fetch the contents of the paste from pastie.caboo.se into a new buffer." t)

(defun autoload-mode (name regex &optional file)
  "Automatically loads a language mode
when opening a file of the appropriate type.

`name' is the name of the mode.
E.g. for javascript-mode, `name' would be \"javascript\".

`regex' is the regular expression matching filenames of the appropriate type.

`file' is the name of the file
from which the mode function should be loaded.
By default, it's `name'-mode.el."
  (let* ((name-mode (concat name "-mode"))
         (name-sym (intern name-mode)))
    (autoload name-sym (or file name-mode)
      (format "Major mode for editing %s." name) t)
    (add-to-list 'auto-mode-alist (cons regex name-sym))))

(autoload-mode "tex" "\\.tex$" "auctex")
(autoload-mode "javascript" "\\.js$" "javascript")
(autoload-mode "d" "\\.d[i]?\\'$")
(autoload-mode "textile" "\\.textile$")
(autoload-mode "haml" "\\.haml$")
(autoload-mode "sass" "\\.sass$")
(autoload-mode "rhtml" "\\.\\(rhtml\\|erb\\)$")
(autoload-mode "yaml" "\\.ya?ml$")
(autoload-mode "ruby" "\\(\\.\\(rb\\|rake\\|rjs\\)\\|Rakefile\\|Capfile\\)$")
(autoload-mode "css" "\\.css$")
(autoload-mode "haskell" "\\.l?hs$" "haskell-mode/haskell-site-file")
(autoload-mode "arc" "\\.arc$" "arc")

(defun my-c-style ()
  (c-set-style "gnu")
  (c-set-offset 'substatement-open '0)
  (c-set-offset 'arglist-intro 2)
  (c-set-offset 'arglist-close 0))
(add-hook 'cc-mode-hook 'my-c-style)

(eval-after-load 'rcirc
  '(progn
     (require 'rcirc-color)
     (require 'rcirc-unambiguous-nick-completion)
     (require 'rcirc-notify)
 
     (setq rcirc-server-alist '(("irc.freenode.net" :channels ("#arc" "#haml")))) 
     (setq my-rcirc-notify-timeout 90)
     (setq rcirc-unambiguous-complete t)
     (setq rcirc-debug-flag t)
     (setq fill-column 80)
     (setq rcirc-default-nick "nex3")
     (setq rcirc-default-user-name "nex3")
     (setq rcirc-default-user-full-name "Nathan Weizenbaum")
     (setq rcirc-time-format "[%l:%M] ")
     (setq rcirc-prompt "%t> ")
     (set-face-foreground 'rcirc-server "gray40")
     (set-face-foreground 'rcirc-timestamp "gray60")
     (rcirc-track-minor-mode 1)
 
     (add-hook 'rcirc-mode-hook (lambda () (flyspell-mode 1)))
 
     (defun-rcirc-command reconnect (arg)
       "Reconnect the server process."
       (interactive "i")
       (unless process
         (error "There's no process for this target"))
       (let* ((server (car (process-contact process)))
              (port (process-contact process :service))
              (nick (rcirc-nick process))
              channels query-buffers)
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (when (eq process (rcirc-buffer-process))
               (remove-hook 'change-major-mode-hook
                            'rcirc-change-major-mode-hook)
               (if (rcirc-channel-p rcirc-target)
                   (setq channels (cons rcirc-target channels))
                 (setq query-buffers (cons buf query-buffers))))))
         (delete-process process)
         (rcirc-connect server port nick
                        rcirc-default-user-name
                        rcirc-default-user-full-name
                        channels)))))

(eval-after-load "auctex"
  '(progn
     (with-temp-buffer (LaTeX-mode))
     (TeX-global-PDF-mode)
     (setcdr (assoc "^pdf$" TeX-output-view-style)
             '("." "evince %o"))))

(eval-after-load "haskell-mode"
  '(progn
     (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
     (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)))

(add-hook 'text-mode-hook 'flyspell-mode)

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

;; Don't ask about version-controlled symlinks.
(setq vc-follow-symlinks t)

;; Confirm killing emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; M-<direction> wraps
(setq windmove-wrap-around t)

;; Don't yell at me for narrowing the region
(put 'narrow-to-region 'disabled nil)

;; Maximize the window on load for Macs, where there's no full maximization,
;; and Windows, where Emacs can access the full maximization.
(if (or (eq window-system 'mac)
        (eq window-system 'w32))
    (maximize-frame))

;; Allow left-scrolling

;; Start server
(server-start)

;; ----------
;; -- Useful Functions
;; ----------

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
  (persp-switch "config")
  (find-file "~/.emacs" t))

(defun x-clipboard-only-yank ()
  "Insert the clipboard contents (but never killed text) at the mark"
  (interactive)
  (insert (x-get-clipboard)))

(defun nex3-irc ()
  "Open an IRC client with my credentials"
  (interactive)
  (let ((passwd (read-passwd "Password: ")))
    (setq rcirc-authinfo `(("freenode" nickserv "nex3" ,passwd)))
    (rcirc nil)))

(defun make-directory-from-minibuffer ()
  "Create a directory at the location given by the minibuffer,
which should be selected."
  (interactive)
  (make-directory (minibuffer-contents) t)
  (princ (concat "Created directory " (minibuffer-contents))))

(defmacro my-key (key fn)
  `(global-set-key (kbd ,key) ',fn))

(defmacro my-map (key name)
  (let ((varname (intern (concat (symbol-name name) "-map"))))
  `(progn
     (define-prefix-command ',name ',varname)
     (global-set-key (kbd ,key) ,varname))))

(defmacro my-unset (key)
  `(global-unset-key (kbd ,key)))

(defmacro my-strong-unset (key)
  `(my-key ,key keyboard-quit))

;; ----------
;; -- Keybindings
;; ----------

(my-unset "C-x C-z")
(my-unset "C-x p")

;; Ergonomic keybindings inspired by http://xahlee.org/emacs/ergonomic_emacs_keybinding.html

(my-key "M-j" backward-char)
(my-key "M-l" forward-char)
(my-key "M-i" previous-line)
(my-key "M-k" next-line)

(my-key "M-J" backward-word)
(my-key "M-L" forward-word)
(my-key "M-I" backward-paragraph)
(my-key "M-K" forward-paragraph)

(my-key "M-u" move-beginning-of-line)
(my-key "M-o" move-end-of-line)
(my-key "M-U" beginning-of-buffer)
(my-key "M-O" end-of-buffer)

(my-key "M-p" pager-page-up)
(my-key "M-;" pager-page-down)

(my-key "C-M-L" windmove-right)
(my-key "C-M-J" windmove-left)
(my-key "C-M-I" windmove-up)
(my-key "M-S-TAB" windmove-up)
(my-key "C-M-K" windmove-down)

(my-key "M-[" backward-delete-char-untabify)
(my-key "M-]" delete-char)
(my-key "M-{" backward-kill-word)
(my-key "M-}" kill-word)

(my-key "M-RET" comment-indent-new-line)
(my-key "C-v" x-clipboard-only-yank)
(my-key "C-z" clipboard-kill-region)

(my-key "M-w" kill-region)
(my-key "M-e" kill-ring-save)
(my-key "M-r" yank)
(my-key "M-R" yank-pop)

(my-key "M-d" undo)
(my-key "M-f" kill-line)

(my-key "M-S-SPC" mark-paragraph)
(my-key "M-SPC" set-mark-command)

(my-key "M-a" execute-extended-command)

(my-key "M-/" hippie-expand)

;; Cold Turkey

(when window-system
  (my-strong-unset "<backspace>")
  (my-strong-unset "C-<backspace>")
  (my-strong-unset "M-<backspace>")
  (my-unset "<left>")
  (my-unset "<right>")
  (my-unset "<up>")
  (my-unset "<down>")
  (my-unset "C-<left>")
  (my-unset "C-<right>")
  (my-unset "C-<up>")
  (my-unset "C-<down>")
  (my-unset "<next>")
  (my-unset "<prior>")
  (my-unset "<home>")
  (my-unset "<end>")
  (my-strong-unset "<delete>"))

;; My Keymap

(my-map "C-n" nex3)
(my-key "C-n ." .emacs)
(my-key "C-n i" nex3-irc)
(my-key "C-n b" blog)
(my-key "C-n c" comment-region)
(my-key "C-n u" uncomment-region)
(my-key "C-n m" make-directory-from-minibuffer)
(my-key "C-n f" auto-fill-mode)

(my-map "C-n C-p" nex3-pastie)
(my-key "C-n C-p p" pastie-region)
(my-key "C-n C-p b" pastie-buffer)
(my-key "C-n C-p g" pastie-get)

(quick-perspective-keys)
