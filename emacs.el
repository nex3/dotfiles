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
    (cond ((and (>= emacs-major-version 23)
                (not (string-match "\\.cs\\.washington\.edu$" system-name)))
           (if (string-equal system-name "Clara")
               (set-frame-font "Monospace-7")
             (set-frame-font "Monospace-8")))
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
(add-to-list 'load-path "~/.elisp/rcirc-notify-el")
(add-to-list 'load-path "~/.elisp/distel")
(add-to-list 'load-path "~/.elisp")
(add-to-list 'load-path "~/share/emacs/site-lisp")

(setq byte-compile-verbose nil)
(setq byte-compile-warnings nil)
(require 'byte-code-cache)
(require 'color-theme)

(defun my-color-theme-mods ()
  (interactive)
  (color-theme-install
   '(my-color-theme-mods
     (())
     (yas/field-highlight-face ((t (:background "gray30"))))
     (erb-face ((t (:background "gray15"))))
     (rcirc-server ((t (:foreground "gray40"))))
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
(autoload-mode "ruby" "\\(\\.\\(rb\\|rake\\|rjs\\|gemspec\\|thor\\)\\|Rakefile\\|Capfile\\|Thorfile\\)$")
(autoload-mode "css" "\\.css$")
(autoload-mode "haskell" "\\.l?hs$" "haskell-mode/haskell-site-file")
(autoload-mode "arc" "\\.arc$" "arc")
(autoload-mode "erlang" "\\.[he]rl$" "erlang/erlang")
(autoload-mode "treetop" "\\.treetop$")

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

     (setq rcirc-colors '("darkviolet" "magenta" "deeppink" "red" "yellow" "lawngreen"
                          "white" "LightSlateGrey" "RoyalBlue" "DeepSkyBlue" "LightSkyBlue"
                          "DarkOliveGreen" "PaleGreen" "ForestGreen" "LightGoldenrodYellow"
                          "sienna"))
     (setq rcirc-server-alist '(("irc.freenode.net" :channels ("#haml" "#rubyfringe" "#freehackersunion"))
                                ("irc.nex-3.com" :nick "Nathan" :channels ("#rc" "#dnd"))))
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
 
     (defun-rcirc-command raw (arg)
       "Send a raw string to the IRC server."
       (rcirc-send-string process arg))))

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

(eval-after-load "erlang"
  '(progn
     (add-hook 'erlang-mode-hook
               (lambda () (setq inferior-erlang-machine-options '("-sname" "emacs"))))
     (condition-case nil
         (progn
           (require 'distel)
           (distel-setup)
           (define-key erlang-extended-mode-map (kbd "C-M-i") 'backward-up-list)
           (defconst distel-shell-keys
             '(("\C-\M-i" erl-complete)
               ("\M-?"    erl-complete)	
               ("\M-."    erl-find-source-under-point)
               ("\M-,"    erl-find-source-unwind) 
               ("\M-*"    erl-find-source-unwind)))
           (add-hook 'erlang-shell-mode-hook
                     (lambda ()
                       (dolist (spec distel-shell-keys)
                         (define-key erlang-shell-mode-map (car spec) (cadr spec))))))
       (file-error nil))))

(when window-system
  (eval-after-load "ruby-mode"
    '(progn
       (define-key ruby-mode-map (kbd "C-M-l") 'ruby-forward-sexp)
       (define-key ruby-mode-map (kbd "C-M-j") 'ruby-backward-sexp)
       (setq ruby-deep-indent-paren-style nil))))

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
      '(".a" ".so" ".o" "~" ".bak" ".class" ".hi" ".beam"))

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

;; Don't yell at me!
(setq disabled-command-function nil)

;; Maximize the window on load for Macs, where there's no full maximization,
;; and Windows, where Emacs can access the full maximization.
(if (or (eq window-system 'mac)
        (eq window-system 'w32))
    (maximize-frame))

(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(transient-mark-mode -1)

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
  (let ((link (file-symlink-p "~/.emacs")))
    (find-file (or link "~/.emacs") t)))

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

(defun load-yasnippet ()
  "Load and (re)initialize yasnippet.el."
  (interactive)
  (unless (featurep 'yasnippet)
    (load "yasnippet/yasnippet")
    (add-to-list 'yas/extra-mode-hooks 'js2-mode)
    (yas/initialize))
  (yas/load-directory "~/.yasnippets"))

;; ----------
;; -- Keybindings
;; ----------

;; -- Setup

(defvar my-keymap (make-keymap)
  "A keymap containing my custom keybindings.")

(define-minor-mode my-keys-mode
  "A minor mode that encapsulates my custom keybindings."
  :init-value t
  :keymap my-keymap)

(defmacro my-key (key fn &rest options)
  `(progn
    (define-key ,(if (memq :global options) 'global-map 'my-keymap) (kbd ,key) ',fn)
    ,(when (or (memq :kill options) (memq :delete options))
       (let ((name (intern (concat "my-delete-" (symbol-name fn))))
             (is-kill (memq :kill options)))
         `(progn
            (defun ,name (arg)
              ,(concat (if is-kill "Kill" "Delete")
                       " the region moved by `" (symbol-name fn) "':\n\n"
                       (documentation fn))
              (interactive "p")
              (,(if is-kill 'kill-region 'delete-region) (point) (progn (,fn arg) (point))))
            (define-key my-delete-map (kbd ,key) ',name))))))

(defmacro my-map (key name)
  (let ((varname (intern (concat (symbol-name name) "-map"))))
  `(progn
     (define-prefix-command ',name ',varname)
     (define-key my-keymap (kbd ,key) ,varname))))

(defmacro my-unset (key)
  `(global-unset-key (kbd ,key)))

(defmacro my-strong-unset (key)
  `(my-key ,key keyboard-quit))

;; -- Actual Bindings

(my-unset "C-x C-z")
(my-unset "C-x p")
(my-unset "C-]")

;; Ergonomic keybindings inspired by http://xahlee.org/emacs/ergonomic_emacs_keybinding.html

(my-map "M-d" my-delete)

(my-key "M-j" backward-char :delete)
(my-key "M-;" forward-char :delete)
(my-key "M-k" next-line :kill)
(my-key "M-l" previous-line :kill)

(my-key "M-J" backward-word :kill)
(my-key "M-:" forward-word :kill)
(my-key "M-K" forward-paragraph :kill)
(my-key "M-L" backward-paragraph :kill)

(my-key "C-M-S-j" beginning-of-line :kill)
(my-key "C-M-:" end-of-line :kill)
(my-key "C-M-S-k" end-of-buffer)
(my-key "C-M-S-l" beginning-of-buffer)

(my-key "C-M-;" forward-sexp :global :kill)
(my-key "C-M-j" backward-sexp :global :kill)
(my-key "C-M-k" down-list :global)
(my-key "C-M-l" backward-up-list :global)

(my-key "M-u" windmove-left)
(my-key "M-p" windmove-right)
(my-key "M-i" windmove-down)
(my-key "M-o" windmove-up)

(my-key "M-RET" comment-indent-new-line)
(my-key "C-v" x-clipboard-only-yank)
(my-key "C-z" clipboard-kill-region)

(my-key "M-w" kill-region)
(my-key "M-e" kill-ring-save)
(my-key "M-r" yank)
(my-key "M-R" yank-pop)

(my-key "M-S-SPC" mark-paragraph)
(my-key "M-SPC" set-mark-command)

(my-key "M-'" repeat)
(my-key "M-a" execute-extended-command)
(my-key "M-/" hippie-expand)

;; My Keymap

(my-map "C-n" nex3)
(my-key "C-n ." .emacs)
(my-key "C-n i" nex3-irc)
(my-key "C-n b" blog)
(my-key "C-n c" comment-region)
(my-key "C-n u" uncomment-region)
(my-key "C-n m" make-directory-from-minibuffer)
(my-key "C-n f" auto-fill-mode)
(my-key "C-n y" load-yasnippet)

(my-map "C-n C-p" nex3-pastie)
(my-key "C-n C-p p" pastie-region)
(my-key "C-n C-p b" pastie-buffer)
(my-key "C-n C-p g" pastie-get)

(quick-perspective-keys)
