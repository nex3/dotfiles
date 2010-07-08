;; Nathan Weizenbaum's .emacs file

;; ----------
;; -- Do This First
;; ----------

(setq inhibit-startup-message t)

(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)

(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path "/usr/share/emacs-snapshot/site-lisp")
(add-to-list 'load-path "~/.elisp/rcirc-notify-el")
(add-to-list 'load-path "~/.elisp/haskell-mode")
(add-to-list 'load-path "~/.elisp/auctex")
(add-to-list 'load-path "~/.elisp/auctex/preview")
(add-to-list 'load-path "~/.elisp/ocaml")
(add-to-list 'load-path "~/.elisp")
(add-to-list 'load-path "~/share/emacs/site-lisp")

(when (< emacs-major-version 23) (require 'old-emacs))

(defun init-frame (&optional frame)
  "Initialize FRAME with my preferences."
  (setq frame (or frame (selected-frame)))
  (with-selected-frame frame
    ;; Set my font
    (when window-system
      (cond ((>= emacs-major-version 23) (set-frame-font "Monospace-8.5"))
            ((and (eq window-system 'mac)
                  (x-list-fonts "-apple-bitstream vera sans mono-medium-r-normal--0-0-0-0-m-0-mac-roman"))
             (set-default-font "-apple-bitstream vera sans mono-medium-r-normal--0-0-0-0-m-0-mac-roman"))
            ((x-list-fonts "-Misc-Fixed-Medium-R-SemiCondensed--13-120-75-75-C-60-ISO8859-1")
             (set-frame-font "-Misc-Fixed-Medium-R-SemiCondensed--13-120-75-75-C-60-ISO8859-1")))
      (toggle-scroll-bar -1))))

(init-frame)
(add-hook 'after-make-frame-functions 'init-frame)

(setq byte-compile-verbose nil)
(setq byte-compile-warnings nil)
(require 'byte-code-cache)
(require 'color-theme)

(color-theme-initialize)
(load "alexandres-theme")
(my-color-theme-dark)

(custom-set-faces
 '(default ((((min-colors 256)) (:foreground "pink"))
            (t (:foreground "white"))))
 ;; Don't highlight lines in the terminal
 '(hl-line ((((min-colors 256)) (:inherit highlight))
            (((min-colors 8)) (:inherit nil :background nil))))
 '(yas/field-highlight-face ((t (:background "gray30"))))
 '(erb-face ((t (:background "gray15"))))
 '(rcirc-server ((((min-colors 8)) (:foreground nil))
                 (t (:foreground "gray40"))))
 '(mode-line ((t (:background "gray80" :foreground "gray20" :box (:line-width -1 :style "released-button")))))
 '(textile-link-face ((t (:foreground "#398EE6"))))
 '(textile-ul-bullet-face ((t (:foreground "#398EE6"))))
 '(magit-item-highlight ((t (:background "#222222")))))

(setq frame-title-format '("Emacs: %b [" (:eval (persp-name persp-curr)) "]"))

;; ----------
;; -- Loading Modules
;; ----------

(load "my-loaddefs")
(require 'pager)
(require 'tex-site)
(eval-when-compile (require 'cl))

(defun load-mode (name regexp)
  "Set up a language mode NAME-mode so that
it's loaded for files matching REGEXP."
  (add-to-list 'auto-mode-alist (cons regexp (intern (format "%s-mode" name)))))

(load-mode 'javascript "\\.js$")
(load-mode 'd "\\.d[i]?\\'$")
(load-mode 'textile "\\.textile$")
(load-mode 'markdown "\\.\\(markdown\\|md\\)$")
(load-mode 'haml "\\.haml$")
(load-mode 'sass "\\.sass$")
(load-mode 'rhtml "\\.\\(rhtml\\|erb\\)$")
(load-mode 'yaml "\\.ya?ml$")
(load-mode 'ruby "\\(\\.\\(rb\\|rake\\|rjs\\|duby\\|gemspec\\|thor\\)\\|Rakefile\\|Capfile\\|Thorfile\\)$")
(load-mode 'css "\\.css$")
(load-mode 'haskell "\\.l?hs$")
(load-mode 'arc "\\.arc$")
(load-mode 'treetop "\\.treetop$")
(load-mode 'lua "\\.lua$")
(load-mode 'csharp "\\.cs$")
(load-mode 'factor "\\.factor$")
(load-mode 'caml "\\.ml[iylp]?$")

(defmacro my-after-load (name &rest body)
  "Like `eval-after-load', but a macro."
  (declare (indent 1))
  (my-after-load-helper name body))

(defun my-after-load-helper (name body)
  (if (eq name nil)
      `(progn ,@body)
    (let ((head (if (symbolp name) name (car name)))
          (rest (when (consp name) (cdr name))))
      `(eval-after-load ',head
         ',(my-after-load-helper rest body)))))

(defmacro my-add-hook (name &rest body)
  "Like `add-hook', but a macro.
The -hook suffix is unnecessary."
  (declare (indent 1))
  (let ((name (format "%s" name)))
    `(add-hook ',(intern
                  (if (string-match "-hook$" name) name
                    (format "%s-hook" name)))
             ,(if (and (eq (length body) 1)
                       (symbolp (car body)))
                  (list 'quote (car body))
                `(lambda () ,@body)))))

(my-after-load comint
  (define-key comint-mode-map (kbd "M-O") 'comint-previous-input)
  (define-key comint-mode-map (kbd "M-I") 'comint-next-input))

(my-after-load cc-mode
  (c-add-style
   "user" (list
           "gnu"
           '(c-offsets-alist
             (substatement-open 0)
             (arglist-intro 2)
             (arglist-close 0))))
  (c-add-style
   "awesome" (list
              "gnu"
              '(c-basic-offset . 4)
              '(c-offsets-alist
                (statement-case-intro 2)
                (case-label 2)
                (substatement-open 0)
                (arglist-intro 2)
                (arglist-close 0))))
  (my-add-hook c-mode-common (c-toggle-electric-state -1))
  (my-add-hook c-mode
    (when (and (stringp (buffer-file-name))
               (string-match "^/home/nex3/code/awesome/" (buffer-file-name)))
      (let ((c-buffer-is-cc-mode t))
        (c-set-style "awesome"))))

  (my-add-hook java-mode
    (when (string-match "^/home/nex3/hw/cse/473/slotcar/" (buffer-file-name))
      (setq indent-tabs-mode t)
      (setq tab-width 4))))

(my-after-load rcirc
  (require 'rcirc-color)
  (require 'rcirc-unambiguous-nick-completion)
  (require 'rcirc-notify)

  (setq rcirc-colors '("darkviolet" "magenta" "deeppink" "red" "yellow" "lawngreen"
                       "white" "LightSlateGrey" "RoyalBlue" "DeepSkyBlue" "LightSkyBlue"
                       "DarkOliveGreen" "PaleGreen" "ForestGreen" "LightGoldenrodYellow"
                       "sienna"))
  (setq rcirc-server-alist '(("irc.freenode.net" :channels ("#haml" "#rubyfringe" "#freehackersunion"))
                             ("irc.nex-3.com" :nick "Nathan" :channels ("#rc"))))
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
 
  (my-add-hook rcirc-mode
    (flyspell-mode 1)
    (rcirc-omit-mode))
  (define-key rcirc-mode-map (kbd "M-O") 'rcirc-insert-prev-input)
  (define-key rcirc-mode-map (kbd "M-I") 'rcirc-insert-next-input)
 
  (defun-rcirc-command raw (arg)
    "Send a raw string to the IRC server."
    (rcirc-send-string process arg)))

(my-after-load tex
  (with-temp-buffer (LaTeX-mode))
  (TeX-global-PDF-mode)
  (setcdr (assoc "^pdf$" TeX-output-view-style)
          '("." "evince %o")))

(my-after-load haskell-mode
  (my-add-hook haskell-mode turn-on-haskell-doc-mode)
  (my-add-hook haskell-mode turn-on-haskell-indent))

(my-after-load erlang
  (my-add-hook erlang-mode
    (setq inferior-erlang-machine-options '("-sname" "emacs")))
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
        (my-add-hook erlang-shell-mode
          (dolist (spec distel-shell-keys)
            (define-key erlang-shell-mode-map (car spec) (cadr spec)))))
    (file-error nil)))

(my-after-load gist
  (setq gist-view-gist t))

(my-after-load ruby-mode
  (defface ruby-tab-face
    '((((class color)) (:background "hotpink"))
      (t (:reverse-video t)))
    "Face to use for highlighting tabs in Ruby files."
    :group 'faces)

  (font-lock-add-keywords 'ruby-mode '(("\t" 0 'ruby-tab-face)))
  (setq ruby-deep-indent-paren-style nil)
  (my-add-hook ruby-mode
    (setq tab-width 2)
    (set-variable (make-variable-buffer-local 'whitespace-tab-width) 2)))

(my-after-load javascript-mode
  (setq javascript-auto-indent-flag nil))

(my-after-load fuel-mode
  (define-key fuel-mode-map "\M-." nil)
  (define-key fuel-mode-map "\M-," nil))

(my-after-load magit (require 'my-magit))

(when window-system
  (my-after-load ruby-mode
    (define-key ruby-mode-map (kbd "C-M-;") 'ruby-forward-sexp)
    (define-key ruby-mode-map (kbd "C-M-j") 'ruby-backward-sexp)))

;; /sudo:nex-3.com:
(my-after-load tramp
  (add-to-list 'tramp-default-proxies-alist
               '("\\`nex-3.com\\'" "\\`root\\'" "/ssh:%h:")))

(my-after-load markdown-mode
  (setq markdown-command "maruku -o /dev/stdout 2> /dev/null"))

(my-after-load compile
  (my-add-hook persp-mode
    (persp-make-variable-persp-local 'compile-history)
    (persp-make-variable-persp-local 'compile-command)))

(my-after-load caml
  (require 'caml-font)
  (define-key caml-mode-map (kbd "C-c C-b") 'caml-eval-buffer))

(my-add-hook text-mode flyspell-mode)
(my-add-hook lisp-mode pretty-lambdas)
(my-add-hook emacs-lisp-mode pretty-lambdas)

(define-key isearch-mode-map (kbd "M-n") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "M-O") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "M-I") 'isearch-ring-retreat)

;; ----------
;; -- Random Customizations and Configurations
;; ----------

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq make-backup-files nil)
(setq completion-ignored-extensions
      '(".a" ".so" ".o" "~" ".bak" ".class" ".hi" ".beam"))
(setq default-truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq column-number-mode t)
(setq normal-erase-is-backspace-mode 0)
(setq save-abbrevs nil)
(setq vc-follow-symlinks t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq windmove-wrap-around t)
(setq disabled-command-function nil)
(setq repeat-message-function 'ignore)
(setq fuel-factor-root-dir "~/src/factor")
(setq magit-save-some-buffers nil)
(setq magit-commit-all-when-nothing-staged t)
(setq magit-remote-ref-format 'remote-slash-branch)
(fset 'yes-or-no-p 'y-or-n-p)
(global-font-lock-mode 1)
(transient-mark-mode -1)

(unless (file-exists-p (format "/tmp/emacs%d/server" (user-uid)))
  (server-start))

(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(defadvice server-save-buffers-kill-terminal (around confirm-before-killing activate)
  "Confirm before killing a windowed emacsclient."
  (unless (and window-system
               (eq this-command 'save-buffers-kill-terminal)
               confirm-kill-emacs
               (not (funcall confirm-kill-emacs "Really kill terminal? ")))
    ad-do-it))

;; ----------
;; -- Useful Functions
;; ----------

(defun kill-whole-line-up (&optional n)
  "Kill current line, moving the cursor to the previous line.
See also `kill-whole-line'."
  (interactive "P")
  (kill-whole-line (- (or n 1))))

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
  (condition-case nil
      (persp-rename "irc")
    (error nil))
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

(defun* pretty-lambdas (&optional (regexp "(?\\(lambda\\>\\)"))
  "Make NAME render as λ."
  (font-lock-add-keywords
   nil `((,regexp
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun my-calc-embedded ()
  "Similar to `calc-embedded', but runs on the region and exits immediately."
  (interactive)
  (let ((text (buffer-substring (point) (mark))))
    (with-current-buffer (generate-new-buffer "thing")
      (LaTeX-mode)
      (insert "\\begin{document}\n$")
      (insert text)
      (insert "$\n\\end{document}")
      (forward-line -1)
      (end-of-line)
      (backward-char 1)
      (save-excursion
        (beginning-of-line)
        (replace-string "\\cdot" "*"))
      (calc-embedded nil)
      (calc-embedded nil)
      (let ((bol (save-excursion (beginning-of-line) (point)))
            (eol (save-excursion (end-of-line) (point))))
        (setq text (buffer-substring (+ bol 1) (- eol 1)))))
    (delete-region (point) (mark))
    (insert text)))

(defvar my-last-tag-was-search nil
  "Non-nil if the last tag lookup was a regexp search.")

(defun my-find-tag ()
  "Go to the tag at point.

The main differences between this and `find-tag' are that
this cycles through tags when used repeatedly and that
it doesn't prompt for a tag name."
  (interactive)
  (if (or (string-equal mode-name "Emacs-Lisp")
          (string-equal mode-name "Lisp Interaction"))
      (let* ((sym (let ((sym (variable-at-point t)))
                    (if (eq sym 0) nil sym)))
             (type (cond
                    ((facep sym) 'face)
                    ((functionp sym) 'function)
                    ((and sym (boundp sym)) 'variable)
                    (t (let ((fn (function-called-at-point)))
                         (when fn (setq sym fn) 'function)))))
             (file (find-lisp-object-file-name
                    sym (if (eq type 'function) (symbol-function sym) type))))
        (if type
            (funcall (button-type-get (intern (format "help-%s-def" type)) 'help-function)
                     sym file)
          (message "%S is not defined" sym)))
    (if (and last-tag (memq last-command (list this-command 'my-tag-search)))
        (find-tag last-tag t my-last-tag-was-search)
      (setq my-last-tag-was-search nil)
      (find-tag (funcall (or find-tag-default-function
                             (get major-mode 'find-tag-default-function)
                             'find-tag-default))))))

(defun my-tag-search (tagname)
  "Search for a tag as a regexp."
  (interactive (find-tag-interactive "Search for tag: "))
  (setq my-last-tag-was-search t)
  (find-tag tagname nil t))

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

(defmacro my-with-keymap (keymap &rest body)
  "Within BODY, `my-key' defines keys on KEYMAP by default."
  (declare (indent 1))
  `(let ((my-keymap ,keymap)) ,@body))

(defmacro my-key (key fn &optional map)
  `(define-key ,(or map 'my-keymap) (kbd ,key) ',fn))

(defmacro my-map (key name)
  (let ((varname (intern (concat (symbol-name name) "-map"))))
  `(progn
     (define-prefix-command ',name ',varname)
     (define-key my-keymap (kbd ,key) ,varname))))

(defmacro my-unset (key)
  `(global-unset-key (kbd ,key)))

(defmacro my-strong-unset (key)
  `(my-key ,key keyboard-quit))

;; -- Terminal hacks

(setf (cddr key-translation-map)
      (eval-when-compile
        (let ((chars (string-to-list "qwertyuiop]\\asdfghjklzxcvbnm")))
          (flet ((add-prefix (prefix char)
                             (read-kbd-macro (concat prefix "-" (char-to-string char))))
                 (make-bindings (binding-prefix)
                                (mapcar
                                 (lambda (char)
                                   (cons (string-to-char (add-prefix "C" char))
                                         (add-prefix binding-prefix char)))
                                 chars)))
            `((? keymap
                   (?& keymap
                       (?M keymap
                           (?\; . ,(kbd "C-M-;"))
                           (?. . ,(kbd "C-M-.")))
                       (?S keymap
                           (? keymap ,@(make-bindings "C-M-S")
                                (?: . ,(kbd "C-M-:")))
                           ,@(make-bindings "C-S")
                           (? . ,(kbd "C-_"))))))))))

;; -- Actual Bindings

(my-unset "C-x C-z")
(my-unset "C-x p")
(my-unset "C-]")

;; Ergonomic keybindings inspired by http://xahlee.org/emacs/ergonomic_emacs_keybinding.html

(my-map "M-d" my-delete)
(my-map "M-s" my-save)

(my-key "M-j" backward-char)
(my-key "M-;" forward-char)
(my-key "M-k" next-line)
(my-key "M-l" previous-line)

(my-key "M-J" backward-word)
(my-key "M-:" forward-word)
(my-key "M-K" forward-paragraph)
(my-key "M-L" backward-paragraph)

(my-key "M-u" beginning-of-line)
(my-key "M-p" end-of-line)
(my-key "M-P" end-of-buffer)
(my-key "M-U" beginning-of-buffer)

(my-with-keymap global-map
  (my-key "C-M-;" forward-sexp)
  (my-key "C-M-j" backward-sexp)
  (my-key "C-M-k" down-list)
  (my-key "C-M-l" backward-up-list)
  (my-key "C-M-o" beginning-of-defun)
  (my-key "M-TAB" end-of-defun))

(my-with-keymap emacs-lisp-mode-map
  (my-key "M-TAB" end-of-defun)
  (my-key "M-<tab>" lisp-complete-symbol))

(my-key "M-n" delete-backward-char)
(my-key "M-." delete-char)
(my-key "M-m" kill-whole-line)
(my-key "M-," kill-whole-line-up)

(my-key "M-N" backward-kill-word)
(my-key "M->" kill-word)
(my-key "M-M" kill-paragraph)
(my-key "M-<" backward-kill-paragraph)

(my-key "C-M-n" backward-kill-sexp)
(my-key "C-M-." kill-sexp)

(my-key "C-M-S-j" windmove-left)
(my-key "C-M-:" windmove-right)
(my-key "C-M-S-k" windmove-down)
(my-key "C-M-S-l" windmove-up)

(my-key "M-o" pager-page-up)
(my-key "M-i" pager-page-down)

(my-with-keymap minibuffer-local-map
  (my-key "M-O" previous-history-element)
  (my-key "M-I" next-history-element))

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

(my-key "M-\"" back-to-indentation)

(my-key "M-a" my-find-tag)
(my-key "M-A" my-tag-search)

(my-key "<M-S-return>" my-magit-status)

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
(my-key "C-n =" my-calc-embedded)
(my-key "C-n C" my-magithub-clone)

(my-map "C-n C-p" nex3-paste)
(my-key "C-n C-p p" gist-region)
(my-key "C-n C-p b" gist-buffer)
(my-key "C-n C-p g" gist-fetch)

(persp-mode)
(quick-perspective-keys)
