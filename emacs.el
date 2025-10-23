;; Natalie Weizenbaum's .emacs file

;; ----------
;; -- Do This First
;; ----------

(setq inhibit-startup-message t)

(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)

;; Add this to the load path explicitly so we can get the color theme up and
;; running as soon as possible. Loading packages the normal way can take time.
(add-to-list 'load-path (concat my-elisp-dir "elpa/color-theme-6.6.1"))

(when (< emacs-major-version 23) (require 'old-emacs))

(defun init-frame (&optional frame)
  "Initialize FRAME with my preferences."
  (setq frame (or frame (selected-frame)))
  (with-selected-frame frame
    ;; Set my font
    (when window-system
      (set-frame-font
       (cond
        ((string-equal system-type "windows-nt") "Consolas-11")
        ((and (>= (/ (+ (display-pixel-width) 0.0) (display-mm-width)) 3)
                (< (display-mm-width) 500))
         "Monospace-12")
        (t "Monospace-8.5")))
      (toggle-scroll-bar -1))))

(init-frame)
(add-hook 'after-make-frame-functions 'init-frame)

(when (or window-system (and (fboundp 'daemonp) (daemonp)))
  (setq color-theme-load-all-themes nil)
  (global-hl-line-mode t)

  (deftheme my-theme "Natalie's theme (based on Alexandre's)")
  (custom-theme-set-faces
   'my-theme

   '(default ((((min-colors 256)) (:foreground "pink" :background "black"))
              (t (:foreground "white" :background "black"))))

   ;; orange: #E67321
   ;; green: #00b200
   ;; dark blue: #6666FF
   ;; light blue: #398EE6
   '(font-lock-builtin-face ((t (:bold t :foreground "#E67321"))))
   '(font-lock-comment-face ((t (:italic t :bold t :foreground "#6666FF"))))
   '(font-lock-constant-face ((t (:bold t :foreground "#398EE6"))))
   '(font-lock-doc-string-face ((t (:bold t :foreground "#00b200"))))
   '(font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))
   '(font-lock-keyword-face ((t (:bold t :foreground "#E67321"))))
   '(font-lock-preprocessor-face ((t (:foreground "#2B2BFF" :bold t))))
   '(font-lock-reference-face ((t (:foreground "red3"))))
   '(font-lock-string-face ((t (:bold t :foreground "#00b200"))))
   '(font-lock-type-face ((t (:bold t :foreground "#398EE6"))))
   '(font-lock-variable-name-face ((t (:italic t :bold t :foreground "magenta3"))))
   '(font-lock-warning-face ((t (:bold t :foreground "red"))))
   '(py-builtins-face ((t (:bold t :foreground "#398EE6"))))
   '(py-pseudo-keyword-face ((t (:bold t :foreground "#398EE6"))))
   '(rst-level-1-face ((t (:bold t :foreground "snow1"))))
   '(rst-level-2-face ((t (:bold t :foreground "snow2"))))
   '(rst-level-3-face ((t (:bold t :foreground "snow3"))))
   '(rst-level-4-face ((t (:bold t :foreground "snow4"))))
   '(erc-action-face ((t (nil))))
   '(erc-notice-face ((t (:foreground "#878899"))))
   '(erc-bold-face ((t (:bold t :weight bold))))
   '(erc-command-indicator-face ((t (:bold t :weight bold))))
   '(erc-dangerous-host-face ((t (:foreground "red"))))
   '(erc-default-face ((t (nil))))
   '(erc-timestamp-face ((t (:bold nil :foreground "gray45" :weight normal))))
   '(erc-underline-face ((t (:underline t))))
   '(erc-prompt-face ((t (:bold t :foreground "GoldenRod3" :weight bold))))
   '(trailing-whitespace ((t (:background "gray30"))))
   '(term-color-blue ((t (:foreground "#7B7BFF"))))
   ;; Don't highlight lines in the terminal
   '(highlight ((t (:background "gray10"))))
   '(hl-line ((((min-colors 256)) (:inherit highlight))
              (((min-colors 8)) (:inherit nil :background nil))))
   '(mode-line ((t (:background "gray80" :foreground "gray20"))))
   '(magit-item-highlight ((t (:background "#222222"))))
   '(markdown-inline-code-face ((t (:foreground "cyan3"))))
   '(markdown-markup-face ((t (:foreground "pink3"))))
   '(markdown-reference-face ((t (:inherit markdown-markup-face)))))
  (enable-theme 'my-theme))

;; ----------
;; -- Loading Modules
;; ----------

(when (boundp 'package-archives)
  (push '("melpa" . "http://melpa.org/packages/") package-archives)
  (push '("melpa-stable" . "http://stable.melpa.org/packages/") package-archives))

(require 'pager)

(persp-mode)

(setq frame-title-format '("Emacs: %b [" (:eval (persp-name (persp-curr))) "]"))

(defun load-mode (name regexp)
  "Set up a language mode NAME-mode so that
it's loaded for files matching REGEXP."
  (add-to-list 'auto-mode-alist (cons regexp (intern (format "%s-mode" name)))))

;; This is here because it would require loading a bunch of MuMaMo macros
;; to have the autoload defined in the file.
(autoload 'nxhtml-mumamo-mode "nxhtml-mumamo.el")

(autoload 'subword-forward "subword.el")
(autoload 'subword-backward "subword.el")
(autoload 'subword-kill "subword.el")
(autoload 'subword-backward-kill "subword.el")

(load-mode 'gfm "\\.\\(markdown\\|md\\)\\(\\.erb\\)?$")
(load-mode 'sass "\\.sass$")
(load-mode 'yaml "\\.ya?ml$")
(load-mode 'ruby "\\(\\.\\(rb\\|rake\\|rjs\\|duby\\|gemspec\\|thor\\)\\|Rakefile\\|Capfile\\|Thorfile\\)$")
(load-mode 'css "\\.css$")
(load-mode 'js "\\.m?js$")

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
  (let ((name (format "%s" name))
        (append (when (eq (car body) :append)
                  (pop body)
                  t)))
    `(add-hook ',(intern
                  (if (string-match "-hook$" name) name
                    (format "%s-hook" name)))
               ,(if (and (eq (length body) 1)
                         (symbolp (car body)))
                    (list 'quote (car body))
                  `(lambda () ,@body))
               ,append)))

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
        (c-set-style "awesome")))))

(my-after-load dart-mode
  (my-add-hook dart-mode
    (c-set-style "dart")))

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

(my-after-load js-mode
  (setq js-auto-indent-flag nil))

(my-after-load magit
  (require 'my-magit))

(my-after-load term
  (require 'my-term))

(my-after-load git-commit
  ;; Unbind next-message and prev-message bindings that conflict with my custom
  ;; bindings.
  (define-key git-commit-mode-map (kbd "M-p") nil)
  (define-key git-commit-mode-map (kbd "M-n") nil)

  (my-add-hook git-commit-mode
    (set (make-local-variable 'whitespace-style) '(lines-tail face))
    (set (make-local-variable 'whitespace-line-column) 70)
    (set (make-local-variable 'fill-column) 70)
    (whitespace-mode)))

(my-after-load git-rebase
  (define-key git-rebase-mode-map (kbd "M-L") 'git-rebase-move-line-up)
  (define-key git-rebase-mode-map (kbd "M-K") 'git-rebase-move-line-up))

(my-after-load scss-mode
  (setq scss-compile-at-save nil))

(when window-system
  (my-after-load ruby-mode
    (define-key ruby-mode-map (kbd "C-M-;") 'ruby-forward-sexp)
    (define-key ruby-mode-map (kbd "C-M-j") 'ruby-backward-sexp)))

;; /sudo:nex-3.com:
(my-after-load tramp
  (add-to-list 'tramp-default-proxies-alist
               '("\\`nex-3.com\\'" "\\`root\\'" "/ssh:%h:")))

(my-after-load compile
  (persp-make-variable-persp-local 'compile-history)
  (persp-make-variable-persp-local 'compile-command))

(my-after-load markdown-mode
  (setq markdown-spaces-after-code-fence 0)
  (my-add-hook markdown-mode
    (setq markdown-gfm-use-electric-backquote nil)
    (toggle-word-wrap 1)
    (toggle-truncate-lines -1)))

(my-after-load package
  (defun my-package-get-desc (package)
    "Return the description of PACKAGE.
PACKAGE may be a desc or a package name."
    (if (package-desc-p package) package
      (assq (if (symbolp package) package (intern package)) package-alist)))

  (defun my-package-latest-version (package)
    "Return the latest version number of `package'."
    (mapconcat #'number-to-string
               (package-desc-version (my-package-get-desc package))
               "."))

  (defun my-commit-package (package)
    "Commit the latest version of `package'."
    (my-commit-config
     (format "[Emacs] Add %s version %s."
             (package-desc-name package)
             (my-package-latest-version package))))

  (defadvice package-install (after my-commit-package-install (pkg &optional dont-select) activate)
    (my-commit-package pkg))

  (defadvice package-delete (after my-commit-package-delete (pkg-desc &optional force nosave) activate)
    (my-commit-config
     (format "[Emacs] Delete %s version %s."
             (package-desc-name pkg)
             (package-desc-version pkg-desc)))))

(my-after-load eshell
  (persp-make-variable-persp-local 'eshell-buffer-name)
  (my-add-hook persp-created
    (setq eshell-buffer-name (format "*eshell* (%s)" (persp-name (persp-curr))))))

(my-after-load grep
  (my-add-hook grep-mode
    (make-variable-buffer-local 'compilation-save-buffers-predicate)
    (setq compilation-save-buffers-predicate (lambda () nil))))

(my-after-load js
  (setq js-indent-level 2)
  (setq typescript-indent-level 2))

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
(setq auto-save-default nil)
(setq create-lockfiles nil)
(set-default 'truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq column-number-mode t)
(setq normal-erase-is-backspace-mode 0)
(setq save-abbrevs nil)
(setq vc-follow-symlinks t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq windmove-wrap-around t)
(setq disabled-command-function nil)
(setq repeat-message-function 'ignore)
(setq magit-save-some-buffers nil)
(setq magit-stage-all-confirm nil)
(setq magit-ask-to-stage nil)
(setq magit-unstage-all-confirm nil)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-status-buffer-switch-function 'switch-to-buffer)
(setq magit-bury-buffer-function 'quit-window)
(setq electric-indent-mode nil)
(setq ring-bell-function 'ignore)
(set-default 'fill-column 80)
(setq sentence-end-double-space nil)
(setq x-select-enable-primary t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-font-lock-mode 1)
(transient-mark-mode -1)

(when (boundp 'mac-function-modifier)
  (setq mac-command-modifier 'meta)
  (setq mac-function-modifier 'control))

(require 'server)
(unless (server-running-p server-name)
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

(defvar my-confirm-before-deleting nil
  "Confirm before deleting a frame. Meant to be let-bound by advice.")

(defadvice delete-frame (around confirm-before-deleting activate)
  "Confirm before deleting a frame if `my-confirm-before-deleting' is set."
  (unless (and my-confirm-before-deleting
               confirm-kill-emacs
               (not (funcall confirm-kill-emacs "Really delete frame? ")))
    ad-do-it))

(defadvice handle-delete-frame (around confirm-before-deleting activate)
  "Confirm before deleting a frame because of an X event."
  (let ((my-confirm-before-deleting t))
    ad-do-it))

;; ----------
;; -- Useful Functions
;; ----------

(defun kill-whole-line-up (&optional n)
  "Kill current line, moving the cursor to the previous line.
See also `kill-whole-line'."
  (interactive "P")
  (kill-whole-line (- (or n 1))))

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

(defun make-directory-from-minibuffer ()
  "Create a directory at the location given by the minibuffer,
which should be selected."
  (interactive)
  (make-directory (minibuffer-contents) t)
  (princ (concat "Created directory " (minibuffer-contents))))

(cl-defun pretty-lambdas (&optional (regexp "(?\\(lambda\\>\\)"))
  "Make NAME render as λ."
  (font-lock-add-keywords
   nil `((,regexp
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

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

(defun my-comment-indent-new-line ()
  "Like `comment-indent-new-line', but adds a prefix for block comments as well."
  (interactive)
  (if (save-excursion
        (or
         (not comment-start-skip)
         (and (re-search-backward comment-start-skip nil t)
              (looking-at (if (functionp 'comment-string-strip)
                              (comment-string-strip comment-start t t)
                            comment-start)))))
      ;; We're looking at a single-line comment,
      ;; which comment-indent-new-line can handle.
      (comment-indent-new-line)
    ;; We're looking at a multiline comment,
    ;; which confuses comment-indent-new-line.
    (newline-and-indent)
    (when (and comment-start
               comment-multi-line
               c-block-comment-prefix
               (save-excursion (comment-beginning)))
      (insert c-block-comment-prefix))
    (indent-according-to-mode)))

(defun my-commit-config (message)
  "Commit a change to the config repo."
  (save-window-excursion
    (magit-status (file-name-directory (file-chase-links my-elisp-dir)))
    (magit-run-git "add" "-A" "elisp/elpa")
    (magit-run-git "commit" "-m" message)))

(defconst my-git-commit-filename-regexp "/\\(\
\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\
\\|BRANCH_DESCRIPTION\\|cl_description[a-zA-Z0-9_]+\\)\\'")

(defun my-load-git-commit ()
  "Load git-commit and replace its setup function."
  (require 'git-commit)
  (remove-hook 'find-file-hook 'my-load-git-commit)

  (defun git-commit-setup-check-buffer ()
    (and buffer-file-name
       (string-match-p my-git-commit-filename-regexp buffer-file-name)
       (git-commit-setup)))

  (defun git-commit-setup-font-lock-in-buffer ()
    (and buffer-file-name
         (string-match-p my-git-commit-filename-regexp buffer-file-name)
         (git-commit-setup-font-lock)))

  (git-commit-setup-check-buffer))
(add-hook 'find-file-hook 'my-load-git-commit)

(defun my-shell-command (command &optional output-buffer)
  "Like `shell-command', but automatically inserts the current file name."
  (interactive
   (list
    (minibuffer-with-setup-hook
        (lambda ()
          (beginning-of-line)
          (insert " ")
          (beginning-of-line))
      (read-shell-command "Shell command: "
                          (let ((filename
                                 (cond
                                  (buffer-file-name)
                                  ((eq major-mode 'dired-mode)
                                   (dired-get-filename nil t)))))
                            (and filename (file-relative-name filename)))))
    current-prefix-arg))
  (shell-command command output-buffer))

(defun my-insert-iso-8601 ()
  "Inserts the current time in JS-compatible ISO-8601 format."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%TZ" nil t)))

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

(defvar my-default-keymap
  (make-composed-keymap (list my-keymap (current-global-map)))
  "A keymap that combines `my-keymap' and `current-global-map'.")

;; -- Terminal hacks
;;
;; The input standard for terminals, having evolved in a time before graphical
;; interfaces, doesn't have nearly the same level of support for modifier keys
;; as X does. Since I use all sorts of modifier keys, often in combination with
;; one another, this presents a problem. How do I get bindings involving two or
;; more of control, meta, and shift to work?
;;
;; Luckily, the X toolkit and xterm (which uses it) are vastly customizable.
;; Through the Xresources config file, which is fed to xrdb, I can tell xterm
;; that any key event (involving as many modifiers as I want) should produce
;; any arbitrary string. In addition, Emacs has low-level but lisp-accessible
;; facilities for saying that a particular string of characters should translate
;; to a particular key sequence.

(setf (cddr key-translation-map)
      (eval-when-compile
        (let ((chars (string-to-list "qwertyuiop]\\asdfghjklzxcvbnm"))
              (meta (string-to-char (kbd "ESC"))))
          (cl-flet* ((add-prefix (prefix char)
                       (concat prefix "-" (char-to-string char)))
                     (make-bindings (binding-prefix)
                       (mapcar
                        (lambda (char)
                          (cons (aref (read-kbd-macro (add-prefix "C" char) t) 0)
                                (kbd (add-prefix binding-prefix char))))
                        chars)))
            ;; C-M-S- bindings are a slightly weird case. None of them work by
            ;; default, so we have to bind them en masse. In addition, they're
            ;; actually included in two of the subdivisions below. The A keymap
            ;; is the "correct" keymap for them, but they also appear in the S
            ;; keymap. This is because, since we emit the S prefix for *all*
            ;; C-S- characters, typing C-S-M (in that order) will generate the S
            ;; prefix before generating the full prefix. Thus we have to process
            ;; the A prefix *after* processing the S sequence.
            (let ((cms-bindings
                   `(,meta keymap ,@(make-bindings "C-M-S")
                           (?: . ,(kbd "C-M-:"))
                           (?\s . ,(kbd "C-M-S-SPC")))))
              ;; I reserve the prefix M-& for my translation map, because it's a
              ;; pain to reach anyway.
              `((,meta keymap
                       (?& keymap
                           ;; The C keymap handles C-M- bindings. There are only
                           ;; a couple of these, since C-M- works without issue
                           ;; for alphabetic characters.
                           (?C keymap
                               (?\; . ,(kbd "C-M-;"))
                               (?. . ,(kbd "C-M-.")))
                           ;; The M keymap handles M-S- bindings. There's only
                           ;; one of these, since M-S- works almost all the time.
                           (?M keymap
                               (?\s . ,(kbd "M-S-SPC")))
                           ;; The S keymap handles C-S- and (sometimes) C-M-S
                           ;; bindings. All C-S- bindings we care about don't
                           ;; work, so we bind them en masse.
                           (?S keymap
                               (,meta keymap (?& keymap (?A keymap ,cms-bindings)))
                               ,@(make-bindings "C-S"))
                           ;; The A keymap (sometimes) handles C-M-S- bindings.
                           (?A keymap
                               ,cms-bindings)))))))))

;; -- Actual Bindings

(my-unset "C-x C-z")
(my-unset "C-x p")
(my-unset "C-]")
(my-unset "M-&") ;; Necessary for terminal hacks

;; Ergonomic keybindings inspired by http://xahlee.org/emacs/ergonomic_emacs_keybinding.html

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

(my-key "M-s-;" subword-forward)
(my-key "M-s-j" subword-backward)
(my-key "M-s-." subword-kill)
(my-key "M-s-n" subword-backward-kill)

(my-key "C-M-S-j" windmove-left)
(my-key "C-M-:" windmove-right)
(my-key "C-M-S-k" windmove-down)
(my-key "C-M-S-l" windmove-up)

(my-key "M-o" pager-page-up)
(my-key "M-i" pager-page-down)

(my-with-keymap minibuffer-local-map
  (my-key "M-O" previous-history-element)
  (my-key "M-I" next-history-element))

(my-key "M-RET" my-comment-indent-new-line)
(my-key "C-v" x-clipboard-only-yank)
(my-key "C-z" clipboard-kill-region)

(my-key "M-SPC" set-mark-command)
(my-key "C-M-SPC" kill-region)
(my-key "C-M-@" kill-region)
(my-key "M-S-SPC" yank)
(my-key "C-M-S-SPC" yank-pop)

(my-key "M-'" execute-extended-command)
(my-key "M-/" hippie-expand)
(my-key "M-?" undo)

(my-key "M-\"" back-to-indentation)

(my-key "M-a" my-find-tag)
(my-key "M-A" my-tag-search)

(my-key "<M-S-return>" my-magit-status)
(my-key "<C-S-return>" my-term)

(my-key "C-M-!" my-shell-command)

(define-key my-keymap (kbd "M-S-s-SPC")
  (lambda () (interactive) (insert-register ?\s t)))

(ffap-bindings)

;; Cold Turkey

(my-unset "C-w")
(my-unset "C-y")
(my-unset "C-_")
(my-unset "M-y")
(my-unset "M-x")

;; My Keymap

(my-map "C-n" nex3)
(my-key "C-n ." .emacs)
(my-key "C-n c" comment-region)
(my-key "C-n u" uncomment-region)
(my-key "C-n m" make-directory-from-minibuffer)
(my-key "C-n f" auto-fill-mode)

(quick-perspective-keys)
(my-key "C-S-SPC" persp-switch-last)
