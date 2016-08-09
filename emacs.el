;; Natalie Weizenbaum's .emacs file

;; ----------
;; -- Do This First
;; ----------

(setq inhibit-startup-message t)

(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)

(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path "/usr/share/emacs-snapshot/site-lisp")
(add-to-list 'load-path "~/.elisp")
(add-to-list 'load-path "~/share/emacs/site-lisp")

;; Add this to the load path explicitly so we can get the color theme up and
;; running as soon as possible. Loading packages the normal way can take time.
(add-to-list 'load-path "~/.elisp/elpa/color-theme-6.6.1")


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

(when (or window-system (and (fboundp 'daemonp) (daemonp)))
  (setq color-theme-load-all-themes nil)
  (require 'color-theme)

  (load "alexandres-theme")
  (my-color-theme-dark)

  (custom-set-faces
   '(default ((((min-colors 256)) (:foreground "pink"))
              (t (:foreground "white"))))
   ;; Don't highlight lines in the terminal
   '(font-lock-comment-face ((t (:italic t :bold t :foreground "#4B4BFF"))))
   '(hl-line ((((min-colors 256)) (:inherit highlight))
              (((min-colors 8)) (:inherit nil :background nil))))
   '(mode-line ((t (:background "gray80" :foreground "gray20" :box (:line-width -1 :style "released-button")))))
   '(magit-item-highlight ((t (:background "#222222"))))))

;; ----------
;; -- Loading Modules
;; ----------

(load "my-loaddefs")
(require 'package)

(when (boundp 'package-archives)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/")))
(setq package-user-dir "~/.elisp/elpa")
(package-initialize)

(require 'pager)
(require 'tex-site)
(eval-when-compile (require 'cl))

(persp-mode)

(setq frame-title-format '("Emacs: %b [" (:eval (persp-name persp-curr)) "]"))

(defun load-mode (name regexp)
  "Set up a language mode NAME-mode so that
it's loaded for files matching REGEXP."
  (add-to-list 'auto-mode-alist (cons regexp (intern (format "%s-mode" name)))))

;; This is here because it would require loading a bunch of MuMaMo macros
;; to have the autoload defined in the file.
(autoload 'nxhtml-mumamo-mode "nxhtml-mumamo.el")

(autoload 'subword-forward "subword.el")
(autoload 'subword-backward "subword.el")

(load-mode 'markdown "\\.\\(markdown\\|md\\)$")
(load-mode 'sass "\\.sass$")
(load-mode 'yaml "\\.ya?ml$")
(load-mode 'ruby "\\(\\.\\(rb\\|rake\\|rjs\\|duby\\|gemspec\\|thor\\)\\|Rakefile\\|Capfile\\|Thorfile\\)$")
(load-mode 'css "\\.css$")

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
        (c-set-style "awesome"))))

  (my-add-hook java-mode
    (when (string-match "^/home/nex3/hw/cse/473/slotcar/" (buffer-file-name))
      (setq indent-tabs-mode t)
      (setq tab-width 4))))

(my-after-load dart-mode
  (setq dart-enable-analysis-server t)
  (dart-start-analysis-server)
  (my-add-hook dart-mode
    (c-set-style "dart")
    (flycheck-mode)))

(my-after-load tex
  (with-temp-buffer (LaTeX-mode))
  (TeX-global-PDF-mode)
  (setcdr (assoc "^pdf$" TeX-output-view-style)
          '("." "evince %o")))

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

(my-after-load git-commit
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

(my-after-load markdown-mode
  (setq markdown-command "maruku -o /dev/stdout 2> /dev/null"))

(my-after-load compile
  (persp-make-variable-persp-local 'compile-history)
  (persp-make-variable-persp-local 'compile-command))

(my-after-load markdown-mode
  (my-add-hook markdown-mode
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
               (aref (cadr (my-package-get-desc package)) 2)
               "."))

  (defun my-commit-package (package)
    "Commit the latest version of `package'."
    (my-commit-config
     (format "[Emacs] Add %s version %s."
             package
             (my-package-latest-version package))))

  (defadvice package-install (after my-commit-package-install (name) activate)
    (my-commit-package name))

  (defadvice package-delete (after my-commit-package-delete (name version) activate)
    (my-commit-config
     (format "[Emacs] Delete %s version %s." name version))))

(my-after-load eshell
  (persp-make-variable-persp-local 'eshell-buffer-name)
  (my-add-hook persp-created
    (setq eshell-buffer-name (format "*eshell* (%s)" (persp-name persp-curr)))))

(my-after-load flymake
  (require 'flymake-cursor))

(my-after-load grep
  (my-add-hook grep-mode
    (make-variable-buffer-local 'compilation-save-buffers-predicate)
    (setq compilation-save-buffers-predicate (lambda () nil))))


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
(setq magit-save-some-buffers nil)
(setq magit-stage-all-confirm nil)
(setq magit-ask-to-stage nil)
(setq magit-unstage-all-confirm nil)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-status-buffer-switch-function 'switch-to-buffer)
(setq electric-indent-mode nil)
(set-default 'fill-column 80)
(setq sentence-end-double-space nil)
(setq x-select-enable-primary t)
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

(defun my-comment-indent-new-line ()
  "Like `comment-indent-new-line', but adds a prefix for block comments as well."
  (interactive)
  (if (save-excursion
        (and (re-search-backward comment-start-skip nil t)
             (looking-at (if (functionp 'comment-string-strip)
                             (comment-string-strip comment-start t t)
                           comment-start))))
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

(defun my-info (file-or-node &optional buffer)
  "Like `info', but with a better interactive interface.
When called interactively, reads an info node rather than an info
filename.

Treats prefix args in the same way as `info'."
  (interactive (list
                (if (and current-prefix-arg (not (numberp current-prefix-arg)))
                    (read-file-name "Info file name: " nil nil t)
                  (require 'magithub)
                  (completing-read "Info node: " (my-complete-info-node-callback)))
                (if (numberp current-prefix-arg)
                    (format "*info*<%s>" current-prefix-arg))))
  (info file-or-node buffer))

(defun my-complete-info-node-callback ()
  "Creates a callback that caches completions for info nodes."
  (lexical-let ((cached-info-nodes-for-file
                 (magithub--cache-function 'my-info-nodes-for-file))
                (cached-info-get-files
                 (magithub--cache-function 'my-info-get-files)))
    (lambda (string predicate allp)
      (let (new-string list)
        (if (string-match "(\\([^)]+\\))\\(.*\\)" string)
            (progn
              (setq new-string string)
              (setq list (funcall cached-info-nodes-for-file (match-string 1 string))))
          (setq new-string
                (concat "(" (if (string-match "(\\([^)]*\\)" string)
                                (match-string 1 string)
                              string)))
          (setq list (funcall cached-info-get-files)))
        (if allp (all-completions new-string list predicate)
          (try-completion new-string list predicate))))))

(defun my-info-nodes-for-file (filename)
  "Return a list of info nodes in FILENAME.
These are in the format (FILENAME)NODENAME."
  (save-window-excursion
    (with-temp-buffer
      (info filename (current-buffer))
      (mapcar (lambda (nodename) (concat "(" filename ")" (car nodename)))
              (Info-build-node-completions)))))

(defun my-info-get-files ()
  "Return a list of all available top-level info files as strings."
  (let ((ext-regexp "\\(\\.\\(info\\|gz\\|bz2\\|xz\\|lzma\\)\\)*"))
    (loop for dir in Info-directory-list
          append (loop for file in (directory-files dir 'full)
                       unless (or (file-directory-p file)
                                  (string-match (concat ext-regexp "-[0-9]+" ext-regexp  "$") file)
                                  (string-match (concat "/dir$") file))
                       collect (concat "("
                                       (string-replace-match
                                        (concat ext-regexp "$")
                                        (file-name-nondirectory file)
                                        "") ")")))))

(defun my-eshell-new-shell ()
  "Create a new eshell."
  (interactive)
  (eshell 'new-shell))

(defun my-commit-config (message)
  "Commit a change to the config repo."
  (save-window-excursion
    (magit-status (file-name-directory (file-chase-links "~/.elisp")))
    (magit-run-git "add" "-A" "elisp/elpa")
    (magit-run-git "commit" "-m" message)))

(defun my-count-words ()
  "Count words, ignoring footnotes and links."
  (interactive)
  (save-excursion
    (end-of-buffer)
    (let ((last-real-line (save-excursion
                            (re-search-backward "^[^0-9 \n]")
                            (point))))
      (re-search-backward "^1\. " last-real-line t)
      (let ((contents-no-footnotes
             (buffer-substring-no-properties
              (save-excursion (beginning-of-buffer) (point))
              (point))))
        (with-temp-buffer
          (insert contents-no-footnotes)
          (beginning-of-buffer)
          (replace-regexp "\\[[0-9]+\\]" "")
          (replace-regexp "\\][(\\[][^)]+[)\\]]" "")
          (replace-regexp "^\\[[^]]+\\]: .*$" "")
          (count-words))))))

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
          (flet ((add-prefix (prefix char)
                             (read-kbd-macro (concat prefix "-" (char-to-string char))))
                 (make-bindings (binding-prefix)
                                (mapcar
                                 (lambda (char)
                                   (cons (string-to-char (add-prefix "C" char))
                                         (add-prefix binding-prefix char)))
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
(my-key "<C-S-return>" my-eshell-new-shell)

;; Cold Turkey

(my-unset "C-w")
(my-unset "C-y")
(my-unset "C-_")
(my-unset "M-y")
(my-unset "M-x")

;; My Keymap

(my-map "C-n" nex3)
(my-key "C-n ." .emacs)
(my-key "C-n i" my-info)
(my-key "C-n b" blog)
(my-key "C-n c" comment-region)
(my-key "C-n u" uncomment-region)
(my-key "C-n m" make-directory-from-minibuffer)
(my-key "C-n f" auto-fill-mode)
(my-key "C-n =" my-calc-embedded)
(my-key "C-n C" my-magithub-clone)

(my-map "C-n C-p" nex3-paste)
(my-key "C-n C-p p" gist-region)
(my-key "C-n C-p b" gist-buffer)
(my-key "C-n C-p g" gist-fetch)

(quick-perspective-keys)
