;;; -*- Mode: Emacs-Lisp -*-
;;; my color theme and other fancy stuff
;;;

(defun my-color-theme-dark ()
  (interactive)
  ;; highlight the line where the cursor is
  (global-hl-line-mode t)
  (when (facep 'hl-line)
    (set-face-background 'hl-line "#222222")
    (set-face-foreground 'hl-line nil))
  ;; main theme
  (color-theme-install
   '(my-color-theme-dark
     ((background-color . "black")
      (cursor-color . "white")
      (mouse-color . "white")
      (background-mode . dark))
     (default ((t (nil))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (italic ((t (:italic t))))
     ;; orange: #E67321
     ;; green: #00b200
     ;; dark blue: #2B2BFF
     ;; light blue: #398EE6
     (font-lock-builtin-face ((t (:bold t :foreground "#E67321"))))
     (font-lock-comment-face ((t (:italic t :bold t :foreground "#2B2BFF"))))
     (font-lock-constant-face ((t (:bold t :foreground "#398EE6"))))
     (font-lock-doc-string-face ((t (:bold t :foreground "#00b200"))))
     (font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))
     (font-lock-keyword-face ((t (:bold t :foreground "#E67321"))))
     (font-lock-preprocessor-face ((t (:foreground "#2B2BFF" :bold t))))
     (font-lock-reference-face ((t (:foreground "red3"))))
     (font-lock-string-face ((t (:bold t :foreground "#00b200"))))
     (font-lock-type-face ((t (:bold t :foreground "#398EE6"))))
     (font-lock-variable-name-face ((t (:italic t :bold t :foreground "magenta3"))))
     (font-lock-warning-face ((t (:bold t :foreground "red"))))
     (py-builtins-face ((t (:bold t :foreground "#398EE6"))))
     (py-pseudo-keyword-face ((t (:bold t :foreground "#398EE6"))))
     (rst-level-1-face ((t (:bold t :foreground "snow1"))))
     (rst-level-2-face ((t (:bold t :foreground "snow2"))))
     (rst-level-3-face ((t (:bold t :foreground "snow3"))))
     (rst-level-4-face ((t (:bold t :foreground "snow4"))))
     (erc-action-face ((t (nil))))
     (erc-notice-face ((t (:foreground "#878899"))))
     (erc-bold-face ((t (:bold t :weight bold))))
     (erc-command-indicator-face ((t (:bold t :weight bold))))
     (erc-dangerous-host-face ((t (:foreground "red"))))
     (erc-default-face ((t (nil))))
     (erc-timestamp-face ((t (:bold nil :foreground "gray45" :weight normal))))
     (erc-underline-face ((t (:underline t))))
     (erc-prompt-face ((t (:bold t :foreground "GoldenRod3" :weight bold))))
     (trailing-whitespace ((t (:background "gray30")))))))

(defun my-color-theme-light ()
  (interactive)
  ;; main theme
  (color-theme-install
   '(my-color-theme-light
     ((background-color . "white")
      (foreground-color . "black")
      (cursor-color . "black")
      (mouse-color . "white")
      (background-mode . light))
     (default ((t (nil))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (italic ((t (:italic t))))
     ;; orange: #E67321
     ;; purple: #8722c9
     ;; green: #00b200
     ;; blue: #398EE6
     (font-lock-builtin-face ((t (:bold t :foreground "#E67321"))))
     (font-lock-comment-face ((t (:italic t :bold t :foreground "#8722c9"))))
     (font-lock-constant-face ((t (:bold t :foreground "#398EE6"))))
     (font-lock-doc-string-face ((t (:bold t :foreground "#51B200"))))
     (font-lock-function-name-face ((t (:bold t))))
     (font-lock-keyword-face ((t (:bold t :foreground "#E67321"))))
     (font-lock-preprocessor-face ((t (:foreground "#8722c9" :bold t))))
     (font-lock-reference-face ((t (:foreground "red3"))))
     (font-lock-string-face ((t (:bold t :foreground "#51B200"))))
     (font-lock-type-face ((t (:bold t :foreground "#398EE6"))))
     (font-lock-variable-name-face ((t (:italic t :bold t :foreground "magenta3"))))
     (font-lock-warning-face ((t (:bold t :foreground "red"))))
     (py-builtins-face ((t (:bold t :foreground "#398EE6"))))
     (py-pseudo-keyword-face ((t (:bold t :foreground "#398EE6"))))
     (rst-level-1-face ((t (:bold t :foreground "snow1"))))
     (rst-level-2-face ((t (:bold t :foreground "snow2"))))
     (rst-level-3-face ((t (:bold t :foreground "snow3"))))
     (rst-level-4-face ((t (:bold t :foreground "snow4"))))
     (erc-action-face ((t (nil))))
     (erc-notice-face ((t (:foreground "#878899"))))
     (erc-bold-face ((t (:bold t :weight bold))))
     (erc-command-indicator-face ((t (:bold t :weight bold))))
     (erc-dangerous-host-face ((t (:foreground "red"))))
     (erc-default-face ((t (nil))))
     (erc-timestamp-face ((t (:bold nil :foreground "gray45" :weight normal))))
     (erc-underline-face ((t (:underline t))))
     (erc-prompt-face ((t (:bold t :foreground "GoldenRod3" :weight bold))))
     (trailing-whitespace ((t (:background "gray90")))))))

(defun my-color-theme-dull ()
  (interactive)
  ;; main theme
  (color-theme-install
   '(my-color-theme-light
     ((background-color . "white")
      (foreground-color . "black")
      (cursor-color . "black")
      (mouse-color . "white")
      (background-mode . light))
     (default ((t (nil))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (italic ((t (:italic t))))
     ;; red earth: #A73232
     ;; purple: #8722c9
     ;; light green: #228C00
     ;; teal: #008080
     (font-lock-builtin-face ((t (:bold t :foreground "#A73232"))))
     (font-lock-comment-face ((t (:foreground "#8722c9"))))
     (font-lock-constant-face ((t (:bold t :foreground "#398EE6"))))
     (font-lock-doc-string-face ((t (:bold t :foreground "#51B200"))))
     (font-lock-function-name-face ((t (:bold t))))
     (font-lock-keyword-face ((t (:bold t :foreground "#A73232"))))
     (font-lock-preprocessor-face ((t (:foreground "#8722c9" :bold t))))
     (font-lock-reference-face ((t (:foreground "red3"))))
     (font-lock-string-face ((t (:foreground "#228C00"))))
     (font-lock-type-face ((t (:bold t :foreground "#008080"))))
     (font-lock-variable-name-face ((t (:foreground "black"))))
     (font-lock-warning-face ((t (:bold t :foreground "red"))))
     (py-builtins-face ((t (:bold t :foreground "#398EE6"))))
     (py-pseudo-keyword-face ((t (:bold t :foreground "#398EE6"))))
     (rst-level-1-face ((t (:bold t :foreground "snow1"))))
     (rst-level-2-face ((t (:bold t :foreground "snow2"))))
     (rst-level-3-face ((t (:bold t :foreground "snow3"))))
     (rst-level-4-face ((t (:bold t :foreground "snow4"))))
     (erc-action-face ((t (nil))))
     (erc-notice-face ((t (:foreground "#878899"))))
     (erc-bold-face ((t (:bold t :weight bold))))
     (erc-command-indicator-face ((t (:bold t :weight bold))))
     (erc-dangerous-host-face ((t (:foreground "red"))))
     (erc-default-face ((t (nil))))
     (erc-timestamp-face ((t (:bold nil :foreground "gray45" :weight normal))))
     (erc-underline-face ((t (:underline t))))
     (erc-prompt-face ((t (:bold t :foreground "GoldenRod3" :weight bold))))
     (trailing-whitespace ((t (:background "gray90")))))))

