(defvar treetop-mode-hook nil)

(defvar treetop-mode-map
        (let ((treetop-mode-map (make-keymap)))
             (define-key treetop-mode-map "\C-j" 'newline-and-indent)
             treetop-mode-map)
             "Keymap for treetop major mode")

(add-to-list 'auto-mode-alist '("\\.treetop\\'" . treetop-mode))

(defconst treetop-font-lock-keywords
  (list
   '("\\<\\grammar\\|rule\\|def\\|end\\>" . font-lock-builtin-face))
   "Minimal highlighting expressions for treetop mode")

(defvar treetop-mode-syntax-table
  (let ((treetop-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" treetop-mode-syntax-table)
    (modify-syntax-entry ?/ ". 124b" treetop-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" treetop-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" treetop-mode-syntax-table)
    (modify-syntax-entry ?\' "\"" treetop-mode-syntax-table)
    treetop-mode-syntax-table)
  "Syntax table for treetop-mode")

(defconst treetop-block-re
  (concat "^\\s *" (regexp-opt '("grammar" "rule" "def"))))

(defun treetop-calculate-indent ()
  (save-excursion
    (beginning-of-line)
    (let ((end (looking-at "^\\s *end")))
      (forward-line -1)
      (while (and (not (bobp)) (looking-at "^\\s *$")) (forward-line -1))
      (cond (end
             (while (not (looking-at treetop-block-re)) (forward-line -1))
             (current-indentation))
            ((looking-at treetop-block-re)
             (+ (current-indentation) 2))
            (t
             (current-indentation))))))

(defun treetop-indent-line ()
  (interactive)
  (let ((indent (treetop-calculate-indent))
        (set-point (>= (save-excursion (back-to-indentation) (point))
                      (point))))
    (save-excursion (indent-line-to indent))
    (if set-point (back-to-indentation))))

;;;###autoload
(defun treetop-mode ()
  "Major mode for editing treetop files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table treetop-mode-syntax-table)
  (use-local-map treetop-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(treetop-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'treetop-indent-line)
  (setq major-mode 'treetop-mode)
  (setq mode-name "treetop")
  (run-hooks 'treetop-mode-hook))

(provide 'treetop-mode)
