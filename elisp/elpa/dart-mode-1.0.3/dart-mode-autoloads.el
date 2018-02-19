;;; dart-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "dart-mode" "dart-mode.el" (23179 17792 607186
;;;;;;  703000))
;;; Generated autoloads from dart-mode.el
 (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

(autoload 'dart-mode "dart-mode" "\
Major mode for editing Dart files.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `dart-mode-hook'.

Key bindings:
\\{dart-mode-map}

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dart-mode-autoloads.el ends here
