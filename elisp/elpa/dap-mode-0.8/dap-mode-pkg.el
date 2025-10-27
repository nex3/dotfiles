;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "dap-mode" "0.8"
  "Debug Adapter Protocol mode."
  '((emacs        "27.1")
    (dash         "2.18.0")
    (lsp-mode     "6.0")
    (bui          "1.1.0")
    (f            "0.20.0")
    (s            "1.12.0")
    (lsp-treemacs "0.1")
    (posframe     "0.7.0")
    (ht           "2.3")
    (lsp-docker   "1.0.0"))
  :url "https://github.com/emacs-lsp/dap-mode"
  :commit "71c2e5a3734debe265c0de11531bf1d1351f7483"
  :revdesc "71c2e5a3734d"
  :keywords '("languages" "debug")
  :authors '(("Ivan Yonchovski" . "yyoncho@gmail.com"))
  :maintainers '(("Ivan Yonchovski" . "yyoncho@gmail.com")))
