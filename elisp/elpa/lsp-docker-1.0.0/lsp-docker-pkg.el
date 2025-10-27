;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "lsp-docker" "1.0.0"
  "LSP Docker integration."
  '((emacs    "27.1")
    (dash     "2.14.1")
    (lsp-mode "6.2.1")
    (f        "0.20.0")
    (s        "1.13.0")
    (yaml     "0.2.0")
    (ht       "2.0"))
  :url "https://github.com/emacs-lsp/lsp-docker"
  :commit "ce291d0f80533f8eaca120eb745d55669e062636"
  :revdesc "ce291d0f8053"
  :keywords '("languages" "langserver")
  :authors '(("Ivan Yonchovski" . "yyoncho@gmail.com"))
  :maintainers '(("Ivan Yonchovski" . "yyoncho@gmail.com")))
