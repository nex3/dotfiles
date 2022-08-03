(define-package "magit" "3.3.0" "A Git porcelain inside Emacs."
  '((emacs "25.1")
    (dash "2.19.1")
    (git-commit "3.3.0")
    (magit-section "3.3.0")
    (transient "0.3.6")
    (with-editor "3.0.5"))
  :commit "f44f6c14500476d918e9c01de8449edb20af4113" :authors
  '(("Marius Vollmer" . "marius.vollmer@gmail.com")
    ("Jonas Bernoulli" . "jonas@bernoul.li"))
  :maintainer
  '("Jonas Bernoulli" . "jonas@bernoul.li")
  :keywords
  '("git" "tools" "vc")
  :url "https://github.com/magit/magit")
;; Local Variables:
;; no-byte-compile: t
;; End:
