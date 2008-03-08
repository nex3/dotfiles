;; Major mode for writing blog posts for the Nex3 blog engine.
;; 
;; Copyright 2007-2008 Nathan Weizenbaum (nex342@gmail.com).
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(require 'http-post)
(require 'textile-mode)

(defvar blog-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map textile-mode-map)
    (define-key map "\C-c\C-v" 'blog-preview)
    (define-key map "\C-c\C-p" 'blog-post-entry)
    map)
  "Keymap for `blog-mode'.
Inherits from `textile-mode-map'.")

(defvar blog-url "http://nex-3.com"
  "The URL of my blog.")

(defvar blog-post-file "~/etc/blog"
  "The file in which new blog posts are saved.")

(defvar blog-browser "x-www-browser"
  "The browser in which to open blog post previews.")

(defmacro http-try-post (&rest args)
  `(let ((proc (http-post ,@args)))
     (while (eq (process-status proc) 'open) (sit-for 0.1))
     (let ((result (buffer-string))
           (status http-status-code))
       (kill-buffer (process-buffer proc))
       (if (>= status 400) nil result))))

(defun blog-try-post (title tags)
  (let ((result
         (http-try-post (concat blog-url "/posts")
                        (list (cons "post[title]" title)
                              (cons "post[tag_string]" tags)
                              (cons "post[content]" (buffer-string))
                              (cons "admin[pass]" (read-passwd "Password: "))
                              '("admin[name]" . "Nathan"))
                        'utf-8)))
    (and result
         (progn
           (string-match "<a href=\"\\([^\"]+\\)\">" result)
           (match-string 1 result)))))

(defun blog-post-entry (&optional title tags)
  "Post an entry to my blog"
  (interactive)
  (setq title (or title (read-from-minibuffer "Post title: ")))
  (setq tags  (or tags  (read-from-minibuffer "Tags: ")))
  (let ((link (blog-try-post title tags)))
    (if (not link)
        (progn
          (message "Invalid password.")
          (sit-for 1)
          (blog-post-entry title tags))
      (progn
        (shell-command (concat blog-browser " " link))
        (message "Successfully posted.")))))

(defun blog-preview ()
  "Preview an entry for my blog"
  (interactive)
  (let ((result
         (http-try-post (concat blog-url "/posts/new.html")
                        (list  (cons "post[content]" (buffer-string))
                               (cons "admin[pass]" (read-passwd "Password: "))
                               '("admin[name]" . "Nathan"))
                        'utf-8)))
    (if (not result)
        (progn
          (message "Invalid password.")
          (sit-for 1)
          (blog-preview))
      (progn
        (while (string-match "\\(href\\|src\\)=\\(\"\\|'\\)/" result)
          (setq result (replace-match (concat "\\1=\\2" blog-url "/") t nil result)))
        (let ((tmp (concat (make-temp-file "blog") ".html")))
          (with-temp-file tmp
            (insert result))
          (shell-command (concat blog-browser " " tmp)))))))

(defun blog ()
  "Open up my blog file"
  (interactive)
  (persp-switch "blog")
  (let ((dir (file-name-directory blog-post-file)))
    (unless (file-exists-p dir) (make-directory dir)))
  (find-file blog-post-file)
  (blog-mode))

(define-derived-mode blog-mode textile-mode "Blog"
  "Major mode for editing blog posts for the Nex3 blog engine.

\\{blog-mode-map}")
