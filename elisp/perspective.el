;; perspective.el --- switch between named "perspectives" of the editor
;; Copyright (C) 2008 Nathan Weizenbaum <nex342@gmail.com>
;; Licensed under the same terms as Emacs.

(eval-when-compile (require 'cl))

(defvar perspectives-hash (make-hash-table :test 'equal :size 10)
  "A hash containing all perspectives. The keys are the
perspetives' names. The values are of the
form (WINDOW-CONFIGURATION BUFFERS).

WINDOW-CONFIGURATION is the configuration given by
`current-window-configuration' last time the perspective was
saved (if this isn't the current perspective, this is when the
perspective was last active).

BUFFERS is a list of buffer objects that are associated with this
perspective.")

(defvar persp-pending-buffers nil
  "When creating a new perspective, we want to set the buffers
using `persp-set-buffers' before we save the perspective using
`persp-save'. This variable stores the buffers in the interim.")

(defvar persp-last-name nil)

(defvar persp-modestring nil
  "The string displayed in the modeline representing the perspectives.")
(put 'persp-modestring 'risky-local-variable t)

(defface persp-selected-face
  '((default (:weight bold :foreground "Blue")))
  "The face used to highlight the current perspective on the modeline.")

(defun persp-save ()
  "Save the current perspective in `perspectives-hash'."
  (when (persp-name)
    (puthash (persp-name)
             (list (current-window-configuration) (persp-remove-dups
                                                   (or persp-pending-buffers (persp-buffers))))
             perspectives-hash)
    (setq persp-pending-buffers nil)))

(defun persp-name ()
  "Return the name of the current perspective."
  (frame-parameter (selected-frame) 'persp-name))

(defun persp-set-name (name)
  "Set the name of the current perspective to NAME."
  (modify-frame-parameters (selected-frame) `((persp-name . ,name))))

(defun persp-names ()
  "Return a list of the names of all perspectives, sorted alphabetically."
  (sort
   (loop for name being the hash-keys of perspectives-hash
         collect name)
   'string<))

(defun persp-buffers ()
  "Return a list of buffers for the current perspective."
  (let ((persp (gethash (persp-name) perspectives-hash)))
    (if persp (cadr persp))))

(defun persp-set-buffers (buffers)
  "Sets the current perspective's buffer list to BUFFERS."
  (let ((persp (gethash (persp-name) perspectives-hash)))
    ;; If the perspective is null, we haven't saved yet
    (if (null persp) (setq persp-pending-buffers buffers)
      (setcar (cdr persp) buffers))))

(defun persp-prompt (&optional default require-match)
  "Prompt for the name of a perspective.

DEFAULT is a default value for the prompt.

REQUIRE-MATCH can take the same values as in `completing-read'."
  (completing-read (concat "Perspective name"
                           (if default (concat " (default " default ")") "")
                           ": ")
                   (persp-names)
                   nil require-match nil nil default))

(defmacro with-perspective (name &rest body)
  "Evaluate BODY with the perspective given by NAME as the current perspective."
  (let ((old-persp-var (gensym)))
    `(let ((,old-persp-var (persp-name)))
       (persp-set-name ,name)
       ,@body
       (persp-set-name ,old-persp-var))))

(defun persp-new (name)
  "Save the current perspective, create a new perspective with
name NAME, and switch to the new perspective.

The new perspective initially has only one buffer: a
Lisp-interaction buffer called \"*scratch* (NAME)\"."
  (interactive "sNew perspective: \n")
  (persp-save)
  (let ((buffer (switch-to-buffer (concat "*scratch* (" name ")"))))
    (lisp-interaction-mode)
    (delete-other-windows)
    (persp-set-name name)
    (persp-set-buffers (list buffer))
    (persp-save)))

(defun persp-remove-dups (list &optional test)
  "Remove duplicate items from LIST.

TEST is a hash table test used to determine if two elements are
equal. It defaults to `equal', but can also be set to `eq',
`eql', or a test defined by `define-hash-table-test'.

For example, (persp-remove-dups '(1 2 1 3 2 4 3 5)) gives '(1 2 3 4 5)."
  (let ((seen (make-hash-table :test (or test 'equal))))
    (loop for item in list
          if (not (gethash item seen))
            collect item into result
            and do (puthash item t seen)
          finally return result)))

(defun persp-reactivate-buffers (buffers)
  "\"Reactivate\" BUFFERS by raising them to the top of the
most-recently-selected list. The result is BUFFERS with all
non-living buffers removed.

See also `other-buffer'."
  (loop for buf in (reverse buffers)
        if (not (null (buffer-name buf)))
          collect buf into living-buffers
          and do (switch-to-buffer buf)
        finally return (reverse living-buffers)))

(defun persp-intersperse (list val)
  "Insert VAL between every pair of items in LIST and return the resulting list.

For example, (persp-intersperse '(1 2 3) 'a) gives '(1 a 2 a 3)."
  (if (or (null list) (null (cdr list))) list
    (cons (car list)
          (cons val
                (persp-intersperse (cdr list) val)))))

(defun persp-update-modestring ()
  "Update `persp-modestring' to reflect the current perspectives."
  (setq persp-modestring
        (append '("[")
                (persp-intersperse (mapcar 'persp-format-name (persp-names)) "|")
                '("]"))))

(defun persp-format-name (name)
  "Format the perspective name given by NAME for display in `persp-modestring'."
  (if (not (equal name (persp-name))) name
    (let ((name (concat name)))
      (add-text-properties 0 (length name) '(face persp-selected-face) name)
      name)))

(defun persp-get-quick (char)
  "Returns the name of the first perspective, alphabetically, that begins with CHAR."
  (loop for persp in (persp-names)
        if (eq (string-to-char persp) char) return persp))

(defun persp-switch (name)
  "Switch to the perspective given by NAME. If it doesn't exist,
create a new perspective and switch to that.

Switching to a perspective means that all buffers associated with
that perspective are reactivated (see `persp-reactivate-buffers')
and the perspective's window configuration is restored."
  (interactive "i")
  (if (null name) (setq name (persp-prompt persp-last-name)))
  (if (equal name (persp-name)) name
    (let ((persp (gethash name perspectives-hash)))
      (setq persp-last-name (persp-name))
      (if (null persp) (persp-new name)
        (persp-save)
        (persp-set-name name)
        (persp-set-buffers (persp-reactivate-buffers (cadr persp)))
        (set-window-configuration (car persp)))
      (persp-update-modestring)
      name)))

(defun persp-switch-quick (char)
  "Switches to the first perspective, alphabetically, that begins with CHAR.

See `persp-switch', `persp-get-quick'."
  (interactive "c")
  (let ((persp (persp-get-quick char)))
    (if persp (persp-switch persp)
      (error (concat "No perspective name begins with " (string char))))))

(defun persp-find-some ()
  "Returns the name of a valid perspective.

This function tries to return the \"most appropriate\"
perspective to switch to. It tries:

  * The perspective given by `persp-last-name'.
  * The main perspective.
  * The first existing perspective, alphabetically.

If none of these perspectives can be found, this function will
create a new main perspective and return \"main\"."
  (cond
   (persp-last-name persp-last-name)
   ((gethash "main" perspectives-hash) "main")
   ((> (hash-table-count perspectives-hash) 0) (car (persp-names)))
   (t (progn
        (persp-set-name "main")
        (persp-set-buffers (buffer-list))
        (persp-save)
        (persp-update-modestring)
        "main"))))

(defun persp-add-buffer (buffer)
  "Associate BUFFER with the current perspective.

See also `persp-switch' and `persp-remove-buffer'."
  (interactive "bAdd buffer to perspective: \n")
  (persp-set-buffers (cons (get-buffer buffer) (persp-buffers))))

(defun persp-remove-buffer (buffer)
  "Disassociate BUFFER with the current perspective.

See also `persp-switch' and `persp-add-buffer'."
  (interactive "bRemove buffer from perspective: \n")
  (setq buffer (get-buffer buffer))
  ; Only kill the buffer if no other perspectives are using it
  (cond ((loop for persp being the hash-values of perspectives-hash using (hash-keys name)
               unless (equal name (persp-name))
               if (memq buffer (cadr persp)) return nil
               finally return t)
         (kill-buffer buffer))
        ;; Make the buffer go away if we can see it.
        ;; TODO: Is it possible to tell if it's visible at all,
        ;;       rather than just the current buffer?
        ((eq buffer (current-buffer)) (bury-buffer))
        (t (bury-buffer buffer)))
  (persp-set-buffers (remq buffer (persp-buffers))))

(defun persp-kill (name)
  "Kill the perspective given by NAME.

Killing a perspective means that all buffers associated with that
perspective and no others are killed."
  (interactive "i")
  (if (null name) (setq name (persp-prompt (persp-name) t)))
  (with-perspective name
    (mapcar 'persp-remove-buffer (persp-buffers)))
  (persp-set-name nil)
  (setq persp-last-name nil)
  (remhash name perspectives-hash)
  (persp-switch (persp-find-some)))

(defun persp-rename (name)
  "Rename the current perspective to NAME."
  (interactive "sNew name: ")
  (if (gethash name perspectives-hash)
      (error (concat "Perspective " name " already exists"))
    (remhash (persp-name) perspectives-hash)
    (persp-set-name name)
    (persp-save)
    (persp-update-modestring)))

(defadvice switch-to-buffer (after persp-add-buffer-adv)
  "Add BUFFER to the current perspective.

See also `persp-add-buffer'."
  (persp-add-buffer buffer))

(defun persp-init ()
  "Initialize the perspectives system."
  (persp-set-name "main")
  (persp-set-buffers (buffer-list))
  (persp-save)
  (ad-activate 'switch-to-buffer)

  (setq global-mode-string (or global-mode-string '("")))
  (if (not (memq 'persp-modestring global-mode-string))
      (setq global-mode-string (append global-mode-string '(persp-modestring))))
  (persp-update-modestring))

(defun quick-perspective-keys ()
  "Binds all C-S-letter key combinations to switch to the first
perspective beginning with the given letter."
  (loop for c from ?a to ?z
        do (global-set-key
            (read-kbd-macro (concat "C-S-" (string c)))
            `(lambda ()
               (interactive)
               (persp-switch-quick ,c)))))


(define-prefix-command 'perspective 'perspective-map)
(global-set-key (read-kbd-macro "C-x x") perspective-map)

(global-set-key (read-kbd-macro "C-x x n") 'persp-new)
(global-set-key (read-kbd-macro "C-x x s") 'persp-switch)
(global-set-key (read-kbd-macro "C-x x k") 'persp-remove-buffer)
(global-set-key (read-kbd-macro "C-x x c") 'persp-kill)
(global-set-key (read-kbd-macro "C-x x r") 'persp-rename)

(if (null (persp-name))
    (persp-init))

(provide 'perspective)
