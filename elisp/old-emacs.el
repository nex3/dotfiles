;; Code for basic compatibility with older Emacs versions

(unless (fboundp 'with-selected-frame)
  (defmacro with-selected-frame (frame &rest body)
    "Execute the forms in BODY with FRAME as the selected frame.
The value returned is the value of the last form in BODY.
See also `with-temp-buffer'."
    (declare (indent 1) (debug t))
    (let ((old-frame (make-symbol "old-frame"))
          (old-buffer (make-symbol "old-buffer")))
      `(let ((,old-frame (selected-frame))
             (,old-buffer (current-buffer)))
         (unwind-protect
             (progn (select-frame ,frame)
                    ,@body)
           (if (frame-live-p ,old-frame)
               (select-frame ,old-frame))
           (if (buffer-live-p ,old-buffer)
               (set-buffer ,old-buffer)))))))

(provide 'old-emacs)
