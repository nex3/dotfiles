(defun rcirc-complete-nick ()
  "Complete nick from list of nicks in channel."
  (interactive)
  (if (eq last-command this-command)
      (setq rcirc-nick-completions
            (append (cdr rcirc-nick-completions)
                    (list (car rcirc-nick-completions))))
    (setq rcirc-nick-completion-start-offset
          (- (save-excursion
               (if (re-search-backward " " rcirc-prompt-end-marker t)
                   (1+ (point))
                 rcirc-prompt-end-marker))
             rcirc-prompt-end-marker))
    (setq rcirc-nick-completions
          (let ((completion-ignore-case t))
            (all-completions
	     (buffer-substring
	      (+ rcirc-prompt-end-marker
		 rcirc-nick-completion-start-offset)
	      (point))
	     (mapcar (lambda (x) (cons x nil))
		     (rcirc-channel-nicks (rcirc-buffer-process)
					  rcirc-target))))))
  (if rcirc-unambiguous-complete
      (rcirc-unambiguous-complete-nick rcirc-nick-completions)
    (rcirc-cycle-complete-nick rcirc-nick-completions)))

(defun rcirc-cycle-complete-nick (completions)
  "Complete nick by cycling through possibilities."
  (let ((completion (car completions)))
    (when completion
      (rcirc-put-nick-channel (rcirc-buffer-process) completion rcirc-target)
      (rcirc-insert-completed-nick completion))))

(defun rcirc-unambiguous-complete-nick (completions)
  "Complete unambiguous portion of nick and prompt for more."
  (let ((unambiguous (car completions)))
    (mapc (lambda (nick)
	    (setq unambiguous (rcirc-get-unambiguous nick unambiguous)))
	  (cdr completions))
    (if (string= unambiguous (buffer-substring
			      (+ rcirc-prompt-end-marker
				 rcirc-nick-completion-start-offset)
			      (point)))
	(message (mapconcat 'identity completions " "))
      (if unambiguous
	  (rcirc-insert-completed-nick unambiguous (not (equal (length completions) 1)))))))

(defun rcirc-get-unambiguous (nick target)
  (if (> (length nick) (length target))
      (setq nick (substring nick 0 (length target))))
  (if (equalp nick (substring target 0 (length nick)))
      nick
    (rcirc-get-unambiguous (substring nick 0 (- (length nick) 1)) target)))

(defun rcirc-insert-completed-nick (nick &optional incomplete)
  (delete-region (+ rcirc-prompt-end-marker
		    rcirc-nick-completion-start-offset)
		 (point))
  (insert nick)
  (if (and (= (+ rcirc-prompt-end-marker
	    rcirc-nick-completion-start-offset)
	      rcirc-prompt-end-marker)
	   (not incomplete))
      (insert ": ")))

(provide 'rcirc-unambiguous-nick-completion)