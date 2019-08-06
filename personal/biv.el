(defvar biv-diff-current-file nil)

(defun biv-toggle-pounds ()
  (interactive)
  (save-excursion
    (if (not mark-active)
        (error "Highlight region to toggle comments")
      (let ((beg (point)) (end (mark)) (end-marker (make-marker)))
        (if (> beg end) (progn (setq beg (mark)) (setq end (point))))
        (set-marker end-marker end)
        (goto-char beg)
        (back-to-indentation)
        (if (not (empty-line-suffix))
            (if (looking-at (substring comment-start 0 1))
                (uncomment-region beg end)
              (comment-region beg end))))
      )))

(defun empty-line-suffix () (only-whitespace (current-line-suffix)))
(defun only-whitespace (str) (and (string-match "^[ \r\t]*\$" str) 't))
(defun current-line-suffix () (buffer-substring (point) (line-end-position)))
(defun current-line-prefix () (buffer-substring (line-beginning-position) (point)))
(defun empty-line-prefix () (only-whitespace (current-line-prefix)))

(setq biv-bounce-list '())
(defun biv-bounce-up-indentation ()
  (interactive)
  (push (point) biv-bounce-list)
  (move-to-mode-line-start)
  (let ((goal-column (current-column)))
    (while (empty-line-prefix)
      (previous-line)
      )
    (move-to-mode-line-start)
    )
  )

(defun biv-bounce-back ()
  (interactive)
  (goto-char (pop biv-bounce-list))
  )

(defun biv-git-full-diff ()
  (interactive)
  (save-buffer)
  (let ((default-directory (root "/")))
    (vc-root-diff 'nil))
  (delete-other-windows)
  )

(defun biv-open-wishlist ()
  (interactive)
  (find-file (home "/.elisp/wishlist.el")))

(defun biv-f4 ()
  (interactive)
  (find-file
   (replace-regexp-in-string "/opt/ss/deploy/current/" (root "/")
                             (biv-file-at-point))
   )
  (recenter)
  )

(defun biv-file-at-point ()
  (interactive)
  (save-excursion
    (search-backward-regexp "[^[:word:]\/\-\_\.]")
    (forward-char 1)
    (let ((beg (point)))
      (search-forward-regexp "[^[:word:]\/\-\_\.]")
      (forward-char -1)
      (let ((file (buffer-substring beg (point))))
        (if (string-match "line \\([[:digit:]]+\\)"
                          (current-line-suffix))
            (concat file ":" (match-string 1 (current-line-suffix)))
          file)
        ))
    ))

(defun biv-open-managepy ()
  (interactive)
  (find-file (gethash "managepy" bivHash)))

(defun root (&rest path)
  (concat (getenv "root") (apply 'concat path)))

(defun home (&rest path)
  (concat (getenv "HOME") (apply 'concat path)))

(defun biv-diff-left ()
  (interactive)
  (save-buffer)
  (biv-file-diff))

(defun biv-file-diff ()
  (interactive)
  (let ((default-directory (root "/"))
        (linenum (current-line-number))
	(column (current-column))
	(diffrv (vc-diff 'nil 't)))
    (if (not (or (not diffrv) (stringp diffrv)))
        (progn
          (setq buffer-read-only 'nil)
          (delete-other-windows)
          (biv-goto-linenum-in-diff linenum)
	  (move-to-column (1+ column))
	  ))))

(defun current-line-number ()
  (let ((linenum (string-to-number (substring (what-line) 5))))
    (message "")
    linenum))

(defun biv-rdiff-forward ()
  (interactive)
  (setq file (biv-file-being-diffed))
  (if (string-match "timemachine" (buffer-name))
      (git-time-machine-diff-forwards)
    (biv-diff-jump-to-line file)
    ))

(defun biv-file-being-diffed ()
  (or
   biv-diff-current-file
   (save-excursion
     (biv-parse-file-from-triple-signs))))

(defun biv-diff-jump-to-line (&optional file)
  (interactive)
  (let ((default-directory (root "/")))
    (if (not file) (setq file (biv-file-being-diffed)))
    (let ((window-line (window-line))
          (source-char (biv-diff-source-char-at-point))
          (linenum (biv-diff-source-linenum))
          (column (1- (current-column)))
          (diff-buffer (current-buffer)))
      (if (< column 0) (setq column 0))
      (if (= linenum -1) (progn (setq column 0) (setq linenum 1)))
      (find-file file)
      (biv-set-diff-buffer diff-buffer)
      (if (and source-char 'nil) ;;; linenum/column is more acurate, i think...
          (if (>= source-char (point-max))
              (progn (goto-line linenum) (move-to-column column))
            (goto-char source-char))
        (progn (goto-line linenum) (move-to-column column)))
      (recenter window-line)
      )))

(defun biv-parse-file-from-triple-signs ()
  (let (file)
    (beginning-of-line)
    (if (looking-at "\\+\\+\\+\\|\\-\\-\\-\\|Index:\\|===")
        (search-forward "@@"))
    (search-backward-regexp "^\\+\\+\\+" 'nil t)
    (setq file (biv-parse-file-from-diff-file-line))
    (when (string= file "/dev/null")
      (search-backward-regexp "^\\-\\-\\-" 'nil t)
      (setq file (biv-parse-file-from-diff-file-line))
      )
    file))

(defun biv-parse-file-from-diff-file-line ()
  (beginning-of-line)
  (search-forward " ")
  (if (looking-at "[ab]/") (forward-char 2))
  (let ((beg (point)))
    (search-forward-regexp "[ \t\n]")
    (forward-char -1)
    (biv-full-cvsfile (buffer-substring beg (point)))))

(defun biv-full-cvsfile (file)
  (if (not (string-match "^/" file))
      (format "%s/%s" (root) (or (string-replace-match "^trunk/" file "") file))))

(defun window-line () (biv-count-lines (window-start) (point)))

(defun biv-count-lines (beg end)
  (let (tmp)
    (if (< end beg) (progn (setq tmp beg) (setq beg end) (setq end tmp)))
    (save-excursion 
      (goto-char beg) (setq beg (line-beginning-position))
      (goto-char end) (setq end (line-beginning-position))
      )
    (count-lines beg end)))

(defun biv-diff-source-char-at-point ()
  (interactive)
  (let ((rev (not (save-excursion (beginning-of-line) (looking-at "[-<]")))))
    (condition-case nil
        (progn
          (destructuring-bind (buf line-offset pos src dst &optional switched)
              (diff-find-source-location 'nil rev)
            (+ pos (cdr src))))
      (error 'nil))
    ))

(defun biv-diff-source-linenum ()
  (interactive)
  (let (minus orig beg hasprevhunk (start (point)) (linenum -1))
    (save-excursion
      (search-backward "@@" 'nil 't)
      (setq hasprevhunk (looking-at "@@"))
      (goto-char start)
      (beginning-of-line)
      (if hasprevhunk
          (progn
            (setq minus (looking-at "-"))
            (setq orig (point))
            (biv-diff-hunk-prev-safe)
            (search-forward (if minus "-" "+"))
            (setq beg (point))
            (while (looking-at "[0-9]") (forward-char 1))
            (setq linenum (1- (string-to-number (buffer-substring beg (point)))))
            (while (< (point) orig)
              (progn
                (forward-line 1)
                (if (looking-at "-")
                    (if minus (setq linenum (1+ linenum)))
                  (if (looking-at "+")
                      (if (not minus) (setq linenum (1+ linenum)))
                    (setq linenum (1+ linenum))))))
            ))
      linenum)))

(defun biv-diff-hunk-prev-safe ()
  (let (hasprevhunk)
    (save-excursion
      (beginning-of-line)
      (setq hasprevhunk (search-backward  "@@" 'nil 't)))
    (if hasprevhunk (diff-hunk-prev))))

(defun biv-set-diff-buffer (buffer)
  (if buffer
      (progn
        (make-local-variable 'biv-diff-buffer)
        (setq biv-diff-buffer buffer))))

(defun biv-goto-linenum-in-diff (linenum)
  (biv-goto-diff-index-line)
  (let ((curline 0) lasthunk)
    (condition-case nil
        (while (<= curline linenum)
          (progn
            (diff-hunk-next)
            (forward-line 1)
            (setq curline (biv-diff-source-linenum))))
      (error (setq lasthunk t)))
    (if (not lasthunk)
        (progn (forward-line -2)
               (biv-diff-hunk-prev-safe)
               (forward-line 1)))
    (setq curline (biv-diff-source-linenum))
    (forward-line 1)
    (while (and (< curline linenum) (not (eobp)))
      (progn
        (while (looking-at "-") (forward-line 1))
        (setq curline (biv-diff-source-linenum))
        (forward-line 1)
        ))
    (forward-line -1)
    )
  (if (eobp) (goto-char (point-min)))
  )

(defun biv-insert-db ()
  (interactive)
  (insert "import pudb; pu.db"))

(defun biv-goto-diff-index-line ()
  (beginning-of-line)
  (if (not (looking-at "Index: ")) (search-backward-regexp "^Index: " nil t))
  (looking-at "Index: "))

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun biv-kill-line-or-region ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end) t)
    (kill-line)))

(defun next-error-recenter ()
  (interactive)
  (let ((code-window (selected-window)))
    (next-error)
    (pop-to-buffer (compilation-find-buffer))
    (recenter)
    (select-window code-window)
    (recenter)
    )
  )

(defun previous-error-recenter ()
  (interactive)
  (let ((code-window (selected-window)))
    (previous-error)
    (pop-to-buffer (compilation-find-buffer))
    (recenter)
    (select-window code-window)
    (recenter)
    )
  )

(defun un-camelcase-word-at-point ()
  "un-camelcase the word at point, replacing uppercase chars with
the lowercase version preceded by an underscore.

The first char, if capitalized (eg, PascalCase) is just
downcased, no preceding underscore.
"
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (replace-regexp "\\([A-Z]\\)" "_\\1" nil
                      (1+ (car bounds)) (cdr bounds))
      (downcase-region (car bounds) (cdr bounds)))))

(defun biv-force-server-start ()
    (interactive)
    (progn
      (server-force-delete)
      (server-start)))

(defun biv-apply-hunk ()
  (interactive)
  (diff-apply-hunk 'nil)
  )

(defun biv-revert-hunk ()
  (interactive)
  (diff-apply-hunk t)
  )

(defun biv-open-ssman-file (filename)
  (interactive "sFilename: ")
  (find-file (concat "/ssh:ssmanel7:" filename))
  )

(defun switch-to-scratch-and-back ()
  "Toggle between *scratch* buffer and the current buffer.
     If the *scratch* buffer does not exist, create it."
  (interactive)
  (let ((scratch-buffer-name (get-buffer-create "*scratch*")))
    (if (equal (current-buffer) scratch-buffer-name)
        (switch-to-buffer (other-buffer))
      (switch-to-buffer scratch-buffer-name (lisp-interaction-mode)))))

(defun biv-findcode-run (findcode-command)
  "Run a findcode command non-interactive"
  (let ((compilation-buffer-name-function
         (lambda (mode-name)
           (format "*%s*" findcode-command)))
        (default-directory (root "/")))
    (grep findcode-command)))

(defun biv-findcode (findcode-command)
  "Run a findcode in separate buffer"
  (interactive
   (list (read-string
          (concat "Run findcode in " (abbreviate-file-name (root "/")) " as: ")
          (format "findcode %s" (current-keyword-or-quoted-active-region 'strip-c-apostrophe)))))
  (biv-findcode-run findcode-command))

(defun current-keyword-or-quoted-active-region (&optional f)
  (if mark-active (concat "'" (active-region) "'")
    (let ((string (or (current-word) "")))
      (if f (funcall f string) string))))

(defun strip-c-apostrophe (s) (replace-regexp-in-string "^C'" "" s))

(defadvice find-file-noselect (around find-file-noselect-at-line
                                      (filename &optional nowarn rawfile wildcards)
                                      activate)
  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (filename (if matched (match-string 1 filename) filename))
           (buffer-name ad-do-it))
      (when line-number
        (with-current-buffer buffer-name
          (goto-char (point-min))
          (forward-line (1- line-number))
          ))
      ))
  ;; (recenter)
  )
