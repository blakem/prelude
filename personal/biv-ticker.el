(defvar biv-tick-file (concat (getenv "HOME") "/.ticker"))
(defvar biv-tick-bash-history (concat (getenv "HOME") "/.bash_history"))
(defvar biv-tick-bash-history-ssman (concat (getenv "HOME")
                                            "/git/SwiftStack/personal/.bash_history_ssman"))
(defvar biv-tick-period 60)
(defvar biv-tick-log-on-start nil)
(defvar biv-timer nil)
(defvar biv-last-keys nil)

(defun biv-log-line ()
  (format-time-string "%Y-%m-%d %T\n"))

(defun biv-tick ()
  (when (or (biv-tick-recent-keys-pressed)
            (biv-tick-recent-bash-activity))
    (biv-write-ticker-line))
  (setq biv-last-keys (recent-keys)))

(defun biv-tick-recent-keys-pressed ()
    (not (equal (recent-keys) biv-last-keys)))

(defun biv-tick-recent-bash-activity ()
  (or
   (file-newer-than-file-p biv-tick-bash-history biv-tick-file)
   (file-newer-than-file-p biv-tick-bash-history-ssman biv-tick-file)))

(defun biv-teardown-timer ()
  (when biv-timer
    (cancel-timer biv-timer)
    (setq biv-timer nil)))

(defun biv-setup-timer ()
  (biv-teardown-timer)
  (when biv-tick-log-on-start (biv-tick))
  (setq biv-timer (run-at-time (biv-ticker-offset-time) biv-tick-period 'biv-tick)))

(defun biv-write-ticker-line ()
  (write-region (biv-log-line) nil biv-tick-file 'append 42))

(defun biv-ticker-offset-time ()
  (- biv-tick-period
     (% (string-to-number (format-time-string "%s"))
        biv-tick-period)))

(biv-setup-timer)

(provide 'biv-ticker)
