(setq kill-whole-line t)

(defun biv-kill-line-or-region ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end) t)
    (kill-line)))
