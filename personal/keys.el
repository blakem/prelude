(define-prefix-command 'sa-map)
(global-set-key (kbd "s-a") 'sa-map)
(global-set-key (kbd "s-x") 'sa-map)

(global-set-key [(super a) ?g ?a] 'vc-annotate) ; sa-svn-annotate)
(global-set-key [(super a) ?g ?f] 'biv-git-diff-full)

(global-set-key (kbd "s-f") 'projectile-find-file)
(global-set-key (kbd "C-k") 'biv-kill-line-or-region)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-j") 'ibuffer)
(global-set-key (kbd "<C-s-268632083>") 'projectile-grep)
(global-set-key (quote [f10]) 'biv-toggle-pounds)
(global-set-key [s-left] 'biv-diff-left)
(define-key diff-mode-map [s-right] 'biv-rdiff-forward)

(global-set-key (kbd "s-w") 'copy-region-as-kill)

