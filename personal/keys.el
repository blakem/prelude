(define-prefix-command 'sa-map)
(global-set-key (kbd "s-a") 'sa-map)
(global-set-key (kbd "s-x") 'sa-map)
(global-set-key (kbd "C-q") 'sa-map)
(global-set-key (kbd "s-q") 'sa-map)

(global-set-key [(super a) ?a ?h] 'biv-apply-hunk)
(global-set-key [(super a) ?d ?b] 'biv-insert-db)
(global-set-key [(super a) ?g ?a] 'vc-annotate) ; sa-svn-annotate)
(global-set-key [(super a) ?g ?f] 'biv-git-full-diff)
(global-set-key [(super a) ?m ?a] 'biv-open-managepy)
(global-set-key [(super a) ?o ?w] 'biv-open-wishlist)
(global-set-key [(super a) ?p ?d] 'jedi:show-doc)
(global-set-key [(super a) ?r ?h] 'biv-revert-hunk)
(global-set-key [(super a) ?s ?c] 'un-camelcase-word-at-point)
(global-set-key [(super a) ?s ?b] 'switch-to-scratch-and-back)

(global-set-key (kbd "s-f") 'projectile-find-file)
(global-set-key (kbd "C-k") 'biv-kill-line-or-region)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-g") 'goto-line)
(global-set-key (kbd "s-b") 'ace-window)
(global-set-key (kbd "C-j") 'helm-mini)
(global-set-key (kbd "<C-s-268632083>") 'biv-findcode) ;; C-s-s
(global-set-key (kbd "<C-s-268632070>") 'biv-findcode) ;; C-s-f
(global-set-key (kbd "<C-s-left>") 'previous-buffer)
(global-set-key (kbd "<C-s-right>") 'next-buffer)
(global-set-key (quote [f10]) 'biv-toggle-pounds)
(global-set-key [s-left] 'biv-diff-left)
(define-key diff-mode-map [s-right] 'biv-rdiff-forward)

(global-set-key (kbd "s-w") 'copy-region-as-kill)

(global-set-key (kbd "C-%") 'goto-match-paren)

(global-set-key (kbd "\C-f") 'projectile-find-file)
(global-set-key [f4] 'find-file-at-point)
(global-set-key [(super f)] 'projectile-find-file)

(global-set-key [f6] 'next-error-recenter)
(global-set-key [(shift f6)] 'previous-error-recenter)
(global-set-key [(control f6)] 'previous-error-recenter)

