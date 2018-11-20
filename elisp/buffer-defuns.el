(provide 'buffer-defuns)

(setq ido-use-virtual-buffers t)
(setq recentf-max-saved-items 100)

(defun ido-recentf-open ()
  "Find a recent file using ido."
  (interactive)
  (find-file (ido-completing-read "Find recent file: " recentf-list)))
