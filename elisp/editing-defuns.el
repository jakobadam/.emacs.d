;;; editing-defuns.el --- Basic text editing defuns -*- lexical-binding: t; -*-
(provide 'editing-defuns)

(defun kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

(defun clone-line ()
   (interactive)
   (let ((col (current-column)))
     (move-beginning-of-line 1)
     (kill-line)
     (yank)
     (newline)
     (yank)
     (move-to-column col)))

;; overwrites upcase word
(global-set-key (kbd "M-c") 'clone-line)
