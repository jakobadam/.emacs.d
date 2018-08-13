(provide 'setup-magit)

(use-package magit
  :ensure t
  :commands magit-status magit-blame
  :init
  (global-magit-file-mode)

  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  :config
  (setq magit-branch-arguments nil
        ;; use ido to look for branches
        magit-completing-read-function 'magit-ido-completing-read
        ;; don't put "origin-" in front of new branch names by default
        magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
        magit-push-always-verify nil
        ;; Get rid of the previous advice to go into fullscreen
        magit-restore-window-configuration t)
  :bind ("C-x g" . magit-status))

;; full screen magit-status
(defun magit-status-fullscreen (prefix)
  (interactive "P")
  (magit-status)
  (unless prefix
    (delete-other-windows)))

;; don't prompt me

;; (set-default 'magit-push-always-verify nil)
;; (set-default 'magit-revert-buffers 'silent)
;; (set-default 'magit-no-confirm '(stage-all-changes
;;                                  unstage-all-changes))

;; move cursor into position when entering commit message

(defun my/magit-cursor-fix ()
  (beginning-of-buffer)
  (when (looking-at "#")
    (forward-line 2)))

(add-hook 'git-commit-mode-hook 'my/magit-cursor-fix)

;; full screen vc-annotate

(defun vc-annotate-quit ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

(eval-after-load "vc-annotate"
  '(progn
     (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))

     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))

(set-default 'magit-diff-refine-hunk t)

;; speed up refs buffer - don't show tags
;; https://magit.vc/manual/magit/Performance.html
(remove-hook 'magit-refs-sections-hook 'magit-insert-tags)

;; Meanwhile you can tell Magit to only automatically refresh the
;; current Magit buffer, but not the status buffer. If you do that,
;; then the status buffer is only refreshed automatically if it is the
;; current buffer.
(setq magit-refresh-status-buffer nil)
