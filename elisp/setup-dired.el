(provide 'setup-dired)

(require 'dired)
(require 'dash)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Wdired.html

(use-package dired-details :ensure t)

;; Make dired less verbose
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

;; Move files between split panes
(setq dired-dwim-target t)

;; Reload dired after making changes
(--each '(dired-do-rename
          dired-do-copy
          dired-create-directory
          wdired-abort-changes)
        (eval `(defadvice ,it (after revert-buffer activate)
                 (revert-buffer))))

;; C-a is nicer in dired if it moves back to start of files
(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

(define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
(define-key dired-mode-map (kbd "k") 'dired-do-delete)

;; Delete with C-x C-k to match file buffers and magit
(define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)

(eval-after-load "wdired"
  '(progn
     (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
     (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
     (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)))

;; https://github.com/thomp/dired-launch
;; J launches external program
;; TODO: Switch between windows and linux.
(use-package dired-launch
  :ensure t
  ;; TODO: switch for linux / windows
  :init (setq dired-launch-default-launcher '("start"))
  :config (dired-launch-enable)
  )

