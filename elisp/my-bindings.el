(provide 'my-bindings)

;; Many from https://github.com/magnars/.emacs.d/blob/master/settings/key-bindings.el

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)

;; Smart M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Use C-x C-m to do M-x per Steve Yegge's advice
(global-set-key (kbd "C-x C-m") 'smex)

;; Expand region (increases selected region by semantic units)
(global-set-key (kbd "C-'") 'er/expand-region)

;; Use shell-like backspace C-h, rebind help to F1
;; (define-key key-translaion-map [?\C-h] [?\C-?])
;;(global-set-key (kbd "<f1>") 'help-command)

;; (global-set-key (kbd "M-h") 'kill-region-or-backward-word)


;; Killing text
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-c C-w") 'kill-to-beginning-of-line)

;; Cursor control with ijkl
(global-set-key "\M-k"     'next-line)
(global-set-key "\M-i"     'previous-line)
(global-set-key "\M-j"     'backward-char)
(global-set-key "\M-l"     'forward-char)
(global-set-key "\M-o"     'forward-word)
(global-set-key "\M-u"     'backward-word)
(global-set-key "\M-C-k"   'forward-list)
(global-set-key "\M-C-i"   'backward-list)

;; Make shell more convenient, and suspend-frame less
(global-set-key (kbd "C-z") 'shell)
(global-set-key (kbd "C-x M-z") 'suspend-frame)

;; Create new frame
(define-key global-map (kbd "C-x C-n") 'make-frame-command)

;; Jump to a definition in the current file. (This is awesome)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; Help should search more than just commands
(global-set-key (kbd "<f1> a") 'apropos)


;; Completion at point
(global-set-key (kbd "C-<tab>") 'completion-at-point)

;; Magit
(global-set-key (kbd "C-x m") 'magit-status-fullscreen)
(autoload 'magit-status-fullscreen "magit")

;; Goto line
(global-set-key (kbd "C-l") 'goto-line)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-x C-r") 'find-alternate-file-with-sudo)

;; Turn on the menu bar for exploring new modes
(global-set-key [f1] 'menu-bar-mode)

;; My up and down shortcut also in auto-complete dropdown
;; (define-key ac-complete-mode-map (kbd "M-k") 'ac-next)
;; (define-key ac-complete-mode-map (kbd "M-i") 'ac-previous)

;; Override the indent on M-j
(define-key js2-mode-map (kbd "M-j") 'backward-char)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f") 'ido-recentf-open)

