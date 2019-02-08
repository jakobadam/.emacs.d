(provide 'my-bindings)

;; Many from https://github.com/magnars/.emacs.d/blob/master/settings/key-bindings.elm

;; Completion that uses many different methods to find options.
;;(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
;(global-set-key (kbd "C-:") 'hippie-expand-lines)
;;(global-set-key (kbd "C-,") 'completion-at-point)
;; Expand region (increases selected region by semantic units)
(global-set-key (kbd "C-'") 'er/expand-region)

;; Use shell-like backspace C-h, rebind help to F1
;; (define-key key-translaion-map [?\C-h] [?\C-?])
;;(global-set-key (kbd "<f1>") 'help-command)

;; (global-set-key (kbd "M-h") 'kill-region-or-backward-word)


;; Killing text
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
;;(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; Help should search more than just commands
(global-set-key (kbd "<f1> a") 'apropos)


;; Completion at point
(global-set-key (kbd "C-<tab>") 'completion-at-point)

;; Goto line
(global-set-key (kbd "C-l") 'goto-line)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; 
(global-set-key (kbd "C-x C-o") 'find-file-other-window)
(global-set-key (kbd "C-x C-r") 'find-alternate-file-with-sudo)

;; use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
