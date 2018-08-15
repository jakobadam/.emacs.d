(provide 'my-bindings)

;; Cursor control with ijkl
(global-set-key "\M-k"     'next-line)
(global-set-key "\M-i"     'previous-line)
(global-set-key "\M-j"     'backward-char)
(global-set-key "\M-l"     'forward-char)
(global-set-key "\M-o"     'forward-word)
(global-set-key "\M-u"     'backward-word)
(global-set-key "\M-C-k"   'forward-list)
(global-set-key "\M-C-i"   'backward-list)

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
