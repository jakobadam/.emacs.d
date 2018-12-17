(provide 'my-html)

(require 'use-package)

(use-package rainbow-mode :ensure t)
(use-package emmet-mode :ensure t)
(use-package flymake-css :ensure t)
;; (use-package ac-html)
;; (use-package ac-haml)
;; (use-package ac-html-default-data-provider)

(defun my-html-mode-hook ()
   (turn-off-auto-fill)
   (emmet-mode)
   (rainbow-mode 1)

;;   (ac-html-enable-data-provider 'ac-html-default-data-provider)
;;   (ac-html-setup)


;;   ;; Not Working...
;;   ;; (ac-html-bootstrap+)

;;   (setq ac-sources '(ac-source-html-tag
;;                      ac-source-html-attr
;;                      ac-source-html-attrv))

;;   (auto-complete-mode)

;;   ;; 2 spaces indent
;;   (setq sgml-basic-offset 2)
)


(defun my-css-mode-hook ()
   (rainbow-mode 1)
   (emmet-mode)
   (flymake-css-load)
)

;; (defun my-scss-mode-hook ()
;;   (rainbow-mode 1)
;;   (emmet-mode)
;;   ;; (auto-complete-mode)
;;   (flymake-mode)
;;   )

;; (autoload 'scss-mode "scss-mode")

(add-hook 'sgml-mode-hook 'my-html-mode-hook)
(add-hook 'haml-mode-hook 'my-html-mode-hook)

(add-hook 'css-mode-hook 'my-css-mode-hook)
(add-hook 'scss-mode 'my-scss-mode-hook)

