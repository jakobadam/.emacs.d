;; General Settings
(defconst jd/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))
(defun jd/emacs-subdirectory (d) (expand-file-name d jd/emacs-directory))

;; General settings - Directory Structure
(let* ((subdirs '("elisp" "backups" "savefile"))
       (fulldirs (mapcar (lambda (d) (jd/emacs-subdirectory d)) subdirs)))
  (dolist (dir fulldirs)
    (when (not (file-exists-p dir))
      (message "Make directory: %s" dir)
      (make-directory dir))))

(defconst savefile-dir (expand-file-name "savefile" jd/emacs-directory))

;; General settings - Customization Section
(setq custom-file (expand-file-name "custom.el" jd/emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; General settings - Setting up the Load Path

;; Extra packages not available via the package manager go in my
;; personal stash at: $HOME/.emacs.d/elisp
(add-to-list 'load-path (jd/emacs-subdirectory "elisp"))

;; Allow allocation of ~50MB before GC (from ~80KB)
(setq gc-cons-threshold 50000000)

;; load .el if it's newer than .elc
(setq load-prefer-newer t)

;; General settings - Backup
;; Move all backup files to a central location.
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (jd/emacs-subdirectory "backups")))))

;; Tramp should do the same:
(setq tramp-backup-directory-alist backup-directory-alist)

;; Make backups of files, even when they’re in version control:
(setq vc-make-backup-files t)

;; Package Initialization - Package Manager
(require 'package)
(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")))
(package-initialize)

;; only update the package list if not already downloaded
(unless package-archive-contents
  (package-refresh-contents))

;; Package Initialization - Use-Package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
;; No need for `:ensure t` to download package
(setq use-package-always-ensure t)

;; Load up a collection of enhancements to Emacs Lisp, including
;; dash, s for string manipulation, and f for file manipulation.
(require 'cl)

(use-package dash
  :config (eval-after-load "dash" '(dash-enable-font-lock)))

(use-package s)

(use-package f)

;; Variables
;;
(if (equal "guest-jdam" user-login-name)
    (setq user-mail-address "jakob.dam@systematic.com")
  (setq user-mail-address "jakob.a.dam@gmail.com"))

;; Only use spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; Make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)

;; Does anyone type yes anymore?
(fset 'yes-or-no-p 'y-or-n-p)

;; Fix the scrolling to keep point in the center:
(setq scroll-conservatively 10000
      scroll-preserve-screen-position t)

;; Display Settings
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq visible-bell t)

(when (window-system)
  (tool-bar-mode 0)
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1))            ;; Scrollbars are waste screen estate

;; Display Settings - Whitespace Mode

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'whitespace-mode))
  ;; cool, but can't do that for the current project
  ;;(add-hook 'before-save-hook 'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs empty trailing))) ;; lines-tail

;; Display Settings - Fill Mode

;; Automatically wrapping when you get to the end of a line (or the
;; fill-region):
;; (use-package fill
;;   :bind (("C-c T f" . auto-fill-mode)
;;          ("C-c T t" . toggle-truncate-lines))
;;   :init (add-hook 'org-mode-hook 'turn-on-auto-fill)
;;   :diminish auto-fill-mode)

;; Key Bindings - Display Command Sequences
(use-package which-key
  :defer 10
  :diminish which-key-mode
  :config (which-key-mode 1))

;; Key Bindings - Undo and Redo

;; According to this article, I get better functionality than the
;; redo+ plugin (which I can’t seem to get working well).

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode 1)
  :config
  (defalias 'redo 'undo-tree-redo)
  :bind (("C-z" . undo)     ; Zap to character isn't helpful
         ("C-S-z" . redo)))

;; Key Bindings - Jumping to Windows
(use-package ace-window
  :init
    ;; use home rows for window selection
    (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?o))
    (global-set-key (kbd "C-x o") 'ace-window)
    :diminish ace-window-mode)

;; Key Bindings - Better Jumping
(use-package avy
  :init (setq avy-background t)
  :bind ("C-;" . avy-goto-char-timer))

;; Wrap selected text
(use-package wrap-region
  :config
  (wrap-region-global-mode t)
  (wrap-region-add-wrappers
   '(("(" ")")
     ("[" "]")
     ("{" "}")
     ("<" ">")
     ("'" "'")
     ("\"" "\"")
     ("_" "_")
     ("‘" "’"   "q")
     ("“" "”"   "Q")
     ("**" "**" "b"   markdown-mode)            ; bolden
     ("*" "*"   "i"   markdown-mode)            ; italics
     ("`" "`"   "c" '(markdown-mode ruby-mode)) ; code
     ("`" "'"   "c"   lisp-mode)                ; code
     ))
  :diminish wrap-region-mode)

(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  :config
  ;;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-mode +1))

;; Loading and finding files - Dired Options
(use-package dired-launch
  :config (when (eq system-type 'windows-nt)
            (setq dired-launch-default-launcher '("start")))
  :init (dired-launch-enable)
  )

;; The dired-x project seems useful:
;; https://www.masteringemacs.org/article/dired-shell-commands-find-xargs-replacement
;;(use-package dired-x)


;; Save buffers on de-focus
(use-package super-save
  :config
  (super-save-mode +1))

(use-package flx)

(use-package ivy-hydra)

;; TODO: When renaming ivy should be disabled, for now use C-M-j
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  ;; use fuzzy match for everything except swiper
  ;; Change matching: C-o m
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t      . ivy--regex-fuzzy)
          ))
  (setq ivy-initial-inputs-alist nil)
  (setq enable-recursive-minibuffers t)
  ;;(global-set-key (kbd "C-c C-r") 'ivy-resume)
  )

(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper)
  )

(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-m") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c a") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; Auto Complete - M-n M-p or M-1 to select
(use-package company
  :init
  (setq company-dabbrev-ignore-case t
        company-dabbrev-downcase 0
        company-idle-delay 0
        company-show-numbers t
        company-tooltip-align-annotations t)
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode)

  :bind ("C-:" . company-complete)  ; In case I don't want to wait
  :diminish company-mode)

;; Take advantage of idle time by displaying some documentation using
;; company-quickhelp project.
(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))

;; Yasnippets
(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (jd/emacs-subdirectory "snippets")))

;; also load the snippets:)
(use-package yasnippet-snippets)

(use-package vimish-fold
  :init
  (global-set-key (kbd "<C-return>") 'vimish-fold)
  :config
  (vimish-fold-global-mode +1)
  )

;; https://editorconfig.org/
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Grey out everything else than highligt region C-x n n, C-x n w
(use-package fancy-narrow)

;; always show matching paren
(use-package paren
  :config
  (show-paren-mode +1))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode +1))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

;; save minibuffer hist and friends
(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

;; Move code blocks with M-S
(use-package move-text
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift i)] . move-text-up)
   ([(meta shift down)] . move-text-down)
   ([(meta shift k)] . move-text-down)))

;; Different colors on delimiters...
(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

;; Colorize #0000ff
(use-package rainbow-mode
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; nicer ui for query replace
(use-package anzu
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package flyspell
  :config
  ;; not needed - When Cygwin is in path
  ;; (when (eq system-type 'windows-nt)
  ;;   (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  ;; Don't interfere with avy
  (define-key flyspell-mode-map (kbd "C-;") nil)
  )

(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package crux
  :bind (("C-c o" . crux-open-with)
         ;;("M-o" . crux-smart-open-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c f" . crux-recentf-find-file)
         ("C-M-z" . crux-indent-defun)
         ("C-c u" . crux-view-url)
         ("C-c e" . crux-eval-and-replace)
         ("C-c w" . crux-swap-windows)
         ("M-c" . crux-duplicate-current-line-or-region)
         ;; ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("s-o" . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-c s" . crux-ispell-word-then-abbrev)))

(use-package lorem-ipsum)

;; C-c P: copy a path to the object at point
;; C-c C-k: replace the sexp with null
(use-package json-mode)

;; auto format json on save
(use-package prettier-js
  :config (add-hook 'json-mode-hook 'prettier-js-mode)
  )

;; Format code nicely when yanking
(defun my-yank (&optional arg)
  (if (member major-mode '(json-mode js-mode tide-mode))
      (prettier-js))
  )
(advice-add 'yank :after 'my-yank)

(use-package tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  )

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . typescript-mode))

;; auto edit opposite tag in html
(use-package tagedit
  :init
  (tagedit-add-experimental-features)
  (unbind-key (kbd "#") tagedit-mode-map)
  :config (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))
  )

(use-package expand-region
  :config
  (global-set-key (kbd "C-'") 'er/expand-region)
  )

(use-package org
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell      . t)
                                 (js         . t)
                                 (emacs-lisp . t)
                                 (perl       . t)
                                 (python     . t)
                                 (ruby       . t)
                                 (css        . t)
                                 (plantuml   . t))))

(setq org-agenda-files '("c:/cura/todo/"))

(require 'setup-magit)
(require 'setup-dired)

(require 'my-html)
(require 'editing-defuns)
(require 'buffer-defuns)

(require 'my-bindings)

(use-package git-gutter
  :init
  (global-git-gutter-mode t)
  )

(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  (doom-themes-org-config)
  )

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  )

;; start server
(server-start)

(windmove-default-keybindings)
;; Make windmove work in Org mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

