;; General Settings

(defconst jd/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))
(defun jd/emacs-subdirectory (d) (expand-file-name d jd/emacs-directory))

;; General settings - Directory Structure

;; In case this is the first time running this on a computer, we need
;; to make sure the following directories have been created.

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

;; Package Initialization - Package Manager
(require 'package)

(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")))

(package-initialize)

;; only update the package list of not present
(unless package-archive-contents
  (package-refresh-contents))

;; Package Initialization - Use-Package

;; Using https://github.com/jwiegley/use-package to automatically
;; install certain packages, as well as the ease of lazily loading
;; them.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Package Initialization - Init File Support

;; Load up a collection of enhancements to Emacs Lisp, including
;; dash, s for string manipulation, and f for file manipulation.
(require 'cl)

(use-package dash
  :ensure t
  :config (eval-after-load "dash" '(dash-enable-font-lock)))

(use-package s
  :ensure t)

(use-package f
  :ensure t)

;; Variables

;; General settings about me that other packages can use. The biggest
;; problem is guessing my email address based on what computer I am
;; using:
(if (equal "guest-jdam" user-login-name)
    (setq user-mail-address "jakob.dam@systematic.com")
  (setq user-mail-address "jakob.a.dam@gmail.com"))

;; Variables - Tabs vs Spaces

;; Only use spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; Make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)

;; Variables - Misc Variable Settings
;; Does anyone type yes anymore?
(fset 'yes-or-no-p 'y-or-n-p)

;; Fix the scrolling to keep point in the center:
(setq scroll-conservatively 10000
      scroll-preserve-screen-position t)

;; Display Settings
(setq inhibit-startup-screen t)
(setq initial-scratch-message "") ;; Uh, I know what Scratch is for
(setq visible-bell t)             ;; Get rid of the beeps

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
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

;; Display Settings - Fill Mode

;; Automatically wrapping when you get to the end of a line (or the
;; fill-region):
(use-package fill
  :bind (("C-c T f" . auto-fill-mode)
         ("C-c T t" . toggle-truncate-lines))
  :init (add-hook 'org-mode-hook 'turn-on-auto-fill)
  :diminish auto-fill-mode)

;; Key Bindings - Display Command Sequences
(use-package which-key
  :ensure t
  :defer 10
  :diminish which-key-mode
  :config (which-key-mode 1))

;; Key Bindings - Undo and Redo

;; According to this article, I get better functionality than the
;; redo+ plugin (which I can’t seem to get working well).

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode 1)
  :config
  (defalias 'redo 'undo-tree-redo)
  :bind (("C-z" . undo)     ; Zap to character isn't helpful
         ("C-S-z" . redo)))

;; Key Bindings - Jumping to Windows
(use-package ace-window
  :ensure t
  :config
  (global-set-key [remap other-window] 'ace-window))

;; Key Bindings - Better Jumping
(use-package avy
  :ensure t
  :init (setq avy-background t)
  :bind ("C-;" . avy-goto-char-timer))

;; Key Bindings - Expand Region

;; Wherever you are in a file, and whatever the type of file, you can
;; slowly increase a region selection by logical segments by using
;; Magnar’s expand-region project.

;; However, the normal experience for expand-region is interactive,
;; expected to be called repeatedly to expand and contract the regions
;; based on syntax, and whatnot. Since I am seldom sure what I will
;; select if I give this function a numeric prefix, I created a wrapper
;; function that will (when given a number), just select the number of
;; lines for the region. Select the current line with a 0 argument. No
;; argument (well, lines is given 1 with no argument), then it just calls
;; expand-region:
(use-package expand-region
  :ensure t
  :config
  (defun jd/expand-region (lines)
    "Prefix-oriented wrapper around Magnar's `er/expand-region'.

Call with LINES equal to 1 (given no prefix), it expands the
region as normal.  When LINES given a positive number, selects
the current line and number of lines specified.  When LINES is a
negative number, selects the current line and the previous lines
specified.  Select the current line if the LINES prefix is zero."
    (interactive "p")
    (cond ((= lines 1)   (er/expand-region 1))
          ((< lines 0)   (jd/expand-previous-line-as-region lines))
          (t             (jd/expand-next-line-as-region (1+ lines)))))

  (defun jd/expand-next-line-as-region (lines)
    (message "lines = %d" lines)
    (beginning-of-line)
    (set-mark (point))
    (end-of-line lines))

  (defun jd/expand-previous-line-as-region (lines)
    (end-of-line)
    (set-mark (point))
    (beginning-of-line (1+ lines)))

  :bind ("C-=" . jd/expand-region))

;; Key Bindings - Block Wrappers

;; But wrap-region is even more flexible. In most editors, selecting
;; text and typing anything replaces the selected text (see the
;; delete-selection-mode), but in this case, we can do something
;; different… like wrapping:

(use-package wrap-region
  :ensure   t
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

;; Loading and Finding Files - Find file in project
;; https://github.com/technomancy/find-file-in-project
(use-package find-file-in-project
  :ensure t
  :bind ("S-C-n" . find-file-in-project)
  ;; inspiration for more bindings https://github.com/magnars/.emacs.d/blob/5ff65739ebda23cfeffa6f70a3c7ecf49b6154ae/settings/key-bindings.el#L314
  )

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  ;;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-mode +1))

;; Loading and finding files - Dired Options
(use-package dired-launch
  :ensure t
  :config (when (eq system-type 'windows-nt)
            (setq dired-launch-default-launcher '("start")))
  
  
  :init (dired-launch-enable)
  )

;; The dired-x project seems useful:
;; https://www.masteringemacs.org/article/dired-shell-commands-find-xargs-replacement
(use-package dired-x)


;; Loading and finding files - IDO (Interactively DO Things)

;; According to Mickey, IDO is the greatest thing.
(use-package ido
  :ensure t
  :init  (setq ido-enable-flex-matching t
               ido-ignore-extensions t
               ido-use-virtual-buffers t
               ido-everywhere t)
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (add-to-list 'completion-ignored-extensions ".pyc"))

;; https://github.com/lewang/flx
(use-package flx-ido
   :ensure t
   :init (setq ido-enable-flex-matching t
               ido-use-faces nil)
   :config (flx-ido-mode 1))

;; https://github.com/creichert/ido-vertical-mode.el
(use-package ido-vertical-mode
  :ensure t
  :init               ; I like up and down arrow keys:
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  :config
  (ido-vertical-mode 1))


;; Loading and finding files - Editing Root Files

;; Once I wrote a find-file-as-root function (graciously borrowed
;; from Emacs Fu), however, bbatsov gave me a better idea to lend
;; some advice to find-file, so that non-writable files would be
;; automatically re-opened using the sudo feature of Tramp.

;; My version works with both local and remotely access files:
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (let* ((file-name (buffer-file-name))
           (file-root (if (string-match "/ssh:\\([^:]+\\):\\(.*\\)" file-name)
                          (concat "/ssh:"  (match-string 1 file-name)
                                  "|sudo:" (match-string 1 file-name)
                                  ":"      (match-string 2 file-name))
                        (concat "/sudo:localhost:" file-name))))
      (find-alternate-file file-root))))

;; No special key-bindings, just load up a file, and if I can’t write
;; it, it will automatically ask me for my credentials, and away I go.

;; SMEX

;; Built using IDO to do something similar but with M-x commands:
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex)
  ("M-X" . smex-major-mode-commands))

;; Backup Settings

;; This setting moves all backup files to a central location. Got it
;; from this page.
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (jd/emacs-subdirectory "backups")))))

;; Tramp should do the same:
(setq tramp-backup-directory-alist backup-directory-alist)

;; Make backups of files, even when they’re in version control:
(setq vc-make-backup-files t)

;; And let’s make sure our files are saved if we wander off and
;; defocus the Emacs application:
(defun save-all ()
  "Save all dirty buffers without asking for confirmation."
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)

;; Auto Complete
;; Using company-mode for all my auto completion needs.
(use-package company
  :ensure t
  :init
  (setq company-dabbrev-ignore-case t
        company-show-numbers t)
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  :bind ("C-:" . company-complete)  ; In case I don't want to wait
  :diminish company-mode)

;; Take advantage of idle time by displaying some documentation using
;; company-quickhelp project.
(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

;; Yasnippets
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (jd/emacs-subdirectory "snippets")))

;; also load the snippets:)
(use-package yasnippet-snippets
  :ensure t)

;; Code folding
;; C-return - toggle element
;; C-M-return - toggle all
;; C-S-return - hide parent
(use-package yafolding
  :ensure t
  :init (add-hook 'prog-mode-hook (lambda () (yafolding-mode)))
  )

;; https://editorconfig.org/
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(setq-default ispell-program-name "aspell")

;; (custom-set-variables '(ispell-program-name "C:\\cygwin64\\bin\\aspell.exe"))

;; 
(require 'setup-magit)
(require 'setup-dired)
(require 'my-javascript)
(require 'my-html)
(require 'editing-defuns)
(require 'buffer-defuns)

(require 'my-bindings)

;; Tools

(use-package ido-completing-read+
  :ensure t
  )

(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode t)
  )

(defun is-in-terminal()
    (not (display-graphic-p)))

(use-package solarized-theme
  :ensure t
  :init
  ;; Change Theme Hint: M-x load-theme soloarized-light
  ;; don't load theme in terminal
  (when (not (is-in-terminal))
    (load-theme 'solarized-dark t))
  )

;; start server
(server-start)

(windmove-default-keybindings)
