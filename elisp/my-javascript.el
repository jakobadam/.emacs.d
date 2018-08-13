(provide 'my-javascript)

;; JS2 Mode

;;  I like the extras found in
;;  [[http://www.emacswiki.org/emacs-test/SteveYegge][Steve Yegge]]'s
;;  [[https://github.com/mooz/js2-mode][js2-mode]].

(use-package js2-mode
  :ensure t
  :init
  (setq js-basic-indent 2)
  (setq-default js2-basic-indent 2
                js2-basic-offset 2
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t
                js2-global-externs (list "window" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "jQuery" "$"))

  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("function" . ?ƒ) prettify-symbols-alist)))

  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;; Color /defined/ variables with
;; [[https://github.com/ankurdave/color-identifiers-mode][color-identifiers-mode]]:

(use-package color-identifiers-mode
  :ensure t
  :init
  (add-hook 'js2-mode-hook 'color-identifiers-mode))

;; Flycheck and JSHint

;;  While editing JavaScript is baked into Emacs, it is quite
;;  important to have [[http://flycheck.readthedocs.org/][flycheck]]
;;  validate the source based on [[http://www.jshint.com/][jshint]],
;;  and [[https://github.com/eslint/eslint][eslint]]. Let’s prefer =eslint=:

(add-hook 'js2-mode-hook
          (lambda () (flycheck-select-checker "javascript-eslint")))

;;  Now load and edit a JavaScript file, like [[file:~/jshint-code-test.js][jshint-code-test.js]].

;; Tern

;;  The [[http://ternjs.net/doc/manual.html#emacs][Tern]] project is a JavaScript analyzer that can be used to
;;  improve the JavaScript integration with editors like Emacs.

(use-package tern
  :ensure t
  :init (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  :config
  (use-package company-tern
    :ensure t
    :init (add-to-list 'company-backends 'company-tern)))

;;   The following additional keys are bound:

;; - ~M-.~ :: Jump to the definition of the thing under the cursor.
;; - M-, :: Brings you back to last place you were when you pressed ~M-.~.
;; - ~C-c C-r~ :: Rename the variable under the cursor.
;; - ~C-c C-c~ :: Find the type of the thing under the cursor.
;; - ~C-c C-d~ :: Find docs of the thing under the cursor. Press again to open the associated URL (if any).

;; Refactoring JavaScript

;;  The [[https://github.com/magnars/js2-refactor.el][js2-refactor]]
;;  mode should start with =C-c .= and then a two-letter mnemonic
;;  shortcut.

  ;; * =ef= is =extract-function=: Extracts the marked expressions out into a new named function.
  ;; * =em= is =extract-method=: Extracts the marked expressions out into a new named method in an object literal.
  ;; * =ip= is =introduce-parameter=: Changes the marked expression to a parameter in a local function.
  ;; * =lp= is =localize-parameter=: Changes a parameter to a local var in a local function.
  ;; * =eo= is =expand-object=: Converts a one line object literal to multiline.
  ;; * =co= is =contract-object=: Converts a multiline object literal to one line.
  ;; * =eu= is =expand-function=: Converts a one line function to multiline (expecting semicolons as statement delimiters).
  ;; * =cu= is =contract-function=: Converts a multiline function to one line (expecting semicolons as statement delimiters).
  ;; * =ea= is =expand-array=: Converts a one line array to multiline.
  ;; * =ca= is =contract-array=: Converts a multiline array to one line.
  ;; * =wi= is =wrap-buffer-in-iife=: Wraps the entire buffer in an immediately invoked function expression
  ;; * =ig= is =inject-global-in-iife=: Creates a shortcut for a marked global by injecting it in the wrapping immediately invoked function expression
  ;; * =ag= is =add-to-globals-annotation=: Creates a =/*global */= annotation if it is missing, and adds the var at point to it.
  ;; * =ev= is =extract-var=: Takes a marked expression and replaces it with a var.
  ;; * =iv= is =inline-var=: Replaces all instances of a variable with its initial value.
  ;; * =rv= is =rename-var=: Renames the variable on point and all occurrences in its lexical scope.
  ;; * =vt= is =var-to-this=: Changes local =var a= to be =this.a= instead.
  ;; * =ao= is =arguments-to-object=: Replaces arguments to a function call with an object literal of named arguments. Requires yasnippets.
  ;; * =3i= is =ternary-to-if=: Converts ternary operator to if-statement.
  ;; * =sv= is =split-var-declaration=: Splits a =var= with multiple vars declared, into several =var= statements.
  ;; * =uw= is =unwrap=: Replaces the parent statement with the selected region.


(use-package js2-refactor
  :ensure t
  :init   (add-hook 'js2-mode-hook 'js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c ."))



;; I also configure Skewer for my [[file:emacs-web.org][HTML and CSS]]
;; files, we need to do the same for JavaScript:

(use-package skewer-mode
  :ensure t
  :init (add-hook 'js2-mode-hook 'skewer-mode))

 ;;  Kick things off with =run-skewer=, and then:

 ;; * C-x C-e :: `skewer-eval-last-expression'
 ;; * C-M-x   :: `skewer-eval-defun'
 ;; * C-c C-k :: `skewer-load-buffer'

;; * Technical Artifacts

;;  Make sure that we can simply =require= this library.

