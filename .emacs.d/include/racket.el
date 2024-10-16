;;; racket.el --- Configuration for Racket.
;;; Commentary:
;;    Configuration for Racket and Racket-based languages, including Pollen.
;;; Code:

(require 'racket-mode)
(require 'racket-edit)

(add-to-list 'load-path (expand-file-name "lisp/pollen-mode" user-emacs-directory))
(autoload 'pollen-mode "pollen" "A major mode for the pollen preprocessor." t)

(defun racket-repl-switch ()
  "Switch to the Racket REPL."
  (interactive)
  (racket-edit-switch-to-repl)
  (unless (equal current-prefix-arg nil)
    (delete-other-windows)))

(defun racket-edit-switch ()
  "Switch to the corresponding racket buffer."
  (interactive)
  (racket-repl-switch-to-edit)
  (unless (equal current-prefix-arg nil)
    (delete-other-windows)))

;; Recognize pollen filetypes
(setq auto-mode-alist (cons '("\\.pp$" . racket-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pm$" . pollen-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pmd$" . pollen-mode) auto-mode-alist))

;; raco review flycheck support
(flycheck-define-checker racket-review
  "check racket source code using racket review"
  :command ("raco" "review" source)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":error:" (message) line-end)
   (warning line-start (file-name) ":" line ":" column ":warning:" (message) line-end))
  :modes racket-mode)
(add-to-list 'flycheck-checkers 'racket-review)
(add-to-list 'flycheck-disabled-checkers 'racket)

(defun configure-racket ()
  "Configure keybinds for racket buffers."
  (keymap-local-set "C-c C-z" #'racket-repl-switch))

(add-hook 'racket-mode-hook #'configure-racket)

(defun configure-racket-repl ()
  "Configure keybinds for the racket repl."
  (keymap-local-set "C-c C-k" #'racket-repl-clear-leaving-last-prompt)
  (keymap-local-set "C-c C-z" #'racket-edit-switch))

(add-hook 'racket-repl-mode-hook #'configure-racket-repl)

(defun configure-scribble ()
  "Configure keybinds for scribble."
  (keymap-local-set "C-c C-c" #'compile))

(add-hook 'scribble-mode-hook #'configure-scribble)

(provide 'racket)
;;; racket.el ends here
