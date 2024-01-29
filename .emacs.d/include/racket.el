;;; racket.el --- Configuration for Racket.
;;; Commentary:
;;    Configuration for Racket and Racket-based languages, including Pollen.
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp/pollen-mode" user-emacs-directory))
(autoload 'pollen-mode "pollen" "A major mode for the pollen preprocessor." t)

;; Recognize pollen filetypes
(setq auto-mode-alist (cons '("\\.pp$" . racket-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pm$" . pollen-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pmd$" . pollen-mode) auto-mode-alist))

(defun configure-racket-repl ()
  "Configure keybinds for the racket repl."
  (keymap-local-set "C-c C-a" #'racket-repl-clear-leaving-last-prompt))

(add-hook 'racket-repl-mode-hook #'configure-racket-repl)

(defun configure-scribble ()
  "Configure keybinds for scribble."
  (keymap-local-set "C-c C-c" #'compile))

(add-hook 'scribble-mode-hook #'configure-scribble)

(provide 'racket)
;;; racket.el ends here
