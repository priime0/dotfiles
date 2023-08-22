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

(provide 'racket)
;;; racket.el ends here
