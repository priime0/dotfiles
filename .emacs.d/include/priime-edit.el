;;; priime-edit.el --- Edit configuration
;;; Commentary:
;; Provides configuration for edit-related packages, including
;; syntax-checking and formatting.
;;; Code:

(use-package meow :straight t
  :init
  (meow-global-mode)
  (meow-setup-indicator))
(use-package flycheck :straight t
  :custom
  (flycheck-check-syntax-automatically '(save mode-enable))
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode))
(use-package paredit :straight t
  :bind (("M-<backspace>" . #'backward-kill-sexp)
         ("M-k" . #'kill-sexp))
  :hook ((emacs-lisp-mode scheme-mode clojure-mode lisp-mode)
         . paredit-mode))
(use-package undo-tree :straight t
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))
(use-package format-all :straight t
  :bind ("C-c f" . format-all-region-or-buffer))
(use-package ws-butler :straight t
  :hook (prog-mode . ws-butler-mode))

(provide 'priime-edit)

;;; priime-edit.el ends here
