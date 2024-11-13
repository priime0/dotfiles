;;; priime-lsp.el --- LSP configuration
;;; Commentary:
;; Provides configuration for LSP-related packages.
;;; Code:

(use-package eglot :straight t
  :custom-face (eglot-inlay-hint-face ((t (:height 1.0))))
  :hook ((racket-mode rust-mode irony-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
         ("C-c l r" . eglot-rename)
         ("C-c l a" . eglot-code-actions)
         ("C-c l f" . eglot-format)))
(use-package eldoc :straight t
  :after (eglot))
(use-package eldoc-box :straight t
  :after (eldoc)
  :hook (eldoc-mode . eldoc-box-hover-mode))

(provide 'priime-lsp)

;;; priime-lsp.el ends here
