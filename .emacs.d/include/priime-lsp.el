;;; priime-lsp.el --- LSP configuration
;;; Commentary:
;; Provides configuration for LSP-related packages.
;;; Code:

(use-package eglot :straight t
  :custom
  (eglot-events-buffer-config '(:size 0 :format full))
  :custom-face (eglot-inlay-hint-face ((t (:height 1.0))))
  :hook ((racket-mode rust-mode irony-mode tuareg-mode python-mode tsx-ts-mode nix-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
         ("C-c l r" . eglot-rename)
         ("C-c l a" . eglot-code-actions)
         ("C-c l f" . eglot-format))
  :init
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-server-programs '(nix-mode "nil")))
(use-package eglot-booster
  :straight (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
  :after (eglot)
  :config (eglot-booster-mode))
(use-package eldoc :straight t
  :after (eglot))
(use-package eldoc-box :straight t
  :after (eldoc)
  :hook (eldoc-mode . eldoc-box-hover-mode))

(provide 'priime-lsp)

;;; priime-lsp.el ends here
