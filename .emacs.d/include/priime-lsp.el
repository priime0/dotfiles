;;; priime-lsp.el --- LSP configuration
;;; Commentary:
;; Provides configuration for LSP-related packages.
;;; Code:

(use-package eglot :straight t
  :custom-face (eglot-inlay-hint-face ((t (:height 1.0))))
  :hook ((racket-mode rust-mode irony-mode) . eglot-ensure)
  :init
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode)
                 ("rust-analyzer" :initializationOptions (:checkOnSave (:command "clippy"))))))
(use-package eldoc :straight t
  :after (eglot))
(use-package eldoc-box :straight t
  :after (eldoc))

(provide 'priime-lsp)

;;; priime-lsp.el ends here
