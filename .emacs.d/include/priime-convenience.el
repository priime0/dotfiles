;;; priime-convenience.el --- Convenience packages configuration
;;; Commentary:
;; Provides configuration for convenient packages that improve
;; development experience and productivity.
;;; Code:

(use-package projectile :straight t
  :custom
  (projectile-completion-system 'auto))
(use-package bufler :straight t
  :bind (("C-x C-b" . bufler-list)
         ("C-x b" . bufler-switch-buffer))
  :init
  (bufler-mode 1))
(use-package embark :straight t
  :bind (("C-." . embark-act)))
(use-package embark-consult :straight t)
(use-package wgrep :straight t)
(use-package no-littering :straight t)
(use-package vterm :straight t)
(use-package rg :straight t)
(use-package anzu :straight t
  :init
  (add-hook 'after-init-hook 'global-anzu-mode))
(use-package pdf-tools
  :straight
  '(pdf-tools :type git :host github
              :repo "vedang/pdf-tools"))
(use-package olivetti :straight t
  :hook (org-mode . olivetti-mode))

(provide 'priime-convenience)

;;; priime-convenience.el ends here
