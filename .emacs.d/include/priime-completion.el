;;; priime-completion.el --- Completion configuration
;;; Commentary:
;; Provides configuration for completion-related packages.
;;; Code:

(use-package corfu :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (corfu-auto t)
  (corfu-popupinfo-delay 1)
  :bind (:map corfu-map
              ("C-n"   . corfu-next)
              ("C-p"   . corfu-previous)
              ("<tab>" . corfu-insert)
              ("RET"   . nil))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))
(use-package cape :straight t
  :bind ("M-p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-keyword))
(use-package vertico :straight t
  :init
  (add-hook 'after-init-hook 'vertico-mode))
(use-package marginalia :straight t
  :init
  (add-hook 'after-init-hook 'marginalia-mode))
(use-package orderless :straight t)
(use-package consult :straight t)
(use-package yasnippet :straight t)
(use-package yasnippet-snippets :straight t)

(provide 'priime-completion)

;;; priime-completion.el ends here
