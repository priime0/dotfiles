;;; priime-org.el --- Org-mode configuration
;;; Commentary:
;; Provides configuration for org-related packages.
;;; Code:

(use-package org :straight t)
(use-package org-roam :straight t
  :after (org))
(use-package org-modern :straight t
  :after (org)
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star 'fold))
(use-package org-modern-indent
  :straight
  '(org-modern-indent
    :type git
    :host github
    :repo "jdtsmith/org-modern-indent")
  :after (org-modern)
  :hook (org-mode . org-modern-indent-mode))
(use-package ob-racket
  :straight
  '(ob-racket
    :type git
    :host github
    :repo "hasu/emacs-ob-racket"))

(provide 'priime-org)

;;; priime-org.el ends here
