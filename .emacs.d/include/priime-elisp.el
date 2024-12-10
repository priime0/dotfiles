;;; priime-elisp.el --- Elisp configuration
;;; Commentary:
;; Provides configuration for Elisp-related libraries and functions
;;; Code:

(require 'straight)

(use-package s :straight t)
(use-package f :straight t)
(use-package ht :straight t)
(use-package dash :straight t)
(use-package htmlize :straight t)
(use-package emacs-async :straight t)

(provide 'priime-elisp)

;;; priime-elisp.el ends here
