;;; priime-elisp.el --- Elisp configuration
;;; Commentary:
;; Provides configuration for Elisp-related libraries and functions
;;; Code:

(require 'straight)

(use-package s :straight t)
(use-package f :straight t)
(use-package dash :straight t)
(straight-use-package 'emacs-async)

(provide 'priime-elisp)

;;; priime-elisp.el ends here
