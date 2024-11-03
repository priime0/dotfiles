;;; priime-vc.el --- Version control configuration
;;; Commentary:
;; Provides configuration for Magit.
;;; Code:

(use-package magit :straight t
  :bind (("<f5>" . magit-status)
         ("C-x g" . magit-status)))
(use-package forge :straight t
  :after (magit))

(provide 'priime-vc)

;;; priime-vc.el ends here
