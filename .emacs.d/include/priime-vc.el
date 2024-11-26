;;; priime-vc.el --- Version control configuration
;;; Commentary:
;; Provides configuration for Magit.
;;; Code:

(use-package magit :straight t
  :custom
  (auth-sources '("~/.authinfo.gpg"))
  (vc-follow-symlink t)
  :bind (("<f5>" . magit-status)
         ("C-x g" . magit-status)))
(use-package forge :straight t
  :after (magit))
(use-package git-link :straight t)

(defun git-link-with-commit ()
  "Create a URL representing the current selection for the current commit."
  (interactive)
  (let* ((link-region (git-link--get-region))
         (old-link (git-link (git-link--remote) (car link-region) (cadr link-region)))
         (branch (git-link--branch))
         (commit (git-link--commit))
         (new-link (s-replace branch commit old-link)))
    (kill-new new-link)
    (message new-link)
    new-link))

(provide 'priime-vc)

;;; priime-vc.el ends here
