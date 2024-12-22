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
    new-link))

(defun git-link--magit-commit-url (type)
  "Create a URL with the given TYPE to the current commit from Magit."
  (let* ((remote (git-link--remote))
         (remote-url (git-link--remote-url remote))
         (short-hash (magit-commit-at-point))
         (full-hash (magit-rev-hash short-hash))
         (target-link (concat remote-url type full-hash)))
    target-link))

(defun git-link-magit-commit-tree ()
  "Create a URL representing a tree to the current commit from Magit."
  (interactive)
  (let* ((target-link (git-link--magit-commit-url "/tree/")))
    (kill-new target-link)
    target-link))

(defun git-link-magit-commit ()
  "Create a URL representing a commit to the current commit from Magit."
  (interactive)
  (let* ((target-link (git-link--magit-commit-url "/commit/")))
    (kill-new target-link)
    target-link))

(provide 'priime-vc)

;;; priime-vc.el ends here
