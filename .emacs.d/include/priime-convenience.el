;;; priime-convenience.el --- Convenience packages configuration
;;; Commentary:
;; Provides configuration for convenient packages that improve
;; development experience and productivity.
;;; Code:

(defun pdf-download-and-view (&optional url filename)
  "Download and view the PDF given by its URL as FILENAME."
  (interactive)
  (let* ((url (or url
                  (read-string "Download URL: ")))
         (default-filename (or filename
                               (car (last (split-string url "/" t)))))
         (filename (or filename
                       (read-string (format "Filename (%s): " default-filename)
                                    nil
                                    nil
                                    default-filename)))
         (file-path (concat "/tmp/" filename))
         (download-buffer (url-retrieve-synchronously url)))
    (set-buffer download-buffer)
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (forward-char)
    (delete-region (point-min) (point))
    (write-file file-path)
    (find-file (expand-file-name file-path))))

(defun priime-split-terminal ()
  "Split a terminal on the right and move to it."
  (interactive)
  (priime-split-right)
  (if (projectile-project-p)
      (projectile-run-vterm)
    (vterm))
  (balance-windows))

(use-package projectile :straight t
  :custom
  (projectile-completion-system 'auto)
  :bind
  (("C-c p" . projectile-command-map))
  :init
  (add-hook 'after-init-hook 'projectile-global-mode))
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
(use-package vterm :straight t
  :custom (vterm-shell (or (executable-find "fish") shell-file-name))
  :bind (("C-c v" . vterm)
         ("<f8>" . priime-split-terminal)))
(use-package rg :straight t)
(use-package anzu :straight t
  :init
  (add-hook 'after-init-hook 'global-anzu-mode))
(use-package pdf-tools
  :straight
  '(pdf-tools :type git :host github
              :repo "vedang/pdf-tools")
  :custom
  (doc-view-resolution 300)
  (pdf-view-continuous t)
  :init
  (pdf-tools-install))
(use-package olivetti :straight t
  :hook (org-mode . olivetti-mode))

(provide 'priime-convenience)

;;; priime-convenience.el ends here
