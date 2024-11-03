;;; priime-ui.el --- UI configuration
;;; Commentary:
;; Provides configuration for the UI.
;;; Code:

(use-package nano-theme
  :straight '(nano-theme :type git :host github
                         :repo "rougier/nano-theme")
  :init
  (load-theme 'nano t)
  (nano-light))
(use-package all-the-icons :straight t)
(use-package neotree :straight t
  :after (all-the-icons)
  :custom
  ((neo-theme 'icons)
   (neo-smart-open t)
   (neo-window-fixed-size nil)
   (neo-show-hidden-files t))
  :bind (("C-c t" . #'neotree-toggle-dir-or-project)))
(use-package git-gutter :straight t
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:update-interval 1)
  :config
  (git-gutter:start-update-timer))
(use-package hl-todo :straight t
  :hook (prog-mode . hl-todo-mode))

(provide 'priime-ui)

;;; priime-ui.el ends here
