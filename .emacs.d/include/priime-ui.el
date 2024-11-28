;;; priime-ui.el --- UI configuration
;;; Commentary:
;; Provides configuration for the UI.
;;; Code:

(require 'projectile)
(require 'magit)

(defun neotree-toggle-dir-or-project ()
  "By default, toggle the directory at the project level, with prefix current."
  (interactive)
  (if (eq current-prefix-arg nil)
      (neotree-toggle-project)
    (neotree-toggle-current-directory)))

(defun neotree-toggle-project ()
  "Toggle neotree at the project level if the project exists, otherwise current."
  (interactive)
  (if (projectile-project-p)
      (neotree-toggle-directory (projectile-project-root))
    (neotree-toggle-current-directory)))

(defun neotree-toggle-current-directory ()
  "Toggle neotree at the current directory."
  (interactive)
  (let ((current-directory
         (or (and buffer-file-name (file-name-directory buffer-file-name))
             (and (eq major-mode 'dired-mode) (dired-current-directory))
             (and (eq major-mode 'magit-status-mode) (magit-toplevel))
             "~")))
    (neotree-toggle-directory current-directory)))

(defun neotree-toggle-directory (dir)
  "Toggle neotree at the given DIR."
  (if (and (fboundp 'neo-global--window-exists-p)
           (neo-global--window-exists-p))
      (neotree-hide)
    (neotree-dir dir)))

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
  :hook ((prog-mode . hl-todo-mode)
         (LaTeX-mode . hl-todo-mode))
  :bind (("C-c r h" . hl-todo-rgrep))
  :custom-face
  (hl-todo ((t (:inherit nano-salient-i))))
  :custom
  '(hl-todo-keyword-faces
    (("HOLD" .   "#ffffff")
     ("TODO" .   "#ffffff")
     ("NEXT" .   "#ffffff")
     ("THEM" .   "#ffffff")
     ("PROG" .   "#ffffff")
     ("OKAY" .   "#ffffff")
     ("DONT" .   "#ffffff")
     ("FAIL" .   "#ffffff")
     ("DONE" .   "#ffffff")
     ("NOTE" .   "#ffffff")
     ("MAYBE" .  "#ffffff")
     ("KLUDGE" . "#ffffff")
     ("HACK" .   "#ffffff")
     ("TEMP" .   "#ffffff")
     ("FIXME" .  "#ffffff")
     ("XXXX*" .  "#ffffff"))))

(provide 'priime-ui)

;;; priime-ui.el ends here
