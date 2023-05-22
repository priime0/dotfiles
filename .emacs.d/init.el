;; priime0 emacs configuration file

;; ====== Packages ===========================

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'company)
(straight-use-package 'company-box)
(straight-use-package 'company-math)
(straight-use-package 'magit)
(straight-use-package 'vertico)
(straight-use-package 'marginalia)
(straight-use-package 'racket-mode)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'rust-mode)
(straight-use-package 'rustic)
(straight-use-package 'which-key)
(straight-use-package 'neotree)
(straight-use-package 'flycheck)
(straight-use-package 'git-gutter)
(straight-use-package 'paredit)
(straight-use-package 'tuareg)
(straight-use-package 'projectile)
(straight-use-package 'workgroups2)
(straight-use-package 'markdown-mode)
(straight-use-package 'web-mode)
(straight-use-package 'just-mode)
(straight-use-package 'justl)
(straight-use-package 'org)
(straight-use-package 'org-roam)

(add-to-list 'load-path (expand-file-name "lisp/pollen-mode" user-emacs-directory))
(autoload 'pollen-mode "pollen" "A major mode for the pollen preprocessor." t)

;; ====== Configuration ======================

(setq inhibit-startup-message t
      visible-bell            nil)

;; Backup directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 5
      kept-old-versions 5)

;; UI
(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)

;; Line
(global-display-line-numbers-mode 1)
(hl-line-mode -1)
(setq line-number-mode t)
(setq column-number-mode t)

;; Cursor
(blink-cursor-mode 1)

;; Font
(defvar font-size 10)
(defvar font-family "Roboto Mono Medium")
(set-frame-font (format "%s %d" font-family font-size))

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Directional window moving
(windmove-default-keybindings)

;; Auto file refresh
(global-auto-revert-mode t)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq catppuccin-flavor 'latte)
(load-theme 'catppuccin t)

;; ====== Keybindings ========================

(global-set-key (kbd "C-c g g") 'magit)
(global-set-key (kbd "C-M-j") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "C-M-k") (lambda () (interactive) (scroll-down 1)))

;; ====== Filetypes ==========================

(setq auto-mode-alist (cons '("\\.pp$" . racket-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pm$" . racket-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\")))
(setq web-mode-markup-indent-offset 2)

(setq lsp-rust-analyzer-server-display-inlay-hints t)

;; ====== Hooks ==============================
(add-hook 'after-init-hook      'global-company-mode)
(add-hook 'after-init-hook      'marginalia-mode)
(add-hook 'after-init-hook      'which-key-mode)
(add-hook 'after-init-hook      'global-flycheck-mode)
(add-hook 'after-init-hook      'git-gutter-mode)
(add-hook 'after-init-hook      'workgroups-mode)
(add-hook 'after-init-hook      'git-gutter-mode)
(add-hook 'after-init-hook      'vertico-mode)

(add-hook 'company-mode-hook    'company-box-mode)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook     'paredit-mode)
(add-hook 'racket-mode-hook     'paredit-mode)
(add-hook 'clojure-mode-hook    'paredit-mode)

;(add-hook 'racket-mode-hook     'lsp-racket-enable)

(add-hook 'lsp-mode             'lsp-ui-mode)

(add-hook 'python-mode          'lsp-mode)
(add-hook 'rust-mode            'lsp-rust)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column 80))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
