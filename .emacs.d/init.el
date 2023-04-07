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
(straight-use-package 'vertico
		      :init (vertico-mode))
(straight-use-package 'marginalia)
(straight-use-package 'racket-mode)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'rust-mode)
(straight-use-package 'which-key)
(straight-use-package 'neotree)
(straight-use-package 'flycheck)
(straight-use-package 'git-gutter)
(straight-use-package 'paredit)
(straight-use-package 'tuareg)
(straight-use-package 'projectile)

(add-to-list 'load-path (expand-file-name "lisp/pollen-mode" user-emacs-directory))
(autoload 'pollen-mode "pollen" "A major mode for the pollen preprocessor." t)

;; ====== Configuration ======================

(setq inhibit-startup-message t
      visible-bell            nil)

;; UI
(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)

;; Line
(global-display-line-numbers-mode 1)
(hl-line-mode -1)

;; Cursor
(blink-cursor-mode 1)

;; Font
(defvar font-size 10)
(defvar font-family "Roboto Mono Medium")
(set-frame-font (format "%s %d" font-family font-size))

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq catppuccin-flavor 'latte)
(load-theme 'catppuccin t)

;; ====== Hooks ==============================
(add-hook 'after-init-hook      'global-company-mode)
(add-hook 'after-init-hook      'marginalia-mode)
(add-hook 'after-init-hook      'which-key-mode)
(add-hook 'after-init-hook      'global-flycheck-mode)
(add-hook 'after-init-hook      'git-gutter-mode)
(add-hook 'company-mode-hook    'company-box-mode)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook     'paredit-mode)
(add-hook 'racket-mode-hook     'paredit-mode)
(add-hook 'clojure-mode-hook    'paredit-mode)

(add-hook 'racket-mode-hook     'lsp-racket-enable)

(add-hook 'lsp-mode             'lsp-ui-mode)
