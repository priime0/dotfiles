;;; general.el --- General configuration settings.
;;; Commentary:
;;    General configuration for Emacs.
;;; Code:

(require 'nano-theme)

(setq inhibit-startup-message t
      visible-bell            nil)

;; Backups
(setq make-backup-files nil)
(setq vc-make-backup-files nil)
(setq kept-new-versions nil)
(setq kept-old-versions nil)
(setq delete-old-versions t)
(rassq-delete-all 'auto-save-mode auto-mode-alist)
(setq auto-save-default nil)
(setq auto-save-mode -1)
(setq backup-directory-alist '(("." . "~/.emacs/backups/")))
(setq backup-by-copying t)

;; UI
(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)

;; Line
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(hl-line-mode -1)
(setq line-number-mode t)
(setq column-number-mode t)

;; Tabs
(setq-default tab-width 2)

;; Cursor
(blink-cursor-mode 1)

;; Font
(defvar font-size 10)
(defvar font-family "JetBrains Mono SemiBold")
(set-frame-font (format "%s %d" font-family font-size))
(custom-set-faces
 '(shr-text ((t (:inherit variable-pitch-text :family "Noto Sans Light")))))

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Column width
(setq fill-column 80)

;; Directional window moving
(windmove-default-keybindings)

;; Auto file refresh
(global-auto-revert-mode t)

;; Automatically follow symlinks
(setq vc-follow-symlinks t)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'nano t)
(nano-light)

;; Keybindings
(global-set-key (kbd "C-M-j") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "C-M-k") (lambda () (interactive) (scroll-down 1)))
(global-unset-key (kbd "C-x f"))
(global-set-key (kbd "C-x f f") #'set-fill-column)
(global-set-key (kbd "C-x f i") #'display-fill-column-indicator-mode)

;; Garbage Collection
(setq gc-cons-threshold 50000000)

;; Confirmations
(fset 'yes-or-no-p 'y-or-n-p)


(provide 'general)
;;; general.el ends here
