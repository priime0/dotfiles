;;; general.el --- General configuration settings.
;;; Commentary:
;;    General configuration for Emacs.
;;; Code:

(require 'nano-theme)
(require 'view)



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
(setq inhibit-startup-message t)
(setq visible-bell            nil)
(setq truncate-lines          t)

;; Scratch
(setq initial-scratch-message nil)

;; Line
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(hl-line-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

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

;; Improve performance in files with long lines
(global-so-long-mode t)

;; Automatically follow symlinks
(setq vc-follow-symlinks t)

;; Undos
(setq undo-limit 200000)

;; Sentences
;; Emacs thinks that sentences ends with period and two spaces,
;; disable this.
(setq sentence-end-double-space nil)

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
(global-set-key (kbd "C-x f a") #'auto-fill-mode)
(global-set-key (kbd "C-v")     #'View-scroll-half-page-forward)
(global-set-key (kbd "M-v")     #'View-scroll-half-page-backward)

;; Garbage Collection
(setq gc-cons-threshold 50000000)

;; Confirmations
(fset 'yes-or-no-p 'y-or-n-p)


(provide 'general)
;;; general.el ends here
