;;; priime-general.el --- General package configuration
;;; Commentary:
;; Provides general configuration for Emacs.
;;; Code:

(require 'dash)
(require 'view)

;;; Font
(defvar priime--font-config
  (cond ((string= (system-name) "framework") '("Roboto Mono Medium" "Roboto" 10 0.8))
        ((eq system-type 'gnu/linux)         '("Roboto Mono Medium" "Roboto" 10 0.8))
        ((eq system-type 'darwin)            '("Menlo" "Verdana" 12 1))
        (t                                   '("Roboto Mono" "Roboto" 10 0.8))))

(defvar priime-fixed-font    (-first-item priime--font-config))
(defvar priime-variable-font (-second-item priime--font-config))
(defvar priime-font-size     (-third-item priime--font-config))
(defvar priime-fixed-height  (-fourth-item priime--font-config))

;;; Keybindings
(defun priime-split-right ()
  "Split the window right and move to it."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun priime-split-down ()
  "Split the window down and move to it."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun priime-split-terminal ()
  "Split a terminal on the right and move to it."
  (priime-split-right)
  (if (projectile-project-p)
      (projectile-run-vterm)
    (vterm))
  (balance-windows))

(defun priime-scroll-up ()
  "Scroll up granularly."
  (interactive)
  (scroll-up 1))

(defun priime-scroll-down ()
  "Scroll down granularly."
  (interactive)
  (scroll-down 1))

(defun priime-scroll-left (arg)
  "Scroll left granularly by ARG."
  (interactive "P")
  (let ((amt (or arg 1)))
    (scroll-left amt)))

(defun priime-scroll-right (arg)
  "Scroll right granularly by ARG."
  (interactive "P")
  (let ((amt (or arg 1)))
    (scroll-right amt)))

(defun priime-toggle-line-numbers ()
  "Toggle the display of line numbers."
  (interactive)
  (cond ((equal display-line-numbers t)
         (setq display-line-numbers 'relative))
        ((equal display-line-numbers 'relative)
         (setq display-line-numbers t))
        (t
         (display-line-numbers-mode 1)
         (setq display-line-numbers t))))

(defvar-keymap priime-fill-map
  "f" #'set-fill-column
  "i" #'display-fill-column-indicator-mode
  "a" #'auto-fill-mode
  "n" #'priime-toggle-line-numbers)

;;; Configuration

(use-package emacs
  :custom
  ;; Backups
  (make-backup-files nil)
  (vc-make-backup-files nil)
  (kept-new-versions nil)
  (delete-old-versions t)
  (auto-save-default nil)
  (backup-directory-alist '(("." . "~/.emacs.d/backups/")))
  (backup-by-copying t)
  (create-lockfiles nil)
  ;; UI
  (inhibit-startup-message t)
  (truncate-lines t)
  (visible-bell nil)
  (ring-bell-function 'ignore)
  (initial-scratch-message nil)
  (display-line-numbers 'relative)
  (split-height-threshold
   (cond ((string= (system-name) "framework") 100)
         (t 80)))
  (split-width-threshold
   (cond ((string= (system-name) "framework") 180)
         (t 160)))
  ;; Editing
  (indent-tabs-mode nil)
  (fill-column 80)
  (vc-follow-symblinks t)
  (undo-limit 200000)
  (show-trailing-whitespace t)
  (sentence-end-double-space nil)
  (scroll-conservatively 101)
  ;; GC
  (gc-cons-threshold 50000000)
  ;; emacs-nativecomp
  (native-comp-async-report-warnings-errors nil)
  (warning-minimum-level :error)
  ;; Misc
  (epa-file-encrypt-to '("lucas@priime.dev"))
  (epa-file-select-keys 1)

  :custom-face
  (default                  ((t (:family ,priime-fixed-font :weight medium))))
  (region                   ((t (:inherit nano-subtle :background "#EBE5F5"))))
  (italic)                  ((t (:family inherit :slant italic :weight medium)))
  (lazy-highlight           ((t (:inherit region))))
  (variable-pitch           ((t (:inherit default :family ,priime-variable-font :height 125 :weight regular))))
  (fixed-pitch              ((t (:family ,priime-fixed-font :height ,priime-fixed-height :inherit nil))))
  (shr-text                 ((t (:inherit variable-pitch-text :family ,priime-variable-font))))
  (line-number              ((t (:inherit default :foreground "#98A4AE"))))
  (line-number-current-line ((t (:inherit default :foreground "#98A4AE"))))

  :bind
  (("C-v" . View-scroll-half-page-forward)
   ("M-v" . View-scroll-half-page-backward))

  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode 1)
  (size-indication-mode 1)
  (blink-cursor-mode 1)
  (global-auto-revert-mode 1)
  (global-so-long-mode 1)

  (rassq-delete-all 'auto-save-mode auto-mode-alist)
  (windmove-default-keybindings)
  (put 'scroll-left 'disabled nil)
  (put 'scroll-right 'disabled nil)

  (fset 'yes-or-no-p 'y-or-n-p)

  (keymap-global-unset "C-z")
  (keymap-global-unset "C-x C-z")
  (keymap-global-unset "C-x f")
  (keymap-global-set "C-x f" priime-fill-map))

(provide 'priime-general)

;;; priime-general.el ends here