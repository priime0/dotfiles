;;; priime-general.el --- General package configuration
;;; Commentary:
;; Provides general configuration for Emacs.
;;; Code:

(require 'dash)
(require 'view)

;;; Font
(defvar priime--font-config
  (cond ((string= (system-name) "framework") '("JetBrains Mono Medium" "Roboto" 10 0.9))
        ((eq system-type 'gnu/linux)         '("Roboto Mono Medium" "Roboto" 12 0.8))
        ((eq system-type 'darwin)            '("Menlo" "Verdana" 12 1))
        (t                                   '("Roboto Mono" "Roboto" 10 0.8))))

(defvar priime-fixed-font    (-first-item priime--font-config))
(defvar priime-variable-font (-second-item priime--font-config))
(defvar priime-font-size     (-third-item priime--font-config))
(defvar priime-fixed-height  (-fourth-item priime--font-config))

(defun priime-font-reload ()
  "Reload the default font."
  (interactive)
  (let ((priime-font-height (* priime-font-size 10)))
    (set-face-attribute 'default nil
                        :family priime-fixed-font
                        :height priime-font-height
                        :weight 'medium)))

(defun priime-font+ (amt)
  "Increases the font size by AMT."
  (interactive "p")
  (let ((amt (or amt 1)))
    (setq priime-font-size (+ amt priime-font-size))
    (priime-font-reload)))

(defun priime-font- (amt)
  "Decreases the font size by AMT."
  (interactive "p")
  (let ((amt (or amt 1)))
    (priime-font+ (- amt))))

;;; Keybindings
(defun priime-split-right ()
  "Split the window right and move to it."
  (interactive)
  (split-window-right)
  (windmove-right)
  (balance-windows))

(defun priime-split-down ()
  "Split the window down and move to it."
  (interactive)
  (split-window-below)
  (windmove-down)
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

(defun priime-display-relative ()
  "Display relative line numbers."
  (display-line-numbers-mode 1)
  (setq display-line-numbers 'relative))

(defun priime-display-fixed ()
  "Display fixed line numbers."
  (display-line-numbers-mode 1)
  (setq display-line-numbers t))

(defun priime-toggle-line-numbers ()
  "Toggle the display of line numbers."
  (interactive)
  (cond ((equal display-line-numbers t) (priime-display-relative))
        ((equal display-line-numbers 'relative) (priime-display-fixed))
        (t (priime-display-fixed))))

(defun download-file (&optional url filepath)
  "Download the file from URL to FILEPATH."
  (interactive)
  (let* ((url (or url (read-string "url: ")))
         (filepath (or filepath (read-file-name "filename: "))))
    (url-copy-file url filepath 1)))

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
  (read-process-output-max (* 4 1024 1024))
  (browse-url-handlers '(("\\.pdf\\'" .
                          (lambda (url &rest _)
                            (async-start-process "zathura" "zathura" "zathura" url)))))

  :custom-face
  (default                  ((t (:family ,priime-fixed-font :weight medium))))
  (region                   ((t (:inherit nano-subtle :background "#EBE5F5"))))
  (italic                   ((t (:inherit nano-default :slant italic))))
  (lazy-highlight           ((t (:inherit region))))
  (variable-pitch           ((t (:inherit default :family ,priime-variable-font :height 1.25 :weight regular))))
  (fixed-pitch              ((t (:family ,priime-fixed-font :height ,priime-fixed-height :inherit default))))
  (shr-text                 ((t (:inherit variable-pitch-text :family ,priime-variable-font))))
  (line-number              ((t (:inherit default :foreground "#98A4AE"))))
  (line-number-current-line ((t (:inherit default :foreground "#98A4AE"))))
  ;; show-paren-mode
  (show-paren-match         ((t (:inherit nano-salient :weight black))))

  :bind
  (("C-v" . View-scroll-half-page-forward)
   ("M-v" . View-scroll-half-page-backward)
   ("<f6>" . priime-split-right)
   ("<f7>" . priime-split-down)
   ("C-M-<down>" . priime-scroll-up)
   ("C-M-<up>" . priime-scroll-down))

  :hook
  ((prog-mode . priime-display-relative))

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
  (put 'scroll-left 'disabled nil)
  (put 'scroll-right 'disabled nil)

  (fset 'yes-or-no-p 'y-or-n-p)

  (keymap-global-unset "C-z")
  (keymap-global-unset "C-x C-z")
  (keymap-global-unset "C-x f")
  (keymap-global-set "C-x f" priime-fill-map)

  (keymap-global-set "C-+" #'priime-font+)
  (keymap-global-unset "C--")
  (keymap-global-set "C--" #'priime-font-)

  (priime-font-reload))

(provide 'priime-general)

;;; priime-general.el ends here
