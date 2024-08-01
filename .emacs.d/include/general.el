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
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))
(setq backup-by-copying t)
(setq create-lockfiles nil)

;; UI
(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(setq truncate-lines          t)

;; Bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Scratch
(setq initial-scratch-message nil)

;; Line
(hl-line-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(setq display-line-numbers 'relative)

;; Tabs
(setq-default tab-width 2)

;; Cursor
(blink-cursor-mode 1)

;; Theme & Font
(load-theme 'nano t)
(nano-light)

(defvar priime-fixed-font nil)
(defvar priime-variable-font nil)
(defvar priime-font-size nil)
(defvar priime-fixed-height nil)

(cond ((eq system-type 'gnu/linux)
       (setf priime-fixed-font "JetBrains Mono SemiBold")
       (setf priime-variable-font "Newsreader")
       (setf priime-font-size 10)
       (setf priime-fixed-height 0.8))
      ((eq system-type 'darwin)
       (setf priime-fixed-font "Menlo")
       (setf priime-variable-font "Verdana")
       (setf priime-font-size 12)
       (setf priime-fixed-height 1))
      (t
       (setf priime-fixed-font "Roboto Mono")
       (setf priime-variable-font "Roboto")
       (setf priime-font-size 10)
       (setf priime-fixed-height 0.8)))

(add-to-list 'default-frame-alist `(font . ,(format "%s-10" priime-fixed-font)))
(set-frame-font (format "%s %d" priime-fixed-font priime-font-size))

(custom-set-faces
 '(region         ((t (:inherit nano-subtle :background "#EBE5F5"))))
 '(lazy-highlight ((t (:inherit region))))
 `(variable-pitch ((t (:family ,priime-variable-font :height 125 :weight medium))))
 `(fixed-pitch    ((t (:family ,priime-fixed-font :height ,priime-fixed-height :inherit nil))))
 `(shr-text       ((t (:inherit variable-pitch-text :family ,priime-variable-font)))))
(set-face-attribute 'italic nil
                    :family 'inherit
                    :slant 'italic
                    :weight 'medium
                    :foreground nano-light-foreground)

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

;; Whitespace
(setq-default show-trailing-whitespace t)

;; Sentences
;; Emacs thinks that sentences ends with period and two spaces,
;; disable this.
(setq sentence-end-double-space nil)

;; Scrolling
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(setq scroll-conservatively 101)

;; Keybindings
(keymap-global-set "C-M-j" (lambda () (interactive) (scroll-up 1)))
(keymap-global-set "C-M-k" (lambda () (interactive) (scroll-down 1)))
(keymap-global-set "C-x <"
                   (lambda (arg)
                     (interactive "P")
                     (let ((amt (or arg 1)))
                       (scroll-right amt))))
(keymap-global-set "C-x >"
                   (lambda (arg)
                     (interactive "P")
                     (let ((amt (or arg 1)))
                       (scroll-left amt))))
(keymap-global-unset "C-x f")
(keymap-global-set "C-x f f" #'set-fill-column)
(keymap-global-set "C-x f i" #'display-fill-column-indicator-mode)
(keymap-global-set "C-x f a" #'auto-fill-mode)
(keymap-global-set "C-x f n" #'toggle-display-line-numbers)
(keymap-global-set "C-v"     #'View-scroll-half-page-forward)
(keymap-global-set "M-v"     #'View-scroll-half-page-backward)

(defun toggle-display-line-numbers ()
  "Toggle the display of line numbers."
  (interactive)
  (cond ((equal display-line-numbers t)
         (setq display-line-numbers 'relative))
        ((equal display-line-numbers 'relative)
         (setq display-line-numbers t))
        (t
         (progn
           (display-line-numbers-mode 1)
           (setq display-line-numbers t)))))

;; Garbage Collection
(setq gc-cons-threshold 50000000)

;; Confirmations
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable suspend state
(keymap-global-unset "C-z")
(keymap-global-unset "C-x C-z")

;; emacs-nativecomp
(setq native-comp-async-report-warnings-errors nil)

;; From https://stackoverflow.com/questions/8309769/how-can-i-prevent-emacs-from-opening-new-window-for-compilation-output

;; Helper for compilation. Close the compilation window if
;; there was no error at all. (emacs wiki)
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))
;; Specify my function (maybe I should have done a lambda function)
(setq compilation-exit-message-function 'compilation-exit-autoclose)

;; shell
(setq vterm-shell (or (executable-find "fish") shell-file-name))
(keymap-global-set "C-c v" #'vterm)

;; epa/gpg
(setq epa-file-encrypt-to '("lucas@priime.dev"))
(setq epa-file-select-keys 1)

(provide 'general)
;;; general.el ends here
