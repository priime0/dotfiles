;;; priime-edit.el --- Edit configuration
;;; Commentary:
;; Provides configuration for edit-related packages, including
;; syntax-checking and formatting.
;;; Code:

(defun meow-mx (arg)
  "Press alt + x."
  (interactive "P")
  (execute-extended-command arg))

(defun meow-mq ()
  "Reformat/reindent."
  (interactive)
  (call-interactively (global-key-binding "\M-q")))

(defun meow-yank-above ()
  "Yank the killed text to the line above."
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (meow-yank)))

(defun meow-change-line ()
  "Kill rest of line and switch to insert state."
  (interactive)
  (kill-line)
  (meow-insert))

;; (flycheck-define-checker racket-review
;;   "check racket source code using racket review"
;;   :command ("raco" "review" source)
;;   :error-patterns
;;   ((error line-start (file-name) ":" line ":" column ":error:" (message) line-end)
;;    (warning line-start (file-name) ":" line ":" column ":warning:" (message) line-end))
;;   :modes racket-mode)

;;; Packages

(use-package meow :straight t
  :bind (("C-q" . meow-quit))
  :custom
  (meow-cheatsheet-layout meow-cheatsheet-layout-dvp)
  :init
  (meow-global-mode)
  (meow-setup-indicator)
  (meow-leader-define-key '("u" . "C-u"))
  (meow-leader-define-key '("l" . "s-l"))
  (meow-motion-overwrite-define-key
   ;; custom keybinding for motion state
   '("<escape>" . ignore))
  (meow-normal-define-key
   '("?" . meow-cheatsheet)
   '("*" . meow-expand-0)
   '("=" . meow-expand-9)
   '("!" . meow-expand-8)
   '("[" . meow-expand-7)
   '("]" . meow-expand-6)
   '("{" . backward-paragraph)
   '("+" . meow-expand-4)
   '("}" . forward-paragraph)
   '(")" . meow-expand-2)
   '("(" . meow-expand-1)
   '("1" . digit-argument)
   '("2" . digit-argument)
   '("3" . digit-argument)
   '("4" . digit-argument)
   '("5" . digit-argument)
   '("6" . digit-argument)
   '("7" . digit-argument)
   '("8" . digit-argument)
   '("9" . digit-argument)
   '("0" . digit-argument)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-line)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-line)
   '("E" . meow-goto-line)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . recenter-top-bottom)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-mq)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-search)
   '("t" . meow-right)
   '("T" . meow-right-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-save)
   '("X" . meow-sync-grab)
   '("y" . meow-yank)
   '("Y" . meow-yank-above)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '(":" . meow-mx)
   '("<escape>" . ignore)))
(use-package flycheck :straight t
  :custom
  (flycheck-check-syntax-automatically '(save mode-enable))
  :hook (after-init . global-flycheck-mode))
(use-package paredit :straight t
  :bind (("M-<backspace>" . #'backward-kill-sexp)
         ("M-k" . #'kill-sexp))
  :hook ((emacs-lisp-mode scheme-mode clojure-mode lisp-mode)
         . paredit-mode))
(use-package undo-tree :straight t
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))
(use-package format-all :straight t
  :bind ("C-c f" . format-all-region-or-buffer))
(use-package ws-butler :straight t
  :hook (prog-mode . ws-butler-mode))

(provide 'priime-edit)

;;; priime-edit.el ends here
