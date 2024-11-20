;;; priime-languages.el --- Programming language package configuration
;;; Commentary:
;; Provides configuration for programming language packages.
;;; Code:

(defun justl-recipes ()
  "Pick and execute a just recipe."
  (interactive)
  (let* ((justfile (justl--find-justfile default-directory))
         (raw-entries (justl--get-recipes justfile))
         (entry-names (mapcar #'justl--recipe-name raw-entries))
         (just-recipe (completing-read "just recipe: " entry-names nil t nil)))
    (justl--exec
     justl-executable
     just-recipe
     (append (transient-args 'justl-help-popup)
             (list just-recipe)))))

(defun racket-repl-switch ()
  "Switch to the Racket REPL."
  (interactive)
  (racket-edit-switch-to-repl)
  (unless (equal current-prefix-arg nil)
    (delete-other-windows)))

(defun racket-edit-switch ()
  "Switch to the corresponding racket buffer."
  (interactive)
  (racket-repl-switch-to-edit)
  (unless (equal current-prefix-arg nil)
    (delete-other-windows)))

(defun custom-compile-c++ ()
  "Run the `just' command -- the command I use to compile my environment."
  (interactive)
  (save-buffer)
  (compile (concat "just build " (buffer-name))))

;;; Languages

(use-package racket-mode :straight t
  :bind (:map racket-mode-map
         ("C-c C-z" . racket-repl-switch)
         :map racket-repl-mode-map
         ("C-c C-k" . racket-repl-clear-leaving-last-prompt)
         ("C-c C-z" . racket-edit-switch))
  :hook
  ((racket-mode . racket-xp-mode)
   (racket-mode . paredit-mode))
  :init
  (put 'struct/contract 'racket-indent-function 1))
(use-package pollen-mode :straight t
  :init
  (setq auto-mode-alist
        (append '(("\\.pp$" . racket-mode)
                  ("\\.pm$" . pollen-mode)
                  ("\\.pmd$" . pollen-mode))
                auto-mode-alist)))
(use-package rust-mode :straight t
  :hook (rust-mode . eglot))
(use-package rustic :straight t)

(use-package markdown-mode :straight t
  :hook ((markdown-mode . markdown-toggle-fontify-code-blocks-natively)
         (markdown-mode . olivetti-mode))
  :custom-face
  (markdown-inline-code-face ((t (:inherit nano-salient))))
  (markdown-header-face-1 ((t (:inherit nano-strong :height 1.3  :family "Inter"))))
  (markdown-header-face-2 ((t (:inherit nano-strong :height 1.25 :family "Inter"))))
  (markdown-header-face-3 ((t (:inherit nano-strong :height 1.2  :family "Inter"))))
  (markdown-header-face-4 ((t (:inherit nano-strong :height 1.15 :family "Inter"))))
  (markdown-header-face-5 ((t (:inherit nano-strong :height 1.1  :family "Inter"))))
  (markdown-header-face-6 ((t (:inherit nano-strong :height 1.05 :family "Inter")))))
(use-package rjsx-mode :straight t)
(use-package just-mode :straight t)
(use-package justl :straight t
  :bind (("C-c j" . #'justl-recipes)))
(use-package scribble-mode :straight t)
(use-package auctex :straight t)
(use-package cdlatex :straight t)
(use-package yaml-mode :straight t)
(use-package poetry :straight t)
(use-package sly :straight t
  :custom (inferior-lisp-program "sbcl"))
(use-package haskell-mode :straight t)
(use-package llvm-mode
  :straight
  '(llvm-mode :type git :host github
              :repo "nverno/llvm-mode"))
(use-package clojure-mode :straight t)
(use-package tuareg :straight t
  :custom
  (utop-command "opam exec -- dune utop . -- -emacs")
  :hook ((tuareg-mode . flycheck-ocaml-setup)
         (tuareg-mode . merlin-mode)))
(use-package utop :straight t)
(use-package dune :straight t)
(use-package flycheck-ocaml :straight t)
(use-package merlin-eldoc :straight t)
(use-package ocamlformat :straight t)
(use-package elixir-mode :straight t)
(use-package inf-elixir :straight t)
(use-package nix-mode :straight t)
(use-package irony :straight t
  :bind (:map irony-mode-map ("C-c C-c" . custom-compile-c++))
  :hook ((c++-mode c-mode) . irony-mode))

(provide 'priime-languages)

;;; priime-languages.el ends here
