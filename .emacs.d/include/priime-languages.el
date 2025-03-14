;;; priime-languages.el --- Programming language package configuration
;;; Commentary:
;; Provides configuration for programming language packages.
;;; Code:

(defun justl--current-recipes ()
  "Retrieve the current recipe names from the current directory."
  (let* ((justfile (justl--find-justfile default-directory))
         (raw-entries (justl--get-recipes justfile))
         (entry-names (mapcar #'justl--recipe-name raw-entries)))
    entry-names))

(defun justl-recipes ()
  "Pick and execute a just recipe."
  (interactive)
  (let* ((entry-names (justl--current-recipes))
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
(use-package rust-mode :straight t)
(use-package rustic :straight t
  :custom (rustic-lsp-client 'eglot))
(use-package rjsx-mode :straight t)
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
  (tuareg-match-clause-indent 0)
  :hook ((tuareg-mode . flycheck-ocaml-setup)))
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
(use-package gleam-ts-mode :straight t
  :mode (rx ".gleam" eos))

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
  (markdown-header-face-6 ((t (:inherit nano-strong :height 1.05 :family "Inter"))))
  (markdown-italic-face ((t (:inherit nano-default :slant italic)))))
(use-package just-mode :straight t)
(use-package justl :straight t
  :bind (("C-c j" . #'justl-recipes)))
(use-package scribble-mode :straight '(scribble-mode :type git :host github :repo "priime0/scribble-mode"))
(use-package auctex :straight t)
(use-package cdlatex :straight t)
(use-package yaml-mode :straight t)

;; From https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config
(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.typ\\'" . typst-ts-mode))
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.23.2"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.23.2"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1" "src"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
               (typst "https://github.com/uben0/tree-sitter-typst")))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
  (dolist (mapping
           '((css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))

(use-package typst-ts-mode
  :straight '(:type git :host sourcehut :repo "meow_king/typst-ts-mode" :files (:defaults "*.el"))
  :custom
  (typst-ts-mode-watch-options "--open")
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-highlight-raw-blocks-at-startup t))


(provide 'priime-languages)

;;; priime-languages.el ends here
