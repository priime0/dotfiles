;;; priime-languages.el --- Programming language package configuration
;;; Commentary:
;; Provides configuration for programming language packages.
;;; Code:

(use-package racket-mode :straight t
  :hook
  ((racket-mode . racket-xp-mode)
   (racket-mode . paredit-mode)))
(use-package rust-mode :straight t
  :hook (rust-mode . eglot))
(use-package rustic :straight t)

(use-package markdown-mode :straight t
  :custom-face
  (markdown-inline-code-face ((t (:inherit nano-salient)))))
(use-package rjsx-mode :straight t)
(use-package just-mode :straight t)
(use-package justl :straight t
  :bind (("C-c j" . #'justl-recipes)))
(use-package scribble-mode :straight t)
(use-package auctex :straight t)
(use-package cdlatex :straight t)
(use-package yaml-mode :straight t)
(use-package poetry :straight t)
(use-package sly :straight t)
(use-package haskell-mode :straight t)
(use-package llvm-mode
  :straight
  '(llvm-mode :type git :host github
              :repo "nverno/llvm-mode"))
(use-package clojure-mode :straight t)
(use-package tuareg :straight t)
(use-package utop :straight t)
(use-package dune :straight t)
(use-package flycheck-ocaml :straight t)
(use-package merlin-eldoc :straight t)
(use-package ocamlformat :straight t)
(use-package elixir-mode :straight t)
(use-package inf-elixir :straight t)
(use-package nix-mode :straight t)
(use-package irony :straight t
  :hook ((c++-mode c-mode) . irony-mode))

(provide 'priime-languages)

;;; priime-languages.el ends here
