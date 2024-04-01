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

;; Elisp
(use-package s    :straight t)
(use-package f    :straight t)
(use-package dash :straight t)


;; Completion
(use-package company            :straight t)
(use-package company-box        :straight t)
(use-package company-math       :straight t)
(use-package vertico            :straight t)
(use-package marginalia         :straight t)
(use-package orderless          :straight t)
(use-package consult            :straight t)
(use-package yasnippet          :straight t)
(use-package yasnippet-snippets :straight t)

;; Syntax checking and editing
(use-package meow      :straight t)
(use-package flycheck  :straight t)
(use-package paredit   :straight t)

;; Convenience
(use-package projectile      :straight t)
(use-package bufler          :straight t)
(use-package embark          :straight t)
(use-package embark-consult  :straight t)
(use-package wgrep           :straight t)

;; UI
(use-package magit          :straight t)
(use-package forge          :straight t)
(use-package neotree        :straight t)
(use-package all-the-icons  :straight t)
(use-package git-gutter     :straight t)
(use-package hl-todo        :straight t)
(use-package nano-theme
  :straight '(nano-theme :type git :host github
                         :repo "rougier/nano-theme"))

;; Org
(use-package org        :straight t)
(use-package org-roam   :straight t)
(use-package org-modern :straight t)
(use-package org-recur  :straight t)
(use-package org-modern-indent
  :straight
  '(org-modern-indent
    :type git
    :host github
    :repo "jdtsmith/org-modern-indent"))
(use-package ob-racket
  :straight
  '(ob-racket
    :type git
    :host github
    :repo "hasu/emacs-ob-racket"))

;; Productivity
(use-package elfeed :straight t)
(use-package pdf-tools
  :straight
  '(pdf-tools :type git :host github
              :repo "vedang/pdf-tools"))
(use-package olivetti     :straight t)
(use-package rg           :straight t)
(use-package anzu         :straight t)
(use-package vterm        :straight t)
(use-package hledger-mode :straight t)

;; LSP
(use-package lsp-mode      :straight t)
(use-package lsp-ui        :straight t)
(use-package eglot         :straight t)
(use-package eldoc         :straight t)
(use-package eldoc-box     :straight t)
(use-package lsp-grammarly :straight t)

;; Programming
(use-package copilot
  :straight
  '(copilot :type git :host github
            :repo "zerolfx/copilot.el"
            :branch "main"
            :files ("dist" "*.el")))

;; Languages
(use-package racket-mode :straight t)
(use-package rust-mode :straight t)
(use-package rustic :straight t)

(use-package markdown-mode :straight t)
(use-package rjsx-mode :straight t)
(use-package just-mode :straight t)
(use-package justl :straight t)
(use-package scribble-mode :straight t)
(use-package go-mode :straight t)
(use-package auctex :straight t)
(use-package cdlatex :straight t)
(use-package yaml-mode :straight t)
(use-package poetry :straight t)
(use-package sly :straight t)
(use-package haskell-mode :straight t)
(use-package lsp-haskell :straight t)
(use-package eglot-java :straight t)
(use-package company-coq :straight t)
(use-package llvm-mode
  :straight
  '(llvm-mode :type git :host github
              :repo "nverno/llvm-mode"))
(use-package pollen-mode
  :straight
  '(pollen-mode :type git :host github
                :repo "basus/pollen-mode"))
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


;; ====== Custom =============================
(add-to-list 'load-path "~/.emacs.d/include")
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(load-library "general")
(load-library "misc")
(load-library "utils")
(load-library "orgconfig")
(load-library "mu4econfig")
(load-library "racket")
(load-library "latexconfig")
(load-library "cppconfig")
(load-library "pdfconfig")
(load-library "gpgconfig")
(load-library "rustconfig")
(load-library "ocamlconfig")

;; ====== Hooks ==============================
(add-hook 'after-init-hook      'global-company-mode)
(add-hook 'after-init-hook      'marginalia-mode)
(add-hook 'after-init-hook      'global-flycheck-mode)
(add-hook 'after-init-hook      'vertico-mode)
(add-hook 'after-init-hook      'global-anzu-mode)

(add-hook 'company-mode-hook    'company-box-mode)

(add-hook 'prog-mode-hook       #'git-gutter-mode)
(add-hook 'prog-mode-hook       #'hl-todo-mode)
(add-hook 'prog-mode-hook       #'display-line-numbers-mode)

(add-hook 'racket-mode-hook     #'lsp)
(add-hook 'rustic-mode-hook     #'lsp)
(add-hook 'python-mode-hook     #'lsp)
(add-hook 'go-mode-hook         #'lsp)
(add-hook 'tuareg-mode-hook     #'lsp)
(add-hook 'haskell-mode-hook    #'lsp)
(add-hook 'rjsx-mode-hook       #'lsp)
(add-hook 'java-mode-hook       #'eglot-java-mode)

(add-hook 'emacs-lisp-mode-hook  #'paredit-mode)
(add-hook 'scheme-mode-hook      #'paredit-mode)
(add-hook 'racket-mode-hook      #'paredit-mode)
(add-hook 'clojure-mode-hook     #'paredit-mode)
(add-hook 'lisp-mode-hook        #'paredit-mode)

(add-hook 'lsp-mode-hook           #'lsp-ui-mode)
(add-hook 'lsp-mode-hook           #'lsp-inlay-hints-mode)
(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode)

(add-hook 'racket-mode-hook     'racket-xp-mode)
