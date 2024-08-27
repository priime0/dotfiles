;;; init.el -- Base configuration
;;; Commentary:
;;    Base configuration.
;;; Code:

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
(use-package s :straight t)
(use-package f :straight t)
(use-package dash :straight t)


;; Completion
(use-package corfu :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (corfu-auto t)
  (corfu-popupinfo-delay 0)
  :bind (:map corfu-map
              ("C-n"   . corfu-next)
              ("C-p"   . corfu-previous)
              ("<tab>" . corfu-insert)
              ("RET"   . nil))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))
(use-package vertico            :straight t
  :init
  (add-hook 'after-init-hook 'vertico-mode))
(use-package marginalia         :straight t
  :init
  (add-hook 'after-init-hook 'marginalia-mode))
(use-package orderless          :straight t)
(use-package consult            :straight t)
(use-package yasnippet          :straight t)
(use-package yasnippet-snippets :straight t)

;; Syntax checking and editing, and formatting
(use-package meow      :straight t)
(use-package flycheck  :straight t
  :custom
  (flycheck-check-syntax-automatically '(save mode-enable))
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode))
(use-package paredit   :straight t
  :hook ((emacs-lisp-mode scheme-mode racket-mode clojure-mode lisp-mode)
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

;; Convenience
(use-package projectile :straight t
  :custom
  (projectile-completion-system 'auto))
(use-package bufler :straight t
  :bind (("C-x C-b" . bufler-list)
         ("C-x b" . bufler-switch-buffer))
  :init
  (bufler-mode 1))
(use-package embark :straight t
  :bind (("C-." . embark-act)))
(use-package embark-consult   :straight t)
(use-package wgrep            :straight t)
(use-package perspective      :straight t
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode))
(use-package persp-projectile :straight t
  :after (perspective projectile))
(use-package no-littering :straight t)

;; Version control
(use-package magit :straight t
  :bind (("<f5>" . magit-status)
         ("C-x g" . magit-status)))
(use-package forge :straight t
  :after (magit))

;; UI
(use-package neotree :straight t
  :custom
  ((neo-theme 'icons)
   (neo-smart-open t)
   (neo-window-fixed-size nil)
   (neo-show-hidden-files t))
  :bind (("C-c t" . #'neotree-toggle-current-directory)))
(use-package all-the-icons :straight t)
(use-package git-gutter :straight t
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:update-interval 1)
  :config
  (git-gutter:start-update-timer))
(use-package hl-todo        :straight t
  :hook (prog-mode . hl-todo-mode))
(use-package nano-theme
  :straight '(nano-theme :type git :host github
                         :repo "rougier/nano-theme"))

;; Org
(use-package org        :straight t)
(use-package org-roam   :straight t
  :after (org))
(use-package org-modern :straight t
  :after (org)
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star 'fold))
(use-package org-modern-indent
  :straight
  '(org-modern-indent
    :type git
    :host github
    :repo "jdtsmith/org-modern-indent")
  :after (org-modern)
  :hook (org-mode . org-modern-indent-mode))
(use-package ob-racket
  :straight
  '(ob-racket
    :type git
    :host github
    :repo "hasu/emacs-ob-racket"))

;; Productivity
(use-package pdf-tools
  :straight
  '(pdf-tools :type git :host github
              :repo "vedang/pdf-tools"))
(use-package olivetti     :straight t
  :hook (org-mode . olivetti-mode))
(use-package rg           :straight t)
(use-package anzu         :straight t
  :init
  (add-hook 'after-init-hook 'global-anzu-mode))
(use-package vterm        :straight t)
(use-package hledger-mode :straight t)

;; LSP
(use-package lsp-mode      :straight t)
(use-package lsp-ui        :straight t
  :after (lsp-mode)
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-mode . lsp-inlay-hints-mode)))

;; Languages
(use-package racket-mode :straight t
  :hook (racket-mode . racket-xp-mode))
(use-package rust-mode :straight t)
(use-package rustic :straight t)

(use-package markdown-mode :straight t
  :custom-face
  (markdown-inline-code-face ((t (:inherit nano-salient)))))
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


;; ====== Custom =============================
(add-to-list 'load-path "~/.emacs.d/include")
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(load-library "general")
(load-library "misc")
(load-library "utils")
(load-library "orgconfig")
(when (eq system-type 'gnu/linux)
  (load-library "mu4econfig"))
(load-library "racket")
(load-library "latexconfig")
(load-library "cppconfig")
(load-library "pdfconfig")
(load-library "gpgconfig")
(load-library "rustconfig")
(load-library "ocamlconfig")

;; ====== Hooks ==============================
(add-hook 'prog-mode-hook       #'display-line-numbers-mode)

(add-hook 'racket-mode-hook     #'lsp)
(add-hook 'rustic-mode-hook     #'lsp)
(add-hook 'python-mode-hook     #'lsp)
(add-hook 'go-mode-hook         #'lsp)
(add-hook 'tuareg-mode-hook     #'lsp)
(add-hook 'haskell-mode-hook    #'lsp)
(add-hook 'rjsx-mode-hook       #'lsp)
