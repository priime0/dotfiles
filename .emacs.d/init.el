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
(straight-use-package 's)
(straight-use-package 'f)
(straight-use-package 'dash)


;; Completion
(straight-use-package 'company)
(straight-use-package 'company-box)
(straight-use-package 'company-math)
(straight-use-package 'vertico)
(straight-use-package 'marginalia)
(straight-use-package 'orderless)
(straight-use-package 'consult)
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)

;; Syntax checking and editing
(straight-use-package 'meow)
(straight-use-package 'flycheck)
(straight-use-package 'paredit)

;; Convenience
(straight-use-package 'projectile)
(straight-use-package 'bufler)
(straight-use-package 'embark)
(straight-use-package 'embark-consult)
(straight-use-package 'wgrep)

;; UI
(straight-use-package 'magit)
(straight-use-package 'forge)
(straight-use-package 'neotree)
(straight-use-package 'all-the-icons)
(straight-use-package 'git-gutter)
(straight-use-package 'hl-todo)
(straight-use-package '(nano-theme :type git :host github
                                   :repo "rougier/nano-theme"))

;; Org
(straight-use-package 'org)
(straight-use-package 'org-roam)
(straight-use-package 'org-modern)
(straight-use-package 'org-recur)
(straight-use-package '(org-modern-indent
                        :type git
                        :host github
                        :repo "jdtsmith/org-modern-indent"))
(straight-use-package '(ob-racket
                        :type git
                        :host github
                        :repo "hasu/emacs-ob-racket"))

;; Productivity
(straight-use-package 'elfeed)
(straight-use-package '(pdf-tools :type git :host github
                                  :repo "vedang/pdf-tools"))
(straight-use-package 'olivetti)
(straight-use-package 'rg)
(straight-use-package 'anzu)
(straight-use-package 'vterm)
(straight-use-package 'hledger-mode)

;; LSP
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'eglot)
(straight-use-package 'eldoc-box)
(straight-use-package 'lsp-grammarly)

;; Programming
(straight-use-package '(copilot :type git :host github
                                :repo "zerolfx/copilot.el"
                                :branch "main"
                                :files ("dist" "*.el")))

;; Languages
(straight-use-package 'racket-mode)
(straight-use-package 'rust-mode)
(straight-use-package 'rustic)

(straight-use-package 'markdown-mode)
(straight-use-package 'rjsx-mode)
(straight-use-package 'just-mode)
(straight-use-package 'justl)
(straight-use-package 'scribble-mode)
(straight-use-package 'go-mode)
(straight-use-package 'auctex)
(straight-use-package 'cdlatex)
(straight-use-package 'yaml-mode)
(straight-use-package 'poetry)
(straight-use-package 'slime)
(straight-use-package 'slime-company)
(straight-use-package 'haskell-mode)
(straight-use-package 'lsp-haskell)
(straight-use-package 'eglot-java)
(straight-use-package 'company-coq)
(straight-use-package 'proof-general)
(straight-use-package '(llvm-mode :type git :host github
                                  :repo "nverno/llvm-mode"))
(straight-use-package '(pollen-mode :type git :host github
                                    :repo "basus/pollen-mode"))
(straight-use-package 'clojure-mode)

(straight-use-package 'tuareg)
(straight-use-package 'utop)
(straight-use-package 'dune)
(straight-use-package 'flycheck-ocaml)
(straight-use-package 'merlin-eldoc)
(straight-use-package 'ocamlformat)
(straight-use-package 'elixir-mode)


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
