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

;; Completion
(straight-use-package 'company)
(straight-use-package 'company-box)
(straight-use-package 'company-math)
(straight-use-package 'vertico)
(straight-use-package 'marginalia)

;; Syntax checking and editing
(straight-use-package 'flycheck)
(straight-use-package 'paredit)

;; Convenience
(straight-use-package 'projectile)
(straight-use-package 'workgroups2)

;; UI
(straight-use-package 'which-key)
(straight-use-package 'magit)
(straight-use-package 'forge)
(straight-use-package 'neotree)
(straight-use-package 'all-the-icons)
(straight-use-package 'git-gutter)
(straight-use-package '(nano-theme :type git :host github
                                   :repo "rougier/nano-theme"))

;; Productivity
(straight-use-package 'org)
(straight-use-package 'elfeed)
(straight-use-package '(pdf-tools :type git :host github
                                  :repo "vedang/pdf-tools"))

;; LSP
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)

;; Languages
(straight-use-package 'racket-mode)
(straight-use-package 'rust-mode)
(straight-use-package 'rustic)
(straight-use-package 'tuareg)
(straight-use-package 'markdown-mode)
(straight-use-package 'web-mode)
(straight-use-package 'just-mode)
(straight-use-package 'justl)
(straight-use-package 'scribble-mode)
(straight-use-package 'go-mode)
(straight-use-package 'auctex)
(straight-use-package 'yaml-mode)


;; ====== Custom =============================
(add-to-list 'load-path "~/.emacs.d/include")
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(load-library "general")
(load-library "misc")
(load-library "orgconfig")
(load-library "mu4econfig")
(load-library "racket")
(load-library "latexconfig")
(load-library "cppconfig")
(load-library "pdfconfig")

;; ====== Hooks ==============================
(add-hook 'after-init-hook      'global-company-mode)
(add-hook 'after-init-hook      'marginalia-mode)
(add-hook 'after-init-hook      'which-key-mode)
(add-hook 'after-init-hook      'global-flycheck-mode)
(add-hook 'after-init-hook      'workgroups-mode)
(add-hook 'after-init-hook      'vertico-mode)

(add-hook 'company-mode-hook    'company-box-mode)

(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'scheme-mode-hook     #'paredit-mode)
(add-hook 'racket-mode-hook     #'paredit-mode)
(add-hook 'clojure-mode-hook    #'paredit-mode)

(add-hook 'lsp-mode             'lsp-ui-mode)

(add-hook 'python-mode          'lsp-mode)
(add-hook 'rust-mode            'lsp-rust)

(add-hook 'racket-mode-hook     'racket-xp-mode)
