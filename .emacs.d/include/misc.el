;;; misc.el --- Miscellaneous configuration for packages.
;;; Commentary:
;;    Miscellaneous configuration for packages.  If a package doesn't
;;    necessarily require its own configuration file (its configuration could be
;;    short), then it belongs here.
;;; Code:

(require 'lsp-mode)
(require 'git-gutter)
(require 'elfeed)
(require 'neotree)
(require 'copilot)

(defun config-compile ()
  "(Re)compile the current Emacs configuration."
  (interactive)
  (byte-recompile-directory "~/.emacs.d/"))

(defun config-download (&optional confirm)
  "Download the latest Emacs config files from the GitHub repository if CONFIRM."
  (interactive)
  (let* ((confirm (or confirm
                      (read-string "Type `YES' to confirm download: "))))
    (if (not (string= confirm "YES"))
        (print "Cancelling config download/update...")
      (let* ((gh-url "https://raw.githubusercontent.com/priime0/dotfiles/master/.emacs.d/")
             (target-dir "~/.emacs.d/")
             (filenames
              '("init.el"
                "include/general.el"
                "include/misc.el"
                "include/utils.el"
                "include/racket.el"
                "include/pdfconfig.el"
                "include/cppconfig.el"
                "include/latexconfig.el"
                "include/mu4econfig.el"
                "include/orgconfig.el"))
             (file-paths (mapcar (lambda (s) (concat target-dir s))
                                 filenames))
             (file-urls (mapcar (lambda (s) (concat gh-url s))
                                filenames))
             (file-path-urls (lists->alist file-paths file-urls)))
        (mkdir (concat target-dir "include") t)
        (mapc (lambda (f)
                (url-copy-file (cdr f) (car f) t))
              file-path-urls)))))


(defun download-file (&optional url filepath)
  "Download the file from URL to FILEPATH."
  (interactive)
  (let* ((url (or url
                  (read-string "url: ")))
         (filepath (or filepath (read-file-name "filename: "))))
    (url-copy-file url filepath 1)))

;; LSP
(setq lsp-inlay-hint-enable t)
(setq lsp-ui-doc-include-signature t)
(setq lsp-ui-doc-header t)
(setq lsp-ui-doc-border (face-foreground 'default))
(setq lsp-ui-sideline-show-code-actions t)
(setq lsp-enable-file-watchers nil)
(setq lsp-log-max nil)
(setq lsp-enable-links nil)
(setq lsp-use-plists t)

(custom-set-faces
 '(lsp-ui-doc-header ((t (:inherit nano-strong :background-color nano-dark-background)))))

(setq read-process-output-max (* 4 1024 1024))

;; Eglot
(with-eval-after-load 'eglot-java
  (define-key eglot-java-mode-map (kbd "C-c l n") #'eglot-java-file-new)
  (define-key eglot-java-mode-map (kbd "C-c l x") #'eglot-java-run-main)
  (define-key eglot-java-mode-map (kbd "C-c l t") #'eglot-java-run-test)
  (define-key eglot-java-mode-map (kbd "C-c l N") #'eglot-java-project-new)
  (define-key eglot-java-mode-map (kbd "C-c l T") #'eglot-java-project-build-task)
  (define-key eglot-java-mode-map (kbd "C-c l R") #'eglot-java-project-build-refresh)
  (define-key eglot-java-mode-map (kbd "C-c l a") #'eglot-code-actions)
  (setq c-basic-offset 2))

;; Git Gutter
(global-git-gutter-mode +1)

;; Elfeed
(setq elfeed-feeds
      '(("https://edwardwibowo.com/rss.xml"     blog)
        ("https://priime.dev/feed.xml"          blog)
        ("https://fasterthanli.me/index.xml"    blog)
        ("https://blog.cleancoder.com/atom.xml" blog)))

;; Magit
(keymap-global-set "C-c g g" #'magit)
(keymap-global-set "C-c g b" #'magit-blame)
(setq auth-sources '("~/.authinfo"))

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Neotree
(defun neotree-toggle-current-directory ()
  "Toggle neotree at the current directory."
  (interactive)
  (let ((current-directory
         (if buffer-file-name
             (file-name-directory buffer-file-name)
           "~")))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (neotree-dir current-directory))))

(keymap-global-set "C-c t" #'neotree-toggle-current-directory)
(setq neo-theme 'icons)

;; Orderless
(setq completion-styles '(orderless basic))
(setq completion-category-overrides '((file (styles basic partial-completion))))

;; bufler
(bufler-mode 1)
(keymap-global-set "C-x b" #'bufler-switch-buffer)
(keymap-global-set "C-x C-b" #'bufler-list)


;; hledger
(setq hledger-currency-string "$")
(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))
(setq hledger-jfile "~/finance/2024.journal")


;; SLIME
(setq inferior-lisp-program "sbcl")

;; Copilot
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)

;; Markdown
(custom-set-faces
 '(markdown-header-face-1 ((t (:inherit nano-strong :height 1.3  :family "Inter"))))
 '(markdown-header-face-2 ((t (:inherit nano-strong :height 1.25 :family "Inter"))))
 '(markdown-header-face-3 ((t (:inherit nano-strong :height 1.2  :family "Inter"))))
 '(markdown-header-face-4 ((t (:inherit nano-strong :height 1.15 :family "Inter"))))
 '(markdown-header-face-5 ((t (:inherit nano-strong :height 1.1  :family "Inter"))))
 '(markdown-header-face-6 ((t (:inherit nano-strong :height 1.05 :family "Inter")))))
(add-hook 'markdown-mode-hook #'markdown-toggle-fontify-code-blocks-natively)
(add-hook 'markdown-mode-hook #'olivetti-mode)

(provide 'misc)
;;; misc.el ends here
