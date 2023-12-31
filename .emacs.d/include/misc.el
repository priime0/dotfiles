;;; misc.el --- Miscellaneous configuration for packages.
;;; Commentary:
;;    Miscellaneous configuration for packages.  If a package doesn't
;;    necessarily require its own configuration file (its configuration could be
;;    short), then it belongs here.
;;; Code:

(require 'git-gutter)
(require 'elfeed)
(require 'neotree)

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

;; LSP
(setq lsp-inlay-hint-enable t)

;; Git Gutter
(global-git-gutter-mode +1)

;; Elfeed
(setq elfeed-feeds
      '(("https://edwardwibowo.com/rss.xml"     blog)
        ("https://priime.dev/feed.xml"          blog)
        ("https://fasterthanli.me/index.xml"    blog)
        ("https://blog.cleancoder.com/atom.xml" blog)))

;; Magit
(global-set-key (kbd "C-c g g") #'magit)
(global-set-key (kbd "C-c g b") #'magit-blame)
(setq auth-sources '("~/.authinfo"))

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

(global-set-key (kbd "C-c t") #'neotree-toggle-current-directory)
(setq neo-theme 'icon)

;; Orderless
(setq completion-styles '(orderless basic))
(setq completion-category-overrides '((file (styles basic partial-completion))))

;; beframe
(beframe-mode 1)
(global-unset-key (kbd "C-x b"))
(global-set-key (kbd "C-x b") #'beframe-switch-buffer)

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
 '(markdown-header-face-1 ((t (:inherit nano-strong :height 1.3 :family "Inter"))))
 '(markdown-header-face-2 ((t (:inherit nano-strong :height 1.2 :family "Inter"))))
 '(markdown-header-face-3 ((t (:inherit nano-strong :height 1.1 :family "Inter"))))
 '(markdown-header-face-4 ((t (:inherit nano-strong :height 1.0 :family "Inter"))))
 '(markdown-header-face-5 ((t (:inherit nano-strong :height 0.9 :family "Inter"))))
 '(markdown-header-face-6 ((t (:inherit nano-strong :height 0.8 :family "Inter")))))
(add-hook 'markdown-mode-hook #'markdown-toggle-fontify-code-blocks-natively)

(provide 'misc)
;;; misc.el ends here
