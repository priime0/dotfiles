;;; misc.el --- Miscellaneous configuration for packages.
;;; Commentary:
;;    Miscellaneous configuration for packages.  If a package doesn't
;;    necessarily require its own configuration file (its configuration could be
;;    short), then it belongs here.
;;; Code:

(require 'git-gutter)
(require 'elfeed)
(require 'neotree)

(defun config-recompile ()
  "Recompile the current Emacs configuration."
  (interactive)
  (byte-recompile-directory "~/.emacs.d/"))

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
(setq auth-sources '("~/.authinfo"))

;; Web configuration
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\")))
(setq web-mode-markup-indent-offset 2)

;; Rust LSP configuration
(setq lsp-rust-analyzer-cargo-watch-enable t)
(setq lsp-rust-analyzer-cargo-watch-command "clippy")
(setq lsp-rust-analyzer-server-display-inlay-hints t)
(setq lsp-rust-analyzer-inlay-hints-mode t)

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

(provide 'misc)
;;; misc.el ends here
