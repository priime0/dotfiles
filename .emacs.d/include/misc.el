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
  "Download the latest Emacs configuration files from the GitHub repository."
  (interactive)
  (let* ((confirm (or confirm
                      (read-string "Type `YES' to confirm download: "))))
    (if (not (string= confirm "YES"))
        (print "Cancelling config download/update...")
      (let* ((gh-url "https://raw.githubusercontent.com/priime0/dotfiles/master/.emacs.d/")
             (target-dir "~/temp/.emacs.d/")
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
