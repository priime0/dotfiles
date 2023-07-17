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

(straight-use-package 'company)
(straight-use-package 'company-box)
(straight-use-package 'company-math)
(straight-use-package 'magit)
(straight-use-package 'vertico)
(straight-use-package 'marginalia)
(straight-use-package 'racket-mode)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'rust-mode)
(straight-use-package 'rustic)
(straight-use-package 'which-key)
(straight-use-package 'neotree)
(straight-use-package 'flycheck)
(straight-use-package 'git-gutter)
(straight-use-package 'paredit)
(straight-use-package 'tuareg)
(straight-use-package 'projectile)
(straight-use-package 'workgroups2)
(straight-use-package 'markdown-mode)
(straight-use-package 'web-mode)
(straight-use-package 'just-mode)
(straight-use-package 'justl)
(straight-use-package 'org)
(straight-use-package 'org-roam)
(straight-use-package 'elfeed)
(straight-use-package 'scribble-mode)
(straight-use-package 'go-mode)
(straight-use-package '(nano-theme :type git :host github
                                   :repo "rougier/nano-theme"))

(add-to-list 'load-path (expand-file-name "lisp/pollen-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(autoload 'pollen-mode "pollen" "A major mode for the pollen preprocessor." t)

;; ====== Configuration ======================

(setq inhibit-startup-message t
      visible-bell            nil)

;; Backups
(setq make-backup-files nil)
(rassq-delete-all 'auto-save-mode auto-mode-alist)

;; UI
(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)

;; Line
(global-display-line-numbers-mode 1)
(hl-line-mode -1)
(setq line-number-mode t)
(setq column-number-mode t)

;; Tabs
(setq-default tab-width 2)

;; Cursor
(blink-cursor-mode 1)

;; Font
(defvar font-size 10)
(defvar font-family "JetBrains Mono SemiBold")
(set-frame-font (format "%s %d" font-family font-size))

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Directional window moving
(windmove-default-keybindings)

;; Auto file refresh
(global-auto-revert-mode t)

;; Git Gutter
(global-git-gutter-mode +1)

;; Elfeed
(setq elfeed-feeds
      '(("https://edwardwibowo.com/rss.xml"     blog)
        ("https://priime.dev/feed.xml"          blog)
        ("https://fasterthanli.me/index.xml"    blog)
        ("https://blog.cleancoder.com/atom.xml" blog)))

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'nano t)
(nano-light)

;; ====== Org Mode ===========================

(require 'org)
(require 'org-capture)
(setq org-hide-emphasis-markers t)
(setcar (nthcdr 4 org-emphasis-regexp-components) 20)
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
(setq org-agenda-files '("~/org/inbox.org"
                         "~/org/gtd.org"
                         "~/org/tickler.org"))
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/org/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/org/tickler.org" "Tickler")
                               "* %i%? \n %U")))
(setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 3)
                           ("~/org/someday.org" :level . 1)
                           ("~/org/tickler.org" :maxlevel . 2)))
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; Enable Racket in Org-mode Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((racket . t)))

(defmacro η (fnc)
  "Return function that ignores its arguments and invokes FNC."
  `(lambda (&rest _rest)
     (funcall ,fnc)))

;; Auto-save org buffers
(advice-add 'org-deadline       :after (η #'org-save-all-org-buffers))
(advice-add 'org-schedule       :after (η #'org-save-all-org-buffers))
(advice-add 'org-store-log-note :after (η #'org-save-all-org-buffers))
(advice-add 'org-todo           :after (η #'org-save-all-org-buffers))
(advice-add 'org-refile         :after (η #'org-save-all-org-buffers))
(advice-add 'org-sort           :after (η #'org-save-all-org-buffers))

;; ====== mu4e ===============================

(require 'mu4e)

(setq
 mue4e-headers-skip-duplicates  t
 mu4e-view-show-images t
 mu4e-view-show-addresses t
 mu4e-compose-format-flowed nil
 mu4e-date-format "%Y-%m-%d"
 mu4e-headers-date-format "%Y-%m-%d"
 mu4e-change-filenames-when-moving t
 mu4e-attachments-dir "~/Downloads"

 mu4e-maildir       "~/mail"   ;; top-level Maildir
 ;; note that these folders below must start with /
 ;; the paths are relative to maildir root
 mu4e-refile-folder "/Archive"
 mu4e-sent-folder   "/Sent"
 mu4e-drafts-folder "/Drafts"
 mu4e-trash-folder  "/Trash")

;; this setting allows to re-sync and re-index mail
;; by pressing U
(setq mu4e-get-mail-command  "mbsync -a")

(setq
   message-send-mail-function   'smtpmail-send-it
   smtpmail-default-smtp-server "smtp.fastmail.com"
   smtpmail-smtp-server         "smtp.fastmail.com")

(setq user-full-name "Lucas Sta Maria")
(setq user-mail-address "lucas@priime.dev")
(setq smtpmail-smtp-service 587)

(require 'mu4e-dashboard)

;; ====== Keybindings ========================

(global-set-key (kbd "C-c g g") 'magit)
(global-set-key (kbd "C-M-j") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "C-M-k") (lambda () (interactive) (scroll-down 1)))

;; ====== Filetypes ==========================

(setq auto-mode-alist (cons '("\\.pp$" . racket-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pm$" . racket-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\")))
(setq web-mode-markup-indent-offset 2)

(setq lsp-rust-analyzer-server-display-inlay-hints t)

;; ====== Hooks ==============================
(add-hook 'after-init-hook      'global-company-mode)
(add-hook 'after-init-hook      'marginalia-mode)
(add-hook 'after-init-hook      'which-key-mode)
(add-hook 'after-init-hook      'global-flycheck-mode)
(add-hook 'after-init-hook      'workgroups-mode)
(add-hook 'after-init-hook      'vertico-mode)

(add-hook 'company-mode-hook    'company-box-mode)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook     'paredit-mode)
(add-hook 'racket-mode-hook     'paredit-mode)
(add-hook 'clojure-mode-hook    'paredit-mode)

;(add-hook 'racket-mode-hook     'lsp-racket-enable)

(add-hook 'lsp-mode             'lsp-ui-mode)

(add-hook 'python-mode          'lsp-mode)
(add-hook 'rust-mode            'lsp-rust)

(add-hook 'racket-mode-hook     'racket-xp-mode)

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
            (auto-save-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column 80))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
