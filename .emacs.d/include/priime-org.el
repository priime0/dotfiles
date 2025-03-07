;;; priime-org.el --- Org-mode configuration
;;; Commentary:
;; Provides configuration for org-related packages.
;;; Code:

(defvar-keymap priime-org-keymap
  "a" #'org-agenda
  "c" #'org-capture
  "n" #'org-roam-capture
  "f" #'org-roam-node-find
  "i" #'org-roam-node-insert
  "t" #'org-roam-tag-add
  "s" #'org-save-all-org-buffers
  "w" #'org-switch-workspace)

(defun org-switch-workspace (&optional workspace-name)
  "Switch the current org-roam-directory to WORKSPACE-NAME."
  (interactive)
  (defconst default-workspace-name (caar org-workspaces-alist))
  (defconst prompt-message (format "Org Roam Workspace (default \"%s\"): "
                                   default-workspace-name))
  (let ((workspace-name
         (or workspace-name
             (completing-read prompt-message
                              org-workspaces-alist
                              nil t nil nil
                              default-workspace-name))))
    (setq org-roam-directory (cadr (assoc workspace-name org-workspaces-alist)))
    (setq org-roam-db-location (cddr (assoc workspace-name org-workspaces-alist)))
    (org-roam-db-sync nil)))

(defun org-roam-migrate ()
  "Migrate a regular org file into an org-roam file in my format."
  (interactive)
  (unless (eq major-mode 'dired-mode)
    (error "BAD!"))
  (dired-find-file)
  (let* ((id (org-id-get-create))
         (_ (save-buffer))
         (new-format "%s-%s.org")
         (filename-old (buffer-file-name (current-buffer)))
         (filename-min (file-name-sans-extension filename-old))
         (filename-new (format new-format filename-min id)))
    (rename-file filename-old filename-new)
    (kill-buffer (current-buffer))
    (revert-buffer)))

(defun priime-pull-diary ()
  "Syncs the calendars directory and imports the calendars into the diary."
  (interactive)
  ;; Sync the calendars directory
  (let ((default-directory (expand-file-name "~/.calendars/")))
    (shell-command "./sync"))
  ;; Import the calendars into the diary
  (let* ((diary-file-name (expand-file-name "~/.emacs.d/diary"))
         (calendar-dir (expand-file-name "~/.calendars/"))
         (calendar-file-suffix ".ics")
         (calendar-file? (lambda (f) (s-suffix? calendar-file-suffix f)))
         (calendar-dir-files (directory-files calendar-dir))
         (calendar-names (-filter calendar-file? calendar-dir-files))
         (calendar-files (-map (lambda (f) (expand-file-name f calendar-dir)) calendar-names)))
    (find-file diary-file-name)
    (delete-region (point-min) (point-max))
    (save-buffer)
    (kill-buffer)
    (-map (lambda (f)
            (find-file f)
            (icalendar-import-buffer diary-file-name t nil)
            (kill-buffer))
          calendar-files)
    (find-file diary-file-name)
    (delete-duplicate-lines (point-min) (point-max))
    (save-buffer)
    (kill-buffer)))

(defun priime-setup-org ()
  "Small setup for `org-mode'."
  (modify-syntax-entry ?< ".")
  (modify-syntax-entry ?> ".")
  (setq line-spacing 0.1)
  (electric-indent-mode -1))

(use-package org :straight t
  :after (ob-racket)
  :custom
  (org-workspaces-alist '(("docs" . ("~/org/docs/" . "~/.emacs.d/org-roam.db"))))
  (org-hide-emphasis-markers t)
  (org-adapt-indentation nil)
  (org-confirm-babel-evaluate nil)
  (org-export-use-babel nil)
  (org-agenda-include-diary t)
  (org-src-preserve-indentation t)
  (org-link-frame-setup '((vm      . vm-visit-folder-other-frame)
                          (vm-imap . vm-visit-imap-folder-other-frame)
                          (gnus    . org-gnus-no-new-news)
                          (file    . find-file)
                          (wl      . wl-other-frame)))
  (org-agenda-files '("~/org/gtd.org"))
  (org-capture-templates '(("t" "Todo [inbox]" entry
                            (file+headline "~/org/gtd.org" "GTD")
                            "* TODO %i%?")))
  (org-refile-targets '(("~/org/gtd.org" :maxlevel . 3)
                        ("~/org/someday.org" :level . 1)
                        ("~/org/tickler.org" :maxlevel . 2)))
  (org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  :custom-face
  (org-document-info ((t (:inherit nano-strong :height 1.0))))
  (org-document-title ((t (:inherit nano-strong :height 1.5 :family "Roboto"))))
  (org-document-info-keyword ((t :inherit (nano-faded fixed-pitch))))
  (org-level-1 ((t (:inherit nano-strong :extend nil :height 1.4))))
  (org-level-2 ((t (:inherit nano-strong :extend nil :height 1.3))))
  (org-level-3 ((t (:inherit nano-strong :extend nil :height 1.2))))
  (org-level-4 ((t (:inherit nano-strong :extend nil :height 1.1))))
  (org-level-4 ((t (:inherit nano-strong :extend nil :height 1.0))))
  (org-level-4 ((t (:inherit nano-strong :extend nil :height 1.0))))
  (org-link ((t (:inherit nano-salient :underline t))))
  (org-code ((t :inherit (nano-salient fixed-pitch))))
  (org-verbatim ((t :inherit (nano-popout fixed-pitch))))
  (org-indent ((t :inherit (org-hide fixed-pitch))))
  (org-table ((t :inherit fixed-pitch)))
  (org-block-begin-line ((t :inherit (nano-faded fixed-pitch) :extend t :underline nil)))
  (org-block-end-line ((t :inherit   (nano-faded fixed-pitch) :extend t :overline nil)))
  (org-block ((t :inherit fixed-pitch :background "#f3f3f3" :extend t)))
  (org-meta-line ((t :inherit (nano-faded fixed-pitch))))
  (org-drawer ((t :inherit (nano-faded fixed-pitch))))
  :hook
  ((org-mode . auto-save-mode)
   (auto-save . org-save-all-org-buffers)
   (org-mode . variable-pitch-mode))
  :init
  (keymap-global-set "C-c o" priime-org-keymap)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((racket . t)
     (python . t)))
  (priime-setup-org))
(use-package org-roam :straight t
  :after (org)
  :custom
  (org-roam-directory "~/org/docs")
  (org-roam-extract-new-file-path "${slug}.org")
  (org-roam-node-display-template
   (concat "${title}  " "(${id:8}) " (propertize "[${tags}]" 'face 'org-tag)))
  (org-roam-capture-templates '(("n" "default" plain "%?"
                                 :target (file+head "${slug}-${id}.org" "#+title: ${title}\n")
                                 :unnarrowed t)
                                ("N" "encrypted" plain "%?"
                                 :target (file+head "${slug}-${id}.org.gpg" "#+title: ${title}\n")
                                 :unnarrowed t)))
  :init
  (org-roam-db-autosync-mode))
(use-package org-modern :straight t
  :after (org)
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star 'replace))
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
    :repo "hasu/emacs-ob-racket")
  :custom
  (org-babel-default-header-args:racket '((:session . "none")
                                          (:results . "output"))))

(provide 'priime-org)

;;; priime-org.el ends here
