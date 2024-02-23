;;; orgconfig.el --- Configuration for org-mode.
;;; Commentary:
;;    Configuration for org-mode.
;;; Code:

(require 'org)
(require 'org-roam)
(require 'org-capture)
(require 'org-id)
(require 'org-modern)
;; Templates for org-mode
(require 'org-tempo)

(setq org-hide-emphasis-markers t)
(setq org-adapt-indentation nil)
(setq org-confirm-babel-evaluate nil)
(setq org-export-use-babel nil)
(setq org-agenda-include-diary t)
(setq org-src-preserve-indentation t)
(setq org-modern-star '("#" "##" "###" "####" "#####" "######"))
(setq org-roam-directory "~/org/docs/")
(setq org-roam-extract-new-file-path "${slug}.org")
(setq org-roam-node-display-template "${title}  (${id:8})")
(org-roam-db-autosync-mode)

;; Enable Racket in Org-mode Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((racket . t)
   (python . t)
   (java . t)))

(setq org-babel-default-header-args:racket
      '((:session . "none")
        (:results . "output")))

(defun org-roam-migrate ()
  "Migrate a regular org file into an org-roam file in my format."
  (interactive)
  (unless (eq major-mode 'dired-mode)
    (error "BAD!"))
  (dired-find-file)
  (let* ((id (org-id-get))
         (new-format "%s-%s.org")
         (filename-old (buffer-file-name (current-buffer)))
         (filename-min (file-name-sans-extension filename-old))
         (filename-new (format new-format filename-min id)))
    (rename-file filename-old filename-new)
    (kill-buffer (current-buffer))
    (revert-buffer)))

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
(setq org-roam-capture-templates
      '(("n" "default" plain "%?"
         :target (file+head "${slug}:${id}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("N" "encrypted" plain "%?"
         :target (file+head "${slug}:${id}.org.gpg" "#+title: ${title}\n")
         :unnarrowed t)))
(setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 3)
                           ("~/org/someday.org" :level . 1)
                           ("~/org/tickler.org" :maxlevel . 2)))
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(defmacro η (fnc)
  "Return function that ignores its arguments and invokes FNC."
  `(lambda (&rest _rest)
     (funcall ,fnc)))

;; Keybinds
(defvar-keymap org-roam-keymap
  "n" #'org-roam-capture
  "f" #'org-roam-node-find
  "i" #'org-roam-node-insert)

(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)
(keymap-global-set "C-c o" org-roam-keymap)

;; Org hooks
(add-hook 'org-mode-hook
          (lambda ()
            (modify-syntax-entry ?< ".")
            (modify-syntax-entry ?> ".")))
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
            (auto-save-mode)))
(add-hook 'org-mode-hook #'olivetti-mode)
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-mode-hook #'org-recur-mode)
(add-hook 'org-mode-hook #'variable-pitch-mode)
(add-hook 'org-agenda-mode-hook #'org-recur-mode)
(add-hook 'org-mode-hook #'org-modern-indent-mode 90)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
(add-hook 'org-mode-hook #'(lambda () (setq line-spacing 0.1)))

;; Diary-syncing functionality
(defun pull-diary ()
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

;; Theme
(custom-set-faces
 '(org-document-info ((t (:inherit nano-strong :height 1.0))))
 '(org-document-title ((t (:inherit nano-strong :height 1.5 :family "Linux Libertine"))))
 '(org-document-info-keyword ((t :inherit (nano-faded fixed-pitch))))
 '(org-level-1 ((t (:inherit nano-strong :extend nil :height 1.4))))
 '(org-level-2 ((t (:inherit nano-strong :extend nil :height 1.3))))
 '(org-level-3 ((t (:inherit nano-strong :extend nil :height 1.2))))
 '(org-level-4 ((t (:inherit nano-strong :extend nil :height 1.1))))
 '(org-level-4 ((t (:inherit nano-strong :extend nil :height 1.0))))
 '(org-level-4 ((t (:inherit nano-strong :extend nil :height 1.0))))
 '(org-link ((t (:inherit nano-salient :underline t))))
 '(org-code ((t :inherit (nano-salient fixed-pitch))))
 '(org-verbatim ((t :inherit (nano-popout fixed-pitch))))
 '(org-indent ((t :inherit (org-hide fixed-pitch))))
 '(org-table ((t :inherit fixed-pitch)))
 '(org-block-begin-line ((t :inherit (nano-faded fixed-pitch) :extend t :underline nil)))
 '(org-block-end-line ((t :inherit   (nano-faded fixed-pitch) :extend t :overline nil)))
 '(org-block ((t :inherit fixed-pitch :background "#f3f3f3" :extend t)))
 '(org-meta-line ((t :inherit (nano-faded fixed-pitch))))
 '(org-drawer ((t :inherit (nano-faded fixed-pitch)))))


(provide 'orgconfig)
;;; orgconfig.el ends here
