;;; orgconfig.el --- Configuration for org-mode.
;;; Commentary:
;;    Configuration for org-mode.
;;; Code:

(require 'org)
(require 'org-capture)
;; Templates for org-mode
(require 'org-tempo)

(setq org-hide-emphasis-markers t)
(setq org-adapt-indentation nil)
(setq org-confirm-babel-evaluate nil)
(setq org-export-use-babel nil)
(setq org-agenda-include-diary t)

;; Enable Racket in Org-mode Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((racket . t)
   (python . t)))

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

;; Org hooks
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
            (add-hook 'org-mode-hook 'auto-fill-mode)

            (auto-save-mode)))
(add-hook 'org-mode-hook #'olivetti-mode)
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-mode-hook #'org-recur-mode)
(add-hook 'org-agenda-mode-hook #'org-recur-mode)
(add-hook 'org-mode-hook #'org-modern-indent-mode 90)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; Diary-syncing functionality
(defun pull-diary ()
  "MUST sync the calendars prior to running this command"
  (interactive)
  (let* ((diary-file-name (expand-file-name "~/.emacs.d/diary"))
         (calendar-dir (expand-file-name "~/.calendars/"))
         (calendar-file-suffix ".ics")
         (calendar-file? (lambda (f) (s-suffix? calendar-file-suffix f)))
         (calendar-dir-files (directory-files calendar-dir))
         (calendar-names (-filter calendar-file? calendar-dir-files))
         (calendar-files (-map (lambda (f) (expand-file-name f calendar-dir)) calendar-names)))
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
 '(org-document-info ((t (:inherit nano-strong :height 1.0 :family "Roboto Mono"))))
 '(org-document-title ((t (:inherit nano-strong :height 1.5 :family "Roboto Mono"))))
 '(org-level-1 ((t (:inherit nano-strong :extend nil :height 1.4 :family "Inter"))))
 '(org-level-2 ((t (:inherit nano-strong :extend nil :height 1.3 :family "Inter"))))
 '(org-level-3 ((t (:inherit nano-strong :extend nil :height 1.2 :family "Inter"))))
 '(org-level-4 ((t (:inherit nano-strong :extend nil :height 1.1 :family "Inter"))))
 '(org-link ((t (:inherit nano-salient :underline t)))))


(provide 'orgconfig)
;;; orgconfig.el ends here
