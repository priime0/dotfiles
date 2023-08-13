;;; orgconfig.el --- Configuration for org-mode.
;;; Commentary:
;;    Configuration for org-mode.
;;; Code:

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

;; Org hooks
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
            (add-hook 'org-mode-hook 'auto-fill-mode)
            (auto-save-mode)))

(provide 'orgconfig)
;;; orgconfig.el ends here
