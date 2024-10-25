;;; misc.el --- Miscellaneous configuration for packages.
;;; Commentary:
;;    Miscellaneous configuration for packages.  If a package doesn't
;;    necessarily require its own configuration file (its configuration could be
;;    short), then it belongs here.
;;; Code:

(require 'meow)
(require 'lsp-mode)
(require 'git-gutter)
(require 'seq)

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

;; meow
(defun meow-mx (arg)
  "Press alt + x."
  (interactive "P")
  (execute-extended-command arg))

(defun meow-mq ()
  "Reformat/reindent."
  (interactive)
  (call-interactively (global-key-binding "\M-q")))

(defun meow-yank-above ()
  "Yank the killed text to the line above."
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (meow-yank)))

(defun meow-change-line ()
  "Kill rest of line and switch to insert state."
  (interactive)
  (kill-line)
  (meow-insert))

(defun meow-setup ()
  "Set up meow."
  (keymap-global-set "C-q" #'meow-quit)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvp)
  (meow-leader-define-key '("u" . "C-u"))
  (meow-motion-overwrite-define-key
   ;; custom keybinding for motion state
   '("<escape>" . ignore))
  (meow-normal-define-key
   '("?" . meow-cheatsheet)
   '("*" . meow-expand-0)
   '("=" . meow-expand-9)
   '("!" . meow-expand-8)
   '("[" . meow-expand-7)
   '("]" . meow-expand-6)
   '("{" . meow-expand-5)
   '("+" . meow-expand-4)
   '("}" . meow-expand-3)
   '(")" . meow-expand-2)
   '("(" . meow-expand-1)
   '("1" . digit-argument)
   '("2" . digit-argument)
   '("3" . digit-argument)
   '("4" . digit-argument)
   '("5" . digit-argument)
   '("6" . digit-argument)
   '("7" . digit-argument)
   '("8" . digit-argument)
   '("9" . digit-argument)
   '("0" . digit-argument)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-line)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-line)
   '("E" . meow-goto-line)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-till)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-mq)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-search)
   '("t" . meow-right)
   '("T" . meow-right-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-save)
   '("X" . meow-sync-grab)
   '("y" . meow-yank)
   '("Y" . meow-yank-above)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '(":" . meow-mx)
   '("<escape>" . ignore)))

(meow-setup)

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

(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

;; Magit
(setq auth-sources '("~/.authinfo"))

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Neotree
(defun neotree-toggle-dir-or-project ()
  "By default, toggle the directory at the project level, with prefix current."
  (interactive)
  (if (eq current-prefix-arg nil)
      (neotree-toggle-project)
    (neotree-toggle-current-directory)))

(defun neotree-toggle-project ()
  "Toggle neotree at the project level if the project exists, otherwise current."
  (interactive)
  (if (projectile-project-p)
      (neotree-toggle-directory (projectile-project-root))
    (neotree-toggle-current-directory)))

(defun neotree-toggle-current-directory ()
  "Toggle neotree at the current directory."
  (interactive)
  (let ((current-directory
         (or (and buffer-file-name (file-name-directory buffer-file-name))
             (and (eq major-mode 'dired-mode) (dired-current-directory))
             (and (eq major-mode 'magit-status-mode) (magit-toplevel))
             "~")))
    (neotree-toggle-directory current-directory)))

(defun neotree-toggle-directory (dir)
  "Toggle neotree at the given DIR."
  (if (and (fboundp 'neo-global--window-exists-p)
           (neo-global--window-exists-p))
      (neotree-hide)
    (neotree-dir dir)))


;; hledger
(setq hledger-currency-string "$")
(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))
(setq hledger-jfile "~/finance/2024.journal")


;; SLIME
(setq inferior-lisp-program "sbcl")

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

;; Consult
(keymap-global-set "C-c r r" #'consult-ripgrep)
(keymap-global-set "C-c r g" #'consult-grep)

;; hl-todo
(custom-set-faces
 '(hl-todo ((t (:inherit nano-salient-i)))))
(setq hl-todo-keyword-faces
      '(("HOLD" .   "#ffffff")
        ("TODO" .   "#ffffff")
        ("NEXT" .   "#ffffff")
        ("THEM" .   "#ffffff")
        ("PROG" .   "#ffffff")
        ("OKAY" .   "#ffffff")
        ("DONT" .   "#ffffff")
        ("FAIL" .   "#ffffff")
        ("DONE" .   "#ffffff")
        ("NOTE" .   "#ffffff")
        ("MAYBE" .  "#ffffff")
        ("KLUDGE" . "#ffffff")
        ("HACK" .   "#ffffff")
        ("TEMP" .   "#ffffff")
        ("FIXME" .  "#ffffff")
        ("XXXX*" .  "#ffffff")))

;; rjsx

(setq auto-mode-alist (cons '("\\.tsx$" . rjsx-mode) auto-mode-alist))

;; elixir

(defvar-keymap inf-elixir-keymap
  "i" #'inf-elixir
  "p" #'inf-elixir-project
  "c" #'inf-elixir-send-line
  "b" #'inf-elixir-send-buffer
  "r" #'inf-elixir-send-region
  "R" #'inf-elixir-reload-module)
(add-hook 'elixir-mode-hook
          (lambda ()
            (keymap-local-set "C-c i" inf-elixir-keymap)))

(provide 'misc)
;;; misc.el ends here
