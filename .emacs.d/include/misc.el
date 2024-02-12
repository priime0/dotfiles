;;; misc.el --- Miscellaneous configuration for packages.
;;; Commentary:
;;    Miscellaneous configuration for packages.  If a package doesn't
;;    necessarily require its own configuration file (its configuration could be
;;    short), then it belongs here.
;;; Code:

(require 'meow)
(require 'lsp-mode)
(require 'eglot)
(require 'git-gutter)
(require 'elfeed)
(require 'neotree)
(require 'copilot)
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
(defun meow-setup ()
  "Set up meow."
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvp)
  (meow-leader-define-key
   '("?" . meow-cheatsheet))
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
   '("q" . meow-quit)
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
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)
   '("TAB" . meow-indent)))

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

;; Eglot
(with-eval-after-load 'eglot-java
  (define-key eglot-java-mode-map (kbd "C-c l n") #'eglot-java-file-new)
  (define-key eglot-java-mode-map (kbd "C-c l x") #'eglot-java-run-main)
  (define-key eglot-java-mode-map (kbd "C-c l t") #'eglot-java-run-test)
  (define-key eglot-java-mode-map (kbd "C-c l N") #'eglot-java-project-new)
  (define-key eglot-java-mode-map (kbd "C-c l T") #'eglot-java-project-build-task)
  (define-key eglot-java-mode-map (kbd "C-c l R") #'eglot-java-project-build-refresh)
  (define-key eglot-java-mode-map (kbd "C-c l a") #'eglot-code-actions)
  (define-key eglot-java-mode-map (kbd "C-c l r") #'eglot-rename)
  (setq c-basic-offset 2))

(set-face-attribute 'eglot-highlight-symbol-face nil
                    :family "JetBrains Mono"
                    :weight 'semi-bold
                    :underline t)

;; Elfeed
(setq elfeed-feeds
      '(("https://edwardwibowo.com/rss.xml"     blog)
        ("https://priime.dev/feed.xml"          blog)
        ("https://fasterthanli.me/index.xml"    blog)
        ("https://blog.cleancoder.com/atom.xml" blog)))

;; Magit
(setq auth-sources '("~/.authinfo"))

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Neotree
(defun neotree-toggle-current-directory ()
  "Toggle neotree at the current directory."
  (interactive)
  (let ((current-directory
         (or (and buffer-file-name (file-name-directory buffer-file-name))
             (and (eq major-mode 'dired-mode) (dired-current-directory))
             (and (eq major-mode 'magit-status-mode) (magit-toplevel))
             "~")))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (neotree-dir current-directory))))

(keymap-global-set "C-c t" #'neotree-toggle-current-directory)
(setq neo-theme 'icons)
(setq neo-smart-open t)
(setq neo-window-fixed-size nil)
(add-hook 'neotree-mode-hook (lambda () (text-scale-set -0.5)))

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
(slime-setup '(slime-fancy slime-company))

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

;; Grading
(defconst grading-directory-root "pl-grading"
  "The root grading directory repository.")

(defun grade-file-format (num)
  "Format NUM to a file in the grading directory."
  (let* ((dir              (if (eq 'dired-mode major-mode)
                               (dired-current-directory)
                             (file-name-directory (buffer-file-name))))
         (dir-files        (directory-files dir))
         (extension        ".rkt")
         (all-rkt-files    (-filter (lambda (f) (string= (file-name-extension f) "rkt"))
                                   dir-files))
         (all-rkt-files+   (-map #'file-name-sans-extension all-rkt-files))
         (hw-files         (-filter (lambda (f) (not (zerop (string-to-number f)))) all-rkt-files+))
         (hw-files+        (seq-sort-by #'length #'> hw-files))
         (digits           (length (car hw-files+)))
         (str-format       (format "%%0%dd.rkt" digits)))
    (format str-format num)))

(defun grade-next (&optional inc)
  "Go to the next homework by INC to grade."
  (interactive)
  (let ((inc (or inc 1))
        (extension ".rkt")
        (filepath (buffer-file-name (current-buffer))))
    (if (not (and (string-match-p grading-directory-root filepath)
                  (string-match-p extension filepath)))
        (message (format "not in %s!" grading-directory-root))
      (let* ((target-directory (file-name-directory filepath))
             (filename-parts (string-split filepath "/"))
             (source-filename (-last-item filename-parts))
             (source-number-str (file-name-sans-extension source-filename))
             (source-number (string-to-number source-number-str))
             (target-number (+ inc source-number))
             (target-filename (grade-file-format target-number))
             (target-filepath (concat target-directory target-filename)))
        (if (file-exists-p target-filepath)
            (find-file target-filepath)
          (message (format "next grading file doesn't exist!")))))))

(defun grade-prev (&optional dec)
  "Go to the previous homework by DEC to grade."
  (interactive)
  (let ((dec (or dec 1)))
    (grade-next (- dec))))

(defun grade-start (&optional start)
  "Start grading in the current directory with homework START."
  (interactive)
  (keymap-global-set "C-c f" #'grade-next)
  (keymap-global-set "C-c b" #'grade-prev)

  (let* ((start (or start
                    (string-to-number (read-string "File number to start with: "))))
         (dir (if (eq major-mode 'dired-mode)
                  (dired-current-directory)
                (error "Must be in Dired directory")))
         (dir-path (file-name-directory dir)))
    (if (not (string-match-p grading-directory-root dir-path))
        (error "Must be in subdirectory of %s" grading-directory-root)
      (let ((readme-file   "README.md")
            (solution-file "solution.rkt")
            (start-file    (concat dir-path (grade-file-format start))))
        (find-file start-file)
        (delete-other-windows)
        (split-window-right)
        (split-window-right)
        (balance-windows)
        (windmove-right)
        (find-file solution-file)
        (windmove-right)
        (find-file readme-file)
        (windmove-left)
        (windmove-left)))))

(provide 'misc)
;;; misc.el ends here
