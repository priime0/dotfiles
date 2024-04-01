;;; latexconfig.el --- Configuration for LaTeX
;;; Commentary:
;;    Custom configuration for LaTeX editing
;;; Code:

(require 'latex)
(require 'cdlatex)
(require 'f)

(require 'company)
(require 'company-math)

(defun custom-compile-latex ()
  "Run the `just' command -- the command I use to compile my environment."
  (interactive)
  (save-buffer)
  (async-start-process "emacs-just" "just" nil))

(defun open-out-pdf ()
  "Find and open the produced PDF via Zathura."
  (interactive)
  (save-excursion
    (when (f-directory-p "./out/")
      (defconst --pdf-files (f-glob "./out/*.pdf"))
      (unless (zerop (length --pdf-files))
        (defconst --pdf-command "zathura")
        (defconst --pdf-file (car --pdf-files))
        (async-start-process "emacs-zathura" --pdf-command nil --pdf-file)))))

(defun latex-env-theorem (environment)
  "Insert ENVIRONMENT with the given name and ref specifications."
  (let ((name (TeX-read-string "Name: " nil nil ""))
        (ref  (TeX-read-string "Ref: " nil nil "")))
    (LaTeX-insert-environment environment (format "{%s}{%s}" name ref))))

(defun enter-math ()
  "Enter and ensure math-mode."
  (interactive)
  (unless (texmathp)
    (cond ((use-region-p)
           (let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))
             (delete-region (region-beginning)
                            (region-end))
             (insert (concat "\\(" selection "\\)"))))
          (t
           (insert "\\(\\)")
           (backward-char 2)))))

(defun latex-absorbable-p ()
  "Determine if the character before the cursor can be absorbed."
  (backward-char)
  (defconst bc (char-after))
  (defconst bcs (char-to-string bc))
  (forward-char)
  (not (or (string-blank-p bcs)
           (memq bc '(?\ ?\( ?\( ?{ ?} ?\[ ?\])))))

(defun insert-raw-slash ()
  "Insert a `/'."
  (interactive)
  (insert "/"))

(defun math-insert-frac ()
  "Insert a fraction if in math mode."
  (interactive)
  (when (texmathp)
    (insert-frac)))

(defun insert-frac ()
  "Ensure math mode and then insert a fraction."
  (interactive)
  (enter-math)
  (backward-char)
  (defconst bc (char-after))
  (defconst bcs (char-to-string bc))
  (forward-char)
  (if (not (latex-absorbable-p))
      (LaTeX-math-frac (not (texmathp)))
    (push-mark)
    (backward-word)
    (defconst content (buffer-substring (point) (mark)))
    (delete-region (point) (mark))
    (pop-mark)
    (insert "\\frac{" content "}{}")
    (backward-char)))

(defun latex-tab ()
  "Multi-purpose tab for LaTeX."
  (interactive)
  (align-current)
  (cdlatex-tab))

(defun configure-latex ()
  "Configure my custom LaTex environment."
  (electric-indent-mode -1)
  (LaTeX-math-mode 1)
  (cdlatex-mode 1)
  (auto-fill-mode 1)
  (display-fill-column-indicator-mode 1)
  (set-fill-column 100)

  (setq-local flycheck-disabled-checkers
              '(tex-chktex))

  (keymap-local-set "C-c C-a" #'align-current)
  (keymap-local-set "C-c C-c" #'custom-compile-latex)
  (keymap-local-set "C-c C-z" #'open-out-pdf)
  (keymap-local-set "C-c C-b" #'latex-insert-block)
  (keymap-local-set "C-c C-h" #'enter-math)
  (keymap-local-set "C-c C-/" #'insert-frac)
  (keymap-local-set "/"       #'math-insert-frac)
  ;; For cases where we actually do want `/' in math...
  (keymap-local-set "M-/"     #'insert-raw-slash)
  (keymap-local-set "C-TAB"   #'latex-tab)
  (keymap-substitute cdlatex-mode-map 'cdlatex-dollar 'cdlatex-math-symbol)
  (setq cdlatex-math-symbol-prefix ?$)

  (setq cdlatex-use-dollar-to-ensure-math nil)
  (set (make-local-variable 'TeX-electric-math)
       (cons "\\(" "\\)"))
  (set (make-local-variable 'TeX-electric-sub-and-superscript) t)
  (set (make-local-variable 'LaTeX-electric-left-right-brace) t)
  (set 'preview-scale-function 0.75)

  (setq-local company-backends
              (append '((company-math-symbols-latex company-latex-commands))
                      company-backends))

  (LaTeX-add-environments
   '("theorem"     latex-env-theorem)
   '("corollary"   latex-env-theorem)
   '("lemma"       latex-env-theorem)
   '("definition"  latex-env-theorem)
   '("exercise"    latex-env-theorem)
   '("proposition" latex-env-theorem)
   '("example"     latex-env-theorem)
   '("remark"      latex-env-theorem)))

(add-hook 'LaTeX-mode-hook #'configure-latex)

(setq cdlatex-math-symbol-alist
      '((?I ("\\int"            "\\Im"))
        (?L ("\\Lambda"         "\\lim"))
        (?S ("\\Sigma"          "\\sum"))
        (?m ("\\mu"             "\\mod"))
        (?0 ("\\varnothing"     "\\emptyset"))
        (?! ("\\not"            "\\neq"))
        (?- ("\\neg"            ""                     "\\bot"))
        (?+ ("\\cup"            "\\cap"                "\\top"))
        (?{ ("\\subset"         "\\subseteq"))
        (?} ("\\supset"         "\\supseteq"))
        (?. ("\\cdots"          "\\ldots"))
        (?* ("\\cdot"           "\\times"))
        (?# ("\\mathbb"         "\\mathcal"))
        (?= ("\\Leftrightarrow" "\\Longleftrightarrow" "\\equiv"))))

(setq cdlatex-math-modify-alist
      '((?b "\\mathbb"  "\\textbf" t nil nil)
        (?B "\\mathbf"  "\\textbf" t nil nil)
        (?c "\\mathcal" nil t nil nil)
        (?s "\\sqrt" nil t nil nil)
        (?t "\\text" nil t nil nil)))

(setq LaTeX-font-list
      (cons (list ?\^U "\\underline{" "}")
            LaTeX-font-list))

(provide 'latexconfig)
;;; latexconfig.el ends here
