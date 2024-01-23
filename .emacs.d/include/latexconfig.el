;;; latexconfig.el --- Configuration for LaTeX
;;; Commentary:
;;    Custom configuration for LaTeX editing
;;; Code:

(require 'cdlatex)

(defun custom-compile-latex ()
  "Run the `just' command -- the command I use to compile my environment."
  (interactive)
  (save-buffer)
  (shell-command-to-string "just"))

(defun configure-latex ()
  "Configure my custom LaTex environment."
  (electric-indent-mode -1)
  (LaTeX-math-mode 1)
  (cdlatex-mode 1)
  (auto-fill-mode 1)
  (display-fill-column-indicator-mode 1)
  (set-fill-column 100)
  
  (local-set-key (kbd "C-c C-z") #'custom-compile-latex)
  (local-set-key (kbd "C-c C-b") #'latex-insert-block)
  (local-set-key (kbd "C-c C-h")
                 (lambda ()
                   (interactive)
                   (insert "\\(\\)")
                   (backward-char 2)))
  (keymap-substitute cdlatex-mode-map 'cdlatex-dollar 'cdlatex-math-symbol)
  (setq cdlatex-math-symbol-prefix ?$)

  (set (make-local-variable 'TeX-electric-math)
       (cons "\\(" "\\)"))
  (set (make-local-variable 'TeX-electric-sub-and-superscript) t)
  (set (make-local-variable 'LaTeX-electric-left-right-brace) t)
  (set 'preview-scale-function 0.75))

(add-hook 'LaTeX-mode-hook #'configure-latex)

(setq cdlatex-math-symbol-alist
      '((?I ("\\int"        "\\Im"))
        (?L ("\\Lambda"     "\\lim"))
        (?0 ("\\varnothing" "\\emptyset"))
        (?- ("\\not"        "\\neg"))
        (?+ ("\\cup"        "\\cap"))
        (?/ ("\\frac"))
        (?{ ("\\subset"     "\\subseteq"))
        (?} ("\\supset"     "\\supseteq"))
        (?. ("\\cdot"       "\\cdots"    "\\ldots"))
        (?# ("\\mathbb"     "\\mathcal"))))

(setq cdlatex-math-modify-alist
      '((?b "\\mathbb"  "\\textbf" t nil nil)
        (?B "\\mathbf"  "\\textbf" t nil nil)
        (?c "\\mathcal" nil t nil nil)
        (?t "\\text" nil t nil nil)))

(provide 'latexconfig)
;;; latexconfig.el ends here
