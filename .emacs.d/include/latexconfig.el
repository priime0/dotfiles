;;; latexconfig.el --- Configuration for LaTeX
;;; Commentary:
;;    Custom configuration for LaTeX editing
;;; Code:

(require 'latex)
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
  
  (keymap-local-set "C-c C-z" #'custom-compile-latex)
  (keymap-local-set "C-c C-b" #'latex-insert-block)
  (keymap-local-set "C-c C-h"
                    (lambda ()
                      (interactive)
                      (unless (texmathp)
                        (insert "\\(\\)")
                        (backward-char 2))))
  (keymap-local-set "C-c C-/"
                    (lambda ()
                      (interactive)
                      (LaTeX-math-frac (not (texmathp)))))
  (keymap-substitute cdlatex-mode-map 'cdlatex-dollar 'cdlatex-math-symbol)
  (setq cdlatex-math-symbol-prefix ?$)

  (setq cdlatex-use-dollar-to-ensure-math nil)
  (set (make-local-variable 'TeX-electric-math)
       (cons "\\(" "\\)"))
  (set (make-local-variable 'TeX-electric-sub-and-superscript) t)
  (set (make-local-variable 'LaTeX-electric-left-right-brace) t)
  (set 'preview-scale-function 0.75))

(add-hook 'LaTeX-mode-hook #'configure-latex)

(setq cdlatex-math-symbol-alist
      '((?I ("\\int"        "\\Im"))
        (?L ("\\Lambda"     "\\lim"))
        (?S ("\\Sigma"      "\\sum"))
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
        (?s "\\sqrt" nil t nil nil)
        (?t "\\text" nil t nil nil)))

(setq LaTeX-font-list
      (cons (list ?\^U "\\underline{" "}")
            LaTeX-font-list))

(provide 'latexconfig)
;;; latexconfig.el ends here
