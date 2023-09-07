;;; latexconfig.el --- Configuration for LaTeX
;;; Commentary:
;;    Custom configuration for LaTeX editing
;;; Code:

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
  
  (local-set-key (kbd "C-c C-z") #'custom-compile-latex)
  (local-set-key (kbd "C-c C-b") #'latex-insert-block)
  (local-set-key (kbd "C-c C-h")
                 (lambda ()
                   (interactive)
                   (insert "\\(\\)")
                   (backward-char 2)))

  (set (make-local-variable 'TeX-electric-math)
       (cons "\\(" "\\)"))
  (set (make-local-variable 'TeX-electric-sub-and-superscript) t)
  (set (make-local-variable 'LaTeX-electric-left-right-brace) t)
  (set 'preview-scale-function 0.75))

(add-hook 'LaTeX-mode-hook #'configure-latex)

(provide 'latexconfig)
;;; latexconfig.el ends here
