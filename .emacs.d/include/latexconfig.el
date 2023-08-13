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
  (local-set-key (kbd "C-c C-z") #'custom-compile-latex)
  (local-set-key (kbd "C-c C-b") #'latex-insert-block)

  (set (make-local-variable 'TeX-electric-math)
       (cons "\\(" "\\)"))
  (set (make-local-variable 'LaTeX-electric-left-right-brace) t)

  (electric-indent-mode -1)
  (LaTeX-math-mode))

(add-hook 'LaTeX-mode-hook #'configure-latex)

(provide 'latexconfig)
;;; latexconfig.el ends here
