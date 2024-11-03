;;; init.el -- Base configuration
;;; Commentary:
;;    Base configuration.
;;; Code:

;;; Straight and Packages

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(add-to-list 'load-path "~/.emacs.d/include")

(load-library "priime-elisp")
(load-library "priime-utils")
(load-library "priime-general")
(load-library "priime-lsp")
(load-library "priime-completion")
(load-library "priime-edit")
(load-library "priime-convenience")
(load-library "priime-vc")
(load-library "priime-ui")
(load-library "priime-org")
(load-library "priime-languages")

(load-library "orgconfig")
(when (eq system-type 'gnu/linux)
  (load-library "mu4econfig"))
(load-library "racket")
(load-library "latexconfig")
(load-library "cppconfig")
(load-library "pdfconfig")
(load-library "gpgconfig")
(load-library "rustconfig")
(load-library "ocamlconfig")

;; ====== Hooks ==============================
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
