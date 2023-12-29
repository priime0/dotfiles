;;; ocamlconfig.el --- Configuration for OCaml and Tuareg
;;; Commentary:
;;; Code:

(add-hook 'tuareg-mode-hook #'flycheck-ocaml-setup)
(add-hook 'tuareg-mode-hook #'(lambda () (merlin-mode -1)))
(setq utop-command "opam exec -- dune utop . -- -emacs")

;;; ocamlconfig.el ends here
