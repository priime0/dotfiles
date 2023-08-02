;;; cppconfig.el --- Configuration for C/C++
;;; Commentary:
;;    Custom configuration for C/C++.
;;; Code:

(defun custom-compile-c++ ()
  "Run the `just' command -- the command I use to compile my environment."
  (interactive)
  (save-buffer)
  (compile (concat "just build " (buffer-name))))

(defun custom-run-c++ ()
  "Run the produced `bin' binary."
  (interactive)
  (shell-command "./bin"))

(defun configure-c++ ()
  "Configure my custom C++ enviroment."
  (local-set-key (kbd "C-c C-c") #'custom-compile-c++)
  (local-set-key (kbd "C-c C-r") #'custom-run-c++)
  (local-set-key (kbd "C-c C-.")   #'company-complete))

(add-hook 'c-mode-hook   #'configure-c++)
(add-hook 'c++-mode-hook #'configure-c++)

(provide 'cppconfig)
;;; cppconfig.el ends here
