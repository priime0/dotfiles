;;; priime-ai.el --- AI completion -*- lexical-binding: t -*-

;;; Commentary:

;; Contains configuration for AI completion.

;;; Code:

(defun gptel-context-delete ()
  "Wrapper over `gptel-context-remove'."
  (interactive)
  (gptel-context-remove))

(defvar-keymap gptel-context-keymap
  "a" #'gptel-add
  "d" #'gptel-context-delete
  "n" #'gptel-context-next
  "p" #'gptel-context-previous
  "v" #'gptel-context-visit
  "RET" #'gptel-context-confirm
  "f" #'gptel-add-file)

(defvar-keymap gptel-keymap
  "c" gptel-context-keymap
  "RET" #'gptel
  "e" #'gptel-send
  "r" #'gptel-rewrite-menu
  "o" #'gptel-menu)

(use-package gptel :straight t
  :custom
  (gptel-model 'claude-3-5-sonnet-20241022)
  (gptel-backend
   (gptel-make-anthropic "Claude"
     :stream t
     :key #'gptel-api-key-from-auth-source))
  :init
  (keymap-global-set "C-c g" gptel-keymap))
(use-package copilot
  :straight (copilot :type git :host github :repo "copilot-emacs/copilot.el"))
(use-package copilot-chat
  :straight (copilot-chat :type git :host github :repo "chep/copilot-chat.el")
  :after (copilot org markdown-mode request shell-maker))

(provide 'priime-ai)

;;; priime-ai.el ends here
