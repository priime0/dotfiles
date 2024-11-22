;;; priime-ai.el --- AI completion -*- lexical-binding: t -*-

;;; Commentary:

;; Contains configuration for AI completion.

;;; Code:

(use-package gptel :straight t)
(use-package copilot
  :straight (copilot :type git :host github :repo "copilot-emacs/copilot.el"))
(use-package copilot-chat
  :straight (copilot-chat :type git :host github :repo "chep/copilot-chat.el")
  :after (copilot org markdown-mode request shell-maker))

(provide 'priime-ai)

;;; priime-ai.el ends here
