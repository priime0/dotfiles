;;; priime-ui.el --- UI configuration
;;; Commentary:
;; Provides configuration for the UI.
;;; Code:

(require 'projectile)
(require 'magit)

;;; Neotree

(defun neotree-toggle-dir-or-project ()
  "By default, toggle the directory at the project level, with prefix current."
  (interactive)
  (if (eq current-prefix-arg nil)
      (neotree-toggle-project)
    (neotree-toggle-current-directory)))

(defun neotree-toggle-project ()
  "Toggle neotree at the project level if the project exists, otherwise current."
  (interactive)
  (if (projectile-project-p)
      (neotree-toggle-directory (projectile-project-root))
    (neotree-toggle-current-directory)))

(defun neotree-toggle-current-directory ()
  "Toggle neotree at the current directory."
  (interactive)
  (let ((current-directory
         (or (and buffer-file-name (file-name-directory buffer-file-name))
             (and (eq major-mode 'dired-mode) (dired-current-directory))
             (and (eq major-mode 'magit-status-mode) (magit-toplevel))
             "~")))
    (neotree-toggle-directory current-directory)))

(defun neotree-toggle-directory (dir)
  "Toggle neotree at the given DIR."
  (if (and (fboundp 'neo-global--window-exists-p)
           (neo-global--window-exists-p))
      (neotree-hide)
    (neotree-dir dir)))

;;; Windmove
(defun priime-window-left ()
  "Move or swap with the left window."
  (interactive)
  (if (eq current-prefix-arg nil)
      (windmove-left)
    (windmove-swap-states-left)))

(defun priime-window-right ()
  "Move or swap with the right window."
  (interactive)
  (if (eq current-prefix-arg nil)
      (windmove-right)
    (windmove-swap-states-right)))

(defun priime-window-down ()
  "Move or swap with the down window."
  (interactive)
  (if (eq current-prefix-arg nil)
      (windmove-down)
    (windmove-swap-states-down)))

(defun priime-window-up ()
  "Move or swap with the up window."
  (interactive)
  (if (eq current-prefix-arg nil)
      (windmove-up)
    (windmove-swap-states-up)))

(defun priime-open-on-right ()
  "Take the current window's buffer and open it on the right window."
  ;; This has an extremely niche usecase. When editing an OCaml project, I can
  ;; have multiple windows open to different buffers. I want to mirror two
  ;; windows such that one displays the module interface and the other displays
  ;; the module implementation. This allows me to set both windows to the same
  ;; overall "module", then quickly swap to the corresponding
  ;; interface/implementation.
  (interactive)
  (let ((buf (current-buffer)))
    (priime-window-right)
    (switch-to-buffer buf)))

(use-package nano-theme
  :straight '(nano-theme :type git :host github
                         :repo "rougier/nano-theme")
  :init
  (load-theme 'nano t)
  (nano-light))
(use-package windmove :straight nil
  :init
  (keymap-set override-global-map "S-<left>" #'priime-window-left)
  (keymap-set override-global-map "S-<right>" #'priime-window-right)
  (keymap-set override-global-map "S-<down>" #'priime-window-down)
  (keymap-set override-global-map "S-<up>" #'priime-window-up))
(use-package all-the-icons :straight t)
(use-package neotree :straight t
  :after (all-the-icons)
  :custom
  ((neo-theme 'icons)
   (neo-smart-open t)
   (neo-window-fixed-size nil)
   (neo-show-hidden-files t))
  :bind (("C-c t" . #'neotree-toggle-dir-or-project)))
(use-package git-gutter :straight t
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:update-interval 1)
  :config
  (git-gutter:start-update-timer))
(use-package hl-todo :straight t
  :hook ((prog-mode . hl-todo-mode)
         (LaTeX-mode . hl-todo-mode))
  :bind (("C-c r h" . hl-todo-rgrep))
  :custom-face
  (hl-todo ((t (:inherit nano-salient-i))))
  :custom
  (hl-todo-keyword-faces
   '(("HOLD" .   "#ffffff")
     ("TODO" .   "#ffffff")
     ("NEXT" .   "#ffffff")
     ("THEM" .   "#ffffff")
     ("PROG" .   "#ffffff")
     ("OKAY" .   "#ffffff")
     ("DONT" .   "#ffffff")
     ("FAIL" .   "#ffffff")
     ("DONE" .   "#ffffff")
     ("NOTE" .   "#ffffff")
     ("MAYBE" .  "#ffffff")
     ("KLUDGE" . "#ffffff")
     ("HACK" .   "#ffffff")
     ("TEMP" .   "#ffffff")
     ("FIXME" .  "#ffffff")
     ("XXXX*" .  "#ffffff"))))
(use-package ligature :straight t
  :config
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  (global-ligature-mode t))

(provide 'priime-ui)

;;; priime-ui.el ends here
