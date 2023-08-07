;;; gpg.el --- Functions for working with gpg
;;; Commentary:
;;    Functions for working with gpg.
;;; Code:

(defun gpg-public-keys ()
  "Produces a list of GPG public keys."
  (interactive)

  (let ((buf (get-buffer-create "*gpg-public-keys*"))
        (info  (list))
        (keyid "")
        (curline  ""))
    (with-current-buffer buf
      (insert (shell-command-to-string "gpg -k"))
      (goto-char (point-min))
      ;; Clear the prelude
      (forward-line 2)
      (delete-region (point-min) (point))
      (goto-char (point-min))
      ;; Collect IDs, names, and emails
      (while (not (eobp))
        (setq curline (thing-at-point 'line t))
        (forward-line)
        (cond ((string-prefix-p "  " curline)
               (setq keyid (car (split-string curline))))
              ((string-prefix-p "uid" curline)
               (let* ((original-length (length curline))
                      (start-raw (concat (memq (string-to-char "]")
                                               (string-to-list curline))))
                      (start (+ 2 (- original-length
                                     (length start-raw))))
                      (name-email (substring curline start -1))
                      (keyid-name-email (concat keyid " " name-email)))
                 (if (not (memq (string-to-char "(")
                                (string-to-list name-email)))
                     (push keyid-name-email info)
                   ;; Remove key descriptions inside parentheses
                   (let* ((open-paren-start
                           (length (concat (memq (string-to-char "(")
                                                 (string-to-list name-email)))))
                          (name-part (substring name-email 0 (- open-paren-start)))
                          (close-paren-start
                           (length (concat (memq (string-to-char ")")
                                                 (string-to-list name-email)))))
                          (email-part (substring name-email
                                                 (+ 2 (- close-paren-start))))
                          (sanitized (concat keyid " " name-part email-part)))
                     (push sanitized info)))))))
      (setq info (-filter (lambda (s) (not (string-match "image" s))) info))
      info)))

(provide 'gpgconfig)
;;; gpgconfig.el ends here
