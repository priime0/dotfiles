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
                      (name-email (substring curline start -1)))
                 (if (not (memq (string-to-char "(")
                                (string-to-list name-email)))
                     (push (cons name-email keyid) info)
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
                          (sanitized (concat name-part email-part)))
                     (push (cons sanitized keyid) info)))))))
      (setq info (-filter (lambda (s) (not (string-match "image" (car s)))) info))
      info)))

(defun gpg-encrypt-sign-save-buffer (&optional filename)
  "Encrypt, sign, and save the buffer as FILENAME.asc for the recipients."
  (interactive)

  (let* ((key-alist (gpg-public-keys))
         (recipients (completing-read-multiple "Recipients: " key-alist))
         (recipient-keys (mapcar (lambda (x) (cdr (assoc x key-alist)))
                                 recipients))
         (recipients-cli-args (mapconcat (lambda (s) (concat "-r " s))
                                         recipient-keys
                                         " ")))
    (unless (or filename buffer-file-name)
      (setq filename (read-string "Filename: " nil nil)))
    (let ((gpg-encrypt-command
           (concat "gpg --encrypt --sign --armor "
                   recipients-cli-args
                   " "
                   (or filename buffer-file-name))))
      (redraw-display)
      (term gpg-encrypt-command))))

(defun gpg-decrypt-buffer (&optional filename)
  "Decrypt the current file FILENAME with GPG."
  (interactive)

  (let* ((filename (or filename
                       buffer-file-name
                       (read-string "Decrypt file: ")))
         (buf (get-buffer-create "*gpg-decrypt*")))
    (switch-to-buffer buf)
    (insert (shell-command-to-string (concat "gpg --decrypt " filename)))))

(provide 'gpgconfig)
;;; gpgconfig.el ends here
