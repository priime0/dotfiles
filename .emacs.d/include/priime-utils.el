;;; priime-utils.el --- Utility functions and commands
;;; Commentary:
;; Provides utility functions and commands.
;;; Code:

(defun lists->alist (l1 l2)
  "Transform lists L1 (keys) and L2 (values) into a single alist."
  (cond ((and (null l1)
              (null l2))
         '())
        ((or (null l1)
             (null l2))
         (error "Lists L1 and L2 have to be of equal length"))
        ((and (consp l1)
              (consp l2))
         (cons `(,(car l1) . ,(car l2))
               (lists->alist (cdr l1)
                             (cdr l2))))))

(defun download-file (&optional url filepath)
  "Download the file from URL to FILEPATH."
  (interactive)
  (let* ((url (or url
                  (read-string "url: ")))
         (filepath (or filepath (read-file-name "filename: "))))
    (url-copy-file url filepath 1)))

(defun buffer-major-mode (buffer-name)
  "Get the major mode of the buffer with the given name."
  (buffer-local-value 'major-mode (get-buffer buffer-name)))

(defun buffer-vterm-p (buffer-name)
  "Is the buffer with the given name a vterm buffer?"
  (eq 'vterm-mode (buffer-major-mode buffer-name)))

(provide 'priime-utils)

;;; priime-utils.el ends here
