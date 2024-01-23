;;; pdfconfig.el --- configuration for working with PDF files
;;; Commentary:
;;    Configuration for working with PDF files.
;;; Code:

;; PDF Tools
(pdf-tools-install)

(setq doc-view-resolution 300)

(defun pdf-download-and-view (&optional url filename)
  "Download and view the PDF given by its URL as FILENAME."
  (interactive)
  (let* ((url (or url
                  (read-string "Download URL: ")))
         (default-filename (or filename
                               (car (last (split-string url "/" t)))))
         (filename (or filename
                       (read-string (format "Filename (%s): " default-filename)
                                    nil
                                    nil
                                    default-filename)))
         (file-path (concat "/tmp/" filename))
         (download-buffer (url-retrieve-synchronously url)))
    (set-buffer download-buffer)
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (forward-char)
    (delete-region (point-min) (point))
    (write-file file-path)
    (find-file (expand-file-name file-path))))

(setq pdf-view-continuous t)

;;; pdfconfig.el ends here
