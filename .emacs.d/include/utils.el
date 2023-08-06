;;; utils.el --- Utility functions.
;;; Commentary:
;;    Provides utility functions.
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

(provide 'utils)

;;; utils.el ends here
