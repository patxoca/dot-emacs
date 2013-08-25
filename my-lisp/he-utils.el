;;; he-utils.el --- utilitats per hippie-expand
;;; (c) 2006 Alexis Roda
;;; $Id$


;;; Commentary:
;;


;;; History:
;;

(require 'hippie-exp)
(require 'etags)

;;; Code:


(defun try-expand-tags (old)
  "Funcio compatile amb `hippie-expand' per expandir tags.

Argument OLD nil la primera vegada que es crida, t les seguents."
  ;; (message ">> try-expand-tags")
  (unless old
    (let ((start-point (save-excursion
                         (backward-word 1)
                         (point))))
      (he-init-string start-point (point))
      (setq he-expand-list (sort
                            (tags-complete-tag he-search-string nil t)
                            'string-lessp))))

  ;; (message "   he-search-string")
  ;; (message he-search-string)
  ;; (message "   he-expand-list")
  ;; (message (prin1-to-string he-expand-list))
  ;; (message "   he-tried-table")
  ;; (message (prin1-to-string he-tried-table))

  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (pop he-expand-list))
  (if (null he-expand-list)
      (progn
        (when old (he-reset-string))
        ())
    (he-substitute-string (car he-expand-list))
    (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
    (pop he-expand-list)
    t))

(provide 'he-utils)

;;; he-utils.el ends here
