;;; he-utils.el --- utilitats per hippie-expand
;;; (c) 2006 Alexis Roda
;;; $Id$


;;; Commentary:
;;


;;; History:
;;


;;; Code:


;;;###autoload
(defun try-expand-tags (old)
  "Funcio compatile amb `hippie-expand' per expandir tags.

Argument OLD nil la primera vegada que es crida, t les seguents."
  ;; @NOTE: alex 2014-08-02 16:30:17: Aquesta funció s'utilitza amb
  ;; `make-hippie-expand-function', en eixe punt el mòdul
  ;; `hippie-expand' estarà carregat, motiu pel que no es requereix el
  ;; mòdul ni es defineix cap autoload. De la mateixa forma la funció
  ;; `tags-complete-tag' es carrega al inicialitzar emacs (mòdul
  ;; `compat'), amb lo que està disponible.
  (unless old
    (let ((start-point (save-excursion
                         (backward-word 1)
                         (point))))
      (he-init-string start-point (point))
      (setq he-expand-list (sort
                            (tags-complete-tag he-search-string nil t)
                            'string-lessp))))
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
