;;; python-pydoc-info.el --- accés a la documentació de python

;; $Id$

;;; Commentary:
;;
;; Integració de la documentació de python y altres amb info via
;; `pydoc-info'.

;;; History:
;;


;;; Code:

(autoload 'info-lookup-add-help "info-look" "" nil nil)

(eval-after-load "python"
  (info-lookup-add-help
   :mode 'python-mode
   :parse-rule 'pydoc-info-python-symbol-at-point
   :doc-spec
   '(("(python)Index" pydoc-info-lookup-transform-entry)
     ("(django14)Index" pydoc-info-lookup-transform-entry))))

;;; python-pydoc-info.el ends here
