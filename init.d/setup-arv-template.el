;;; arv-template.el --- arv-template initialization
;;; $Id$

;;; Commentary:
;;


;;; History:
;;


;;; Code:

(arv/template-setup)
(eval-after-load "yasnippet"
  '(progn
     (add-to-list 'yas-snippet-dirs
                  (arv/startup-get-absolute-path "shared/yasnippets/headers"))))


;;; arv-template.el ends here
