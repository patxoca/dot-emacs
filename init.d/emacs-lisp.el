;;; emacs-lisp.el --- configuració de emacs-lisp

;; $Id$


;;; Commentary:
;;


;;; History:
;;


;;; Code:



(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (local-set-key '[f9] (lambda ()
                                     (interactive)
                                     (ert t)))))

;;; 'emacs-lisp.el ends here
