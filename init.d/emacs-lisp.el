;;; emacs-lisp.el --- configuració de emacs-lisp

;; $Id$


;;; Commentary:
;;


;;; History:
;;


;;; Code:



(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              ;; company-mode (complete anything)
              (company-mode t)
              (eldoc-mode t)
              (local-set-key (kbd "s-SPC") 'company-complete)
              (paredit-mode 1)
              (elisp-slime-nav-mode 1)
              (local-set-key '[f9] (lambda ()
                                     (interactive)
                                     (ert t)))))

;;; 'emacs-lisp.el ends here
