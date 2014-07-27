;;; emacs-lisp.el --- configuració de emacs-lisp

;; $Id$


;;; Commentary:
;;


;;; History:
;;


;;; Code:


(require 'company)
(require 'eldoc)
(require 'elisp-slime-nav)
(require 'ert)
(require 'lisp-mode)
(require 'paredit)
(require 'pretty-lambdada)

(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              ;; company-mode (complete anything)
              (company-mode t)
              (eldoc-mode t)
              (local-set-key (kbd "s-SPC") 'company-complete)
              (paredit-mode 1)
              (elisp-slime-nav-mode 1)
              (when (require 'pretty-lambdada nil 'noerror)
                (pretty-lambda-mode t))
              (local-set-key '[f9] (lambda ()
                                     (interactive)
                                     (ert t)))))

;;; 'emacs-lisp.el ends here
