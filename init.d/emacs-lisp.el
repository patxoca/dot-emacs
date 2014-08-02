;;; emacs-lisp.el --- configuració de emacs-lisp

;; $Id$


;;; Commentary:
;;


;;; History:
;;


;;; Code:


(autoload 'company-complete "company" "" t nil)


(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "C-<right>") 'right-word)
     (define-key paredit-mode-map (kbd "C-<left>") 'left-word)
     (define-key paredit-mode-map (kbd "M-<right>") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-<left>") 'paredit-forward-barf-sexp)))

(eval-after-load "lisp-mode"
  '(progn
     (define-key emacs-lisp-mode-map (kbd "s-SPC") 'company-complete)
     (define-key emacs-lisp-mode-map '[f9] (lambda () (interactive) (ert t)))))

(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (company-mode t)
              (eldoc-mode t)
              (paredit-mode 1)
              (elisp-slime-nav-mode 1)
              (pretty-lambda-mode t)
              ))

;;; 'emacs-lisp.el ends here
