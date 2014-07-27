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

(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "C-<right>") 'right-word)
     (define-key paredit-mode-map (kbd "C-<left>") 'left-word)
     (define-key paredit-mode-map (kbd "M-<right>") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-<left>") 'paredit-forward-barf-sexp)))

(eval-after-load "lisp-mode"
  '(progn
     (local-set-key (kbd "s-SPC") 'company-complete)
     (local-set-key '[f9] (lambda () (interactive) (ert t)))))

(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              ;; company-mode (complete anything)
              (company-mode t)
              (eldoc-mode t)
              (paredit-mode 1)
              (elisp-slime-nav-mode 1)
              (pretty-lambda-mode t)
              ))

;;; 'emacs-lisp.el ends here
