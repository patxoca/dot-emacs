;;; setup-c-mode.el --- customization for c-mode

;; $Id$


;;; Commentary:
;;

;;; Local keybindings:
;;
;; f9        : compile
;; f7        : next error
;; mayus-f7  : previous error
;;
;; fontifica las cadenas de la forma @@[A-Z ]+:

;;; History:
;;


;;; Code:

(autoload 'company-complete "company" "" t nil)

(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-base-map '[f7]  'next-error)
     (define-key c-mode-base-map '[f8]  'previous-error)
     (define-key c-mode-base-map '[f9]  'compile)

     (add-hook 'c-mode-common-hook
               (lambda ()
                 (setq indent-tabs-mode nil)
                 (setq c-default-style "python")
                 (setq c-basic-offset 4)
                 ;; company-mode
                 (company-mode t)
                 ;; fontifica las cadenas de la forma @@[A-Z ]+: util
                 ;; para resaltar observaciones
                 (font-lock-add-keywords
                  'c-mode
                  '(("@@\\([A-Z ]+\\):" 1 font-lock-warning-face prepend)))))))

;;; setup-c-mode.el ends here
