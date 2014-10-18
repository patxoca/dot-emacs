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

(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-map '[f7]  'next-error)
     (define-key c-mode-map '[f8]  'previous-error)
     (define-key c-mode-map '[f9]  'compile)))

(add-hook 'c-mode-hook
          (lambda ()
            (setq c-default-style "python")
            (setq c-basic-offset 4)
            ;; fontifica las cadenas de la forma @@[A-Z ]+: util
            ;; para resaltar observaciones
            (font-lock-add-keywords
             'c-mode
             '(("@@\\([A-Z ]+\\):" 1 font-lock-warning-face prepend)))))

;;; setup-c-mode.el ends here
