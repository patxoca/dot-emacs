;;; javascript-mode.el --- personalitzacio del javascript mode
;;; (c) 2006 Alexis Roda
;;; $Id$

;;; Commentary:
;;;
;;; Arxiu de configuracio de javascript-mode


;;; History:
;;;
;;; consultar log de svn

;;; Code:

(autoload 'javascript-mode "javascript" "JavaScript editing mode." t)

(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))

(add-hook 'javascript-mode-hook
          #'(lambda ()
              ;; activa el resaltat d'espai en blanc al final de línia
              (set-variable 'show-trailing-whitespace t)
              ))

;;; javascript-mode.el ends here
