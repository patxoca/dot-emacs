;;; tex-mode.el --- personalitzacio latex mode
;;; $Id$

;;; Commentary:
;;


;;; History:
;;

;;; Code:

(require 'tex-site)
(require 'latex)

(load "preview-latex.el" nil t t)

;;; el hook no s'executa, cal investigar-ho

(add-hook 'latex-mode-hook
          (lambda ()
            ;; activa el resaltat d'espai en blanc al final de línia
            (set-variable 'show-trailing-whitespace t)
            (message "latex hook")
            (auto-fill-mode)))


;;; tex-mode.el ends here
