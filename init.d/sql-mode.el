;;; sql-mode.el --- personalitzacio del sql-mode
;;; $Id$

;;; Commentary:
;;
;; personalitza SQL mode per a que utilitzi els keywords d'Oracle al
;; resaltar la sintaxis.  Curiosament s'adapta millor a postgres que
;; la definicio corresponent a postgres (imagino que fa temps que no
;; s'actualitza

;;; History:
;;

;;; Code:

(require 'sql)
(add-hook 'sql-mode-hook
          (lambda ()
            (message "sql-mode-hook")
            ;; activa el resaltat d'espai en blanc al final de línia
            (set-variable 'show-trailing-whitespace t)
            (sql-highlight-oracle-keywords)))


;;; sql-mode.el ends here
