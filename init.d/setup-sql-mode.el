;;; sql-mode.el --- personalitzacio del sql-mode
;;; $Id$

;;; Commentary:
;;

;;; History:
;;

;;; Code:

(add-hook 'sql-mode-hook
          (lambda ()
            (message "sql-mode-hook")
            ;; activa el resaltat d'espai en blanc al final de línia
            (set-variable 'show-trailing-whitespace t)
            (sql-highlight-postgres-keywords)))


;;; sql-mode.el ends here
