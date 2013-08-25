;;; zope.el --- definicions utils per treballar amb zope
;;; (c) 2006 Alexis Roda
;;; $Id$

;;; Commentary:
;;


;;; History:
;;

;;; Code:

;; associa alguns tipus d'arxius utilitzats amb zope al mode mes
;; adient

(require 'python)

(add-to-list 'auto-mode-alist '("\\.pt$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.cpt$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.zpt$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vpy$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.cpy$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.css.dtml$" . css-mode))

;; (add-to-list 'auto-mode-alist '("\\.dtml$" . html-mode))
;; (add-to-list 'auto-mode-alist '("\\.zsql$" . sql-mode))


;;; zope.el ends here
