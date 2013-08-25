;;; cperl-mode.el --- personalitzacio de perl mode
;;; (c) 2006 Alexis Roda
;;; $Id$

;;; Commentary:
;;;
;;; arxiu de configuracio del perl mode

;;; History:
;;;
;;; consultar log de svn

;;; Code:

(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)

(add-to-list 'auto-mode-alist '("\\.pl$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pm$" . cperl-mode))

(add-hook 'cperl-mode-hook
	  '(lambda()
         ;; activa el resaltat d'espai en blanc al final de línia
         (set-variable 'show-trailing-whitespace t)
	     (setq cperl-indent-level 4)
	     (setq cperl-hairy t)
	     (turn-on-font-lock)))



;;; cperl-mode.el ends here
