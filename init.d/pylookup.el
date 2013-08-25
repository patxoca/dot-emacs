;;; pylookup.el --- configuracio de pylookup
;;; $Id$

;;; Commentary:
;;


;;; History:
;;

;;; Code:

(autoload 'pylookup-lookup "pylookup")
(autoload 'pylookup-update "pylookup")

(add-hook 'python-mode-hook
          #'(lambda()
              (let ((pylookup-dir (concat emacs-startup-dir "/site-lisp/pylookup")))
                (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
                (setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))
                (local-set-key [(control c) (h)] 'pylookup-lookup))))

;;; pylookup.el ends here
