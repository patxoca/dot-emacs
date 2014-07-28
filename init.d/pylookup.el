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
              (let ((pylookup-dir (arv/startup-get-absolute-path "site-lisp/pylookup")))
                (setq pylookup-program (arv/path-join pylookup-dir "pylookup.py"))
                (setq pylookup-db-file (arv/path-join pylookup-dir "pylookup.db"))
                (local-set-key [(control c) (h)] 'pylookup-lookup))))

;;; pylookup.el ends here
