;;; -*- encoding: utf-8 -*-

;;; punt_emacs.el --- arxiu de configuracio d'emacs
;;; (c) 1998-2005 Alexis Roda
;;; $Id$

;;; Commentary: arxiu de configuracio d'Emacs, l'arxiu .emacs es un
;;; enllaç simbolic a aquest

;;; Per facilitar mantindre una única configuració d'emacs per tots
;;; els sistemes en que l'utilitzo de manera més o menys intensiva he
;;; optat per utilitzar un sistema semblant als scripts d'arranc del
;;; SystemV.


;;; Code:

(message "******************************************************************")
(message "******************** Inici de l'archiu .emacs ********************")
(message "******************************************************************")

(defun arv/path-join (&rest parts)
  (concat
   (mapconcat 'file-name-as-directory (butlast parts) "")
   (car (last parts))))

(defun arv/startup-get-absolute-path (filename)
  (if (file-name-absolute-p filename)
      filename
    (arv/path-join emacs-startup-dir filename)))

(defun arv/startup-load-directory-in-order (dirname)
  "Carrega els arxius .el d'un directori en ordre lexicogràfic."
  (let* ((fullname (arv/startup-get-absolute-path dirname))
         (arxius   (mapcar
                    'file-name-sans-extension
                    (directory-files fullname t "\.elc\?$" nil))))
    (mapc (lambda (x) (load (arv/startup-get-absolute-path x))) arxius)))

(defun arv/get-instance-name ()
  (let ((instance-name (getenv "EMACS_INSTANCE"))
        (sys-name (system-name)))
    (if instance-name
        instance-name
      (substring sys-name 0 (string-match "\\." sys-name)))))


(defvar emacs-startup-dir "~/emacslib"
  "Directori on l'usuari guarda el codi elisp.")

(defvar instance-name (arv/get-instance-name)
  "Nom de la instancia.")


(dolist (dir '("site-lisp" "site-lisp/pylookup" "my-lisp"))
  (add-to-list 'load-path (arv/startup-get-absolute-path dir)))


;; aquest el carreguem abans per poder utilitzar algunes funcions
(load (arv/startup-get-absolute-path "utils"))
(load (arv/startup-get-absolute-path "compat"))


;; defineix l'arxiu de customització basant-se en la instancia
(setq custom-file
      (arv/startup-get-absolute-path
       (arv/path-join "instances" instance-name "customize.el")))


;;; @NOTE: alex 2011-12-24 13:08:20 : no se que pinta açò, no recordo
;;; haver-ho afegit però podria ser. M'he fixat que existia quan vaig
;;; instal·lar ELPA però no sé si estan relacionats.
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)


;;; inicialitza ELPA si està instal·lat
(if (and (not (fboundp 'package-initialize))
         (file-readable-p (expand-file-name "~/.emacs.d/elpa/package.el")))
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el")))
(if (fboundp 'package-initialize)
  (package-initialize))


;;; carrega el codi d'inicialitzacio comú i específic de la instancia
(arv/startup-load-directory-in-order "instances/common/init.d")
(arv/startup-load-directory-in-order (arv/path-join "instances" instance-name "init.d"))

;;; carrega les customitzacions de la instancia
(load custom-file)

;;; si la instancia defineix codi de postinicialitzacio el carrega
(let ((postload (arv/path-join emacs-startup-dir
                        "instances"
                        instance-name
                        "postload.el")))
  (if (file-readable-p postload)
      (load postload))
)





(message "******************************************************************")
(message "********************** Fi de l'arxiu .emacs **********************")
(message "******************************************************************")

;;; punt_emacs.el ends here
