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

(defvar emacs-startup-dir "~/emacslib"
  "Directori on l'usuari guarda el codi elisp.")

(defvar instance-name (let ((instance-name (getenv "EMACS_INSTANCE"))
                            (sys-name (system-name)))
                        (if instance-name
                            instance-name
                          (substring sys-name 0 (string-match "\\." sys-name))))
  "Nom de la instancia.")

;; aquest el carreguem abans per poder utilitzar algunes funcions
(load (concat emacs-startup-dir "/utils"))
(load (concat emacs-startup-dir "/compat"))



;; defineix l'arxiu de customització basant-se en la instancia
(setq custom-file (concat emacs-startup-dir "/"
                          "instances/"
                          instance-name "/"
                          "customize.el"))


(add-to-list 'load-path
             (concat emacs-startup-dir "/site-lisp"))
(add-to-list 'load-path
             (concat emacs-startup-dir "/site-lisp/pylookup"))
(add-to-list 'load-path
             (concat emacs-startup-dir "/my-lisp"))

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
(startup-load-directory-in-order "instances/common/init.d")
(startup-load-directory-in-order (concat "instances/"
                                         instance-name
                                         "/init.d"))

;;; carrega les customitzacions de la instancia
(startup-load-file custom-file)

;;; si la instancia defineix codi de postinicialitzacio el carrega
(let ((postload (concat emacs-startup-dir "/"
                        "instances/"
                        instance-name "/"
                        "postload.el")))
  (if (file-readable-p postload)
      (load postload))
)

;;; Nota: per llistar les fonts disponibles:
;;;
;;; (prin1-to-string (x-list-fonts "*"))
;;; despres es poden fer uns pocs replace-regexp al resultat
;;;  * <espai>\\" -> ^Q^J(set-face-font 'default "
;;;  * \\" -> ")
;;; i obtindrem un forma senzilla de provar l'apariencia de les fonts


;;; font antigua
;;; '(default ((t (:stipple nil :background "AntiqueWhite" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 88 :width normal :family "adobe-courier"))))

;;; elimina efecte 3D a la mode line
;;(set-face-attribute 'mode-line nil :box nil)





;;; Activa autoguardado de sesiones
;;; Debe estar tras cargar todos los archivos

;;; @FIXME: 2005-11-30 : pymacs-load no funciona be si no s'executa
;;; cap al final de la inicialitzacio d'emacs, apareix un missatge
;;; "Emacs: Object vanished when helper was killed."
;;; Al final ha resultat una incompatibilitat entre desktop i pymacs

;; (load-library "desktop")
;; (desktop-load-default)
;; (desktop-read)





(message "******************************************************************")
(message "********************** Fi de l'arxiu .emacs **********************")
(message "******************************************************************")

;;; punt_emacs.el ends here
