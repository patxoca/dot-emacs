;;; -*- encoding: utf-8 -*-

;;; init.el --- arxiu de configuracio d'emacs
;;; (c) 1998-2005 Alexis Roda
;;; $Id$

;;; Commentary: arxiu de configuracio d'Emacs, l'arxiu .emacs es un
;;; enllaç simbolic a aquest

;;; Per facilitar mantindre una única configuració d'emacs per tots
;;; els sistemes en que l'utilitzo de manera més o menys intensiva he
;;; optat per utilitzar un sistema semblant als scripts d'arranc del
;;; SystemV.

;;; Variables d'entorn:
;;
;;  - EMACS_INSTANCE: nom de la instànica a arrancar. Si s'omet
;;    utilitza el nom del host.
;;
;;  - EMACS_INSTALL: si està definida, no importa el valor, executa
;;    emacs en mode instal·lació: descarrega i instal·la paquets,
;;    compila i genera autoloads per la configuració.

;;; Code:



;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(message "    ___ _ __ ___   __ _  ___ ___")
(message "   / _ \\ '_ ` _ \\ / _` |/ __/ __|")
(message "  |  __/ | | | | | (_| | (__\\__ \\")
(message " (_)___|_| |_| |_|\\__,_|\\___|___/")
(message " ")

(defun arv/path-concat (&rest components)
  (concat
   (mapconcat 'file-name-as-directory (butlast components) "")
   (car (last components))))

(defconst arv/emacs-conf-dir
  (or
   (getenv "EMACS_CONFIG_DIR")
   (arv/path-concat user-emacs-directory "conf.d"))
  "Directori de configuració d'emacs.")

;; L'orde d'aquests dos és important per garantir que es carrega la
;; versió correcta de org. En cas que la versió local no estigui
;; disponible s'utilitzarà la incorporada en emacs.
;;
;; settings.org inclou instruccions (blocs de codi avaluables) per
;; instal·lar ord des del codi font.

(let ((org-path "~/.local/share/emacs/site-lisp/org"))
  (when (file-directory-p org-path)
    (add-to-list 'load-path org-path)))
(require 'org)

;; Ací tenim el problema de l'ou i la gallina, la configuració està en
;; org i pot configurar org. Veure la respota de wasamasa en
;; https://emacs.stackexchange.com/questions/3143/can-i-use-org-mode-to-structure-my-emacs-or-other-el-configuration-file
;; si es vol processar la configuració sense haver de carregar org
;; prèviament.

(let ((settings-org (arv/path-concat arv/emacs-conf-dir "settings.org"))
      (settings-sh (arv/path-concat arv/emacs-conf-dir "settings.sh"))
      (execute-settings-sh nil))

  ;; si cal genera l'arxiu "settings.sh"
  (unless (file-exists-p settings-sh)
    (org-babel-tangle-file settings-org settings-sh "sh")
    (chmod settings-sh #o700)
    (setq execute-settings-sh t))

  ;; executa els blocs de codi emacs-lisp de "settings.org",
  ;; extraient-los prèviament si és necessari.
  (org-babel-load-file settings-org)

  ;; si cal executa "settings.sh"
  (when execute-settings-sh
    (start-process "env-setup" "*env-setup*" settings-sh)
    (switch-to-buffer "*env-setup*")))


(message "*** end of .emacs ************************************************")

;;; punt_emacs.el ends here
