;;; -*- coding: utf-8; lexical-binding: t -*-

;;; init.el --- arxiu de configuracio d'emacs
;;; (c) 1998-2020 Alexis Roda
;;; $Id$

;;; Commentary: arxiu de configuracio d'Emacs.

;; Aquest arxiu prepara l'entorn per executar la configuració que hi ha
;; en "settings.org".


;; La inicialització del sistema de paquets es fa en "settings.org".
;; Aquesta "inicialització" comentada l'ha afegida "package.el" i és
;; necessari mantindre-la per evitar que torni a afegir-la.

;; (package-initialize)

(message "    ___ _ __ ___   __ _  ___ ___")
(message "   / _ \\ '_ ` _ \\ / _` |/ __/ __|")
(message "  |  __/ | | | | | (_| | (__\\__ \\")
(message " (_)___|_| |_| |_|\\__,_|\\___|___/")
(message " ")


;; accelera la càrrega

(let ((threshold gc-cons-threshold)
      (percentage gc-cons-percentage))
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6)
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-threshold threshold
                    gc-cons-percentage percentage)
              (garbage-collect))
            t))

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

;; NOTE: el script "scripts/update_conf_from_svn" és solapa amb la
;; funcionalitat següent. Si es fa cap canvi ací caldrà reflectir-lo
;; allí.

(let ((settings-org (arv/path-concat arv/emacs-conf-dir "settings.org"))
      (settings-sh (arv/path-concat arv/emacs-conf-dir "settings.sh")))

  ;; executa els blocs de codi emacs-lisp de "settings.org",
  ;; extraient-los prèviament si és necessari.
  (org-babel-load-file settings-org)

  ;; si cal genera l'arxiu "settings.sh" i l'executa
  (unless (file-exists-p settings-sh)
    (org-babel-tangle-file settings-org settings-sh "sh")
    (chmod settings-sh #o700)
    (start-process "env-setup" "*env-setup*" "/bin/bash" "-c" settings-sh)
    (switch-to-buffer "*env-setup*")))


(message "*** end of .emacs ************************************************")

;;; punt_emacs.el ends here
