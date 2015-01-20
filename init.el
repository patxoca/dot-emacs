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




(message "    ___ _ __ ___   __ _  ___ ___")
(message "   / _ \\ '_ ` _ \\ / _` |/ __/ __|")
(message "  |  __/ | | | | | (_| | (__\\__ \\")
(message " (_)___|_| |_| |_|\\__,_|\\___|___/")


(require 'cl-lib)


;; variables

(defvar emacs-startup-dir "~/.emacs.d/conf.d"
  "Directori on l'usuari guarda el codi elisp.")

(defvar arv/load-path '("site-lisp" "site-lisp/pylookup"
                        "site-lisp/eproject" "lisp"
                        "../site-lisp/pyx")
  "Directoris addicionals, relatius a `emacs-startup-dir', que
s'inclouran en `load-path'")


;; funcions auxiliars

(defun arv/path-join (&rest parts)
  (concat
   (mapconcat 'file-name-as-directory (butlast parts) "")
   (car (last parts))))

(defun arv/startup-get-absolute-path (filename)
  (if (file-name-absolute-p filename)
      filename
    (arv/path-join emacs-startup-dir filename)))

(defun arv/startup-get-instance-name ()
  (let ((candidate (or
                    (getenv "EMACS_INSTANCE")
                    (car (split-string (system-name) "\\.")))))
    (if (file-accessible-directory-p (arv/startup-get-path-in-instance "" candidate))
        candidate
      "default")))

(defun arv/startup-get-path-in-instance (filename &optional instance)
  (let ((instance-name (or instance (arv/startup-get-instance-name))))
    (arv/startup-get-absolute-path
     (arv/path-join "instances" instance-name filename))))

(defun arv/startup-load-directory-in-order (dirname)
  "Carrega els arxius .el d'un directori en ordre lexicogràfic."
  (let* ((fullname (arv/startup-get-absolute-path dirname))
         (arxius   (mapcar
                    'file-name-sans-extension
                    (directory-files fullname t "\.elc\?$" nil))))
    (mapc (lambda (x) (load (arv/startup-get-absolute-path x))) arxius)))

(defun arv/startup-byte-recompile ()
  ""
  (interactive)
  (dolist (dir arv/load-path)
    (let ((abs-dir (arv/startup-get-absolute-path dir)))
      (message abs-dir)
      (let ((generated-autoload-file (arv/path-join abs-dir "loaddefs.el")))
        (update-directory-autoloads abs-dir))
      (byte-recompile-directory abs-dir 0)))
  (dolist (dir (list "init.d" (arv/startup-get-path-in-instance "init.d" "common")))
    (message (arv/startup-get-absolute-path dir))
    (byte-recompile-directory (arv/startup-get-absolute-path dir) 0)))


;; funcions per la inicialització

(defun arv/startup-configure-load-path ()
  (dolist (dir arv/load-path)
    (add-to-list 'load-path (arv/startup-get-absolute-path dir))))

(defun arv/startup-configure-custom-file ()
  (setq custom-file (arv/startup-get-path-in-instance "customize.el"))
  (load custom-file))

(defun arv/startup-package-initialize ()
  (if (and (not (fboundp 'package-initialize))
           (file-readable-p (expand-file-name "~/.emacs.d/elpa/package.el")))
      (load
       (expand-file-name "~/.emacs.d/elpa/package.el")))
  (setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa stable" . "http://melpa-stable.milkbox.net/packages/")))
  (package-initialize))

(defun arv/startup-load-autoloads ()
  (dolist (dir arv/load-path)
    (load-file (arv/startup-get-absolute-path (arv/path-join dir "loaddefs.el")))))

(defun arv/startup-initialize-instance ()
  (arv/startup-load-directory-in-order (arv/startup-get-path-in-instance "init.d" "common"))
  (arv/startup-load-directory-in-order (arv/startup-get-path-in-instance "init.d")))


;; altres

(defun arv/load-org-dev-version ()
  ""
  (interactive)
  (add-to-list 'load-path "/home/alex/prog/emacs/org/org-mode/lisp")
  (load "/home/alex/prog/emacs/org/org-mode/lisp/org-loaddefs")
  (require 'org)
  (message "Loaded org %s" (org-version)))


;; inicialització
(defun arv/emacs-startup ()
  "Starts emacs normally."
  (arv/startup-configure-load-path)
  (arv/startup-configure-custom-file)
  (arv/startup-package-initialize)
  (arv/startup-load-autoloads)
  (arv/startup-initialize-instance))

(defun arv/emacs-install ()
  "Install this configuration dowloading required packages and
bytecompiling and generating autoloads."
  (arv/startup-configure-load-path)
  (arv/startup-package-initialize)
  (when (load-library "ensure-installed-packages")
    (arv/ensure-required-packages)
    (arv/startup-byte-recompile)))

(if (getenv "EMACS_INSTALL")
    (arv/emacs-install)
  (arv/emacs-startup))

;;; codi afegit per emacs per activar comandes "perilloses" per usuaris nous
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)


(message "*** end of .emacs ************************************************")

;;; punt_emacs.el ends here
