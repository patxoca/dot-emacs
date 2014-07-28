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




(message "    ___ _ __ ___   __ _  ___ ___")
(message "   / _ \\ '_ ` _ \\ / _` |/ __/ __|")
(message "  |  __/ | | | | | (_| | (__\\__ \\")
(message " (_)___|_| |_| |_|\\__,_|\\___|___/")


(defun arv/get-instance-name ()
  (or
   (getenv "EMACS_INSTANCE")
   (let ((sys-name (system-name)))
     (substring sys-name 0 (string-match "\\." sys-name)))))


;; variables

(defvar emacs-startup-dir "~/emacslib"
  "Directori on l'usuari guarda el codi elisp.")

(defvar instance-name (arv/get-instance-name)
  "Nom de la instancia.")

(defvar arv/load-path '("site-lisp" "site-lisp/pylookup"
                        "site-lisp/eproject" "my-lisp")
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

(defun arv/startup-get-path-in-instance (filename &optional instance)
  (let ((instance-name (or instance instance-name)))
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
    (message (arv/startup-get-absolute-path dir))
    (byte-recompile-directory (arv/startup-get-absolute-path dir) 0))
  (dolist (dir (list "init.d" (arv/startup-get-path-in-instance "init.d" "common")))
    (message (arv/startup-get-absolute-path dir))
    (byte-recompile-directory (arv/startup-get-absolute-path dir) 0)))

(defun -package-p (name)
  "Return t if NAME is an available package."
  (unless package-archive-contents
    (package-refresh-contents))
  (not (null (assoc name package-archive-contents))))

(defun arv/require-package (pkg)
  (when (and (-package-p pkg)
             (not (package-installed-p pkg)))
    (package-install pkg)))


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
  (package-initialize))

(defun arv/startup-initialize-instance ()
  (arv/startup-load-directory-in-order (arv/startup-get-path-in-instance "init.d" "common"))
  (arv/startup-load-directory-in-order (arv/startup-get-path-in-instance "init.d"))
  (let ((postload (arv/startup-get-path-in-instance "postload.el")))
    (if (file-readable-p postload)
        (load postload))))


;; inicialització

(arv/startup-configure-load-path)
(arv/startup-configure-custom-file)
(arv/startup-package-initialize)
(arv/startup-initialize-instance)

;;; codi afegit per emacs per activar comandes "perilloses" per usuaris nous
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)


(message "*** end of .emacs ************************************************")

;;; punt_emacs.el ends here
