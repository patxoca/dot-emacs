;;; sphinx-doc.el --- funcions d'utilitat per treballar amb sphinx

;; $Id$


;;; Commentary:
;;
;; Aquest modul implementa funcions per facilitar el treball amb
;; documentació processada amb sphix.
;;
;; Generació de la sortida:
;;
;; Les funcions per compilar el projecte operen amb el Makefile
;; generat per sphinx.  El Makefile es busca en el directori actual
;; (on està l'arxiu del buffer actual) i en els directoris pare.
;;
;; arv-sphinx-build-latexpdf: make latexpdf
;;
;; arv-sphinx-build-html: make html


;;; History:
;;


;;; Code:


;;; funcions per compilar el document

(defun -arv-sphinx-locate-makefile ()
  "Busca l'arxiu Makefile començant en el directori on està el
buffer actual i visitant els directoris pare.

Si troba el Makefile retorna la ruta absoluta del directori que
el conte. Retorna nil si no el troba."
  (let ((cwd (file-name-directory (expand-file-name (buffer-file-name)))))
    (while (and (not (string= cwd "/"))
                (not (file-exists-p (concat cwd "Makefile"))))
      (setq cwd (file-name-directory (directory-file-name cwd))))
    (if (string= cwd "/")
        nil
      cwd)))

(defun -arv-sphinx-build (target)
  "Intenta executar 'make <target>'"
  (let ((makefile (-arv-sphinx-locate-makefile)))
    (unless (null makefile)
      (compile (format "make -k -C %s %s" makefile target)))))

(defun arv-sphinx-build-latexpdf ()
   "Genera PDF"
   (interactive)
   (-arv-sphinx-build "latexpdf"))

(defun arv-sphinx-build-html ()
   "Genera HTML"
   (interactive)
   (-arv-sphinx-build "html"))

(provide 'sphinx-doc)

;;; sphinx-doc.el ends here
