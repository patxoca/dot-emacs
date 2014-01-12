;;; python-django.el --- inicialització per python-django

;; $Id$


;;; Commentary:
;;
;; Aquest mòdul integra diferents eines per facilitar el treball amb
;; django:
;;
;; * python-django: ofereix una gestió de projecte
;;
;; * pony-tpl: la part de pony-mode encarregada de les plantilles
;;
;; Actualment la forma recomanada de treballar és:
;;
;; * cada projecte django utilitza un virtualenv independent.
;;
;; * la ruta de l'arrel del projecte i el nom del mòdul de settings
;;   arriben a emacs mitjançant les variables d'entorn
;;   `DJANGO_PROJECT' i `DJANGO_SETTINGS_MODULE' respectivament,
;;   definides al activar el virtualenv, per exemple.
;;
;; * en tot moment només pot haver un projecte obert. Canviar de
;;   projecte requereix tancar emacs, activar el nou virtualenv i
;;   obrir emacs.
;;
;; Si les necessitats canvien miraré si és possible canviar de
;; virtualenv en calent o com tindre varis projectes django obert
;; simultàniament.

;;; History:
;;


;;; Code:

(require 'dash)
(require 'pony-tpl)
(require 'python-django)
(require 's)

;; funcions de suport

(defun arv/django-open-default-project ()
  "Obre el projecte django definit per les variables d'entorn
`DJANGO_PROJECT' i `DJANGO_SETTINGS_MODULE', si estan
definides. En cas contrari demana a l'usuari les dades.

Si ja hi ha un projecte obert no té cap efecte.

Retorna el buffer del projecte.

La idea és que el hook `postactivate' de virtualenvwrapper
defineixi aquestes variables amb el valor adient. Aquesta solució
m'agrada més que utilitzar variables locals de directori."
  (interactive)
  (or (arv/django-get-project-buffer)
      (if (getenv "DJANGO_PROJECT")
          (python-django-open-project (getenv "DJANGO_PROJECT")
                                      (getenv "DJANGO_SETTINGS_MODULE"))
        (call-interactively 'python-django-open-project))))

(defun arv/django-get-project-buffer ()
  "Retorna el buffer del projecte o nil si no hi ha cap projecte
obert. Assumeix que hi ha UN ÚNIC PROJECTE obert."
  (-first (lambda (buf)
            (with-current-buffer buf
              (eq major-mode 'python-django-mode)))
          (buffer-list)))

(defun arv/django-file-belongs-to-app-p (filename)
  "Retorna t si `filename' correspon a un arxiu dins alguna de
les aplicacions del projecte."
  (let ((tpl-dir (file-name-directory (expand-file-name filename)))
        (project-buffer (arv/django-get-project-buffer)))
    (if project-buffer
        (with-current-buffer project-buffer
          (-any-p
           (lambda (app) (s-starts-with-p (cdr app) tpl-dir))
           (python-django-info-get-app-paths))))))

(defun arv/django-switch-to-project-buffer ()
  "Canvia el focus a la finestra que te el projecte django. Si
cap finestra mostra el projecte el mostra en la finestra actual,
obrint-lo si cal."
  (interactive)
  (let* ((project-buffer (or (arv/django-get-project-buffer)
                             (arv/django-open-default-project)))
         (project-window (get-buffer-window project-buffer)))
    (if project-window
        (select-window project-window)
      (switch-to-buffer project-buffer))))


;; configuració dels modes involucrats

(eval-after-load "python-django"
  '(progn
     (define-key python-django-mode-map (kbd "<left>") 'python-django-ui-move-up-tree)))

(eval-after-load "sgml-mode"
  '(progn
     (add-hook 'html-mode-hook
               #'(lambda ()
                   (when (arv/django-file-belongs-to-app-p buffer-file-name)
                     (pony-tpl-mode))))))

;;; 'python-django.el ends here
