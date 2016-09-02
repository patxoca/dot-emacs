;;; arv-python-django.el --- utilitats per python-django

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
(require 'python-django)
(require 's)
(require 'thingatpt)

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
  "Determina si `filename' correspon a un arxiu dins alguna de
les aplicacions del projecte. En cas afirmatiu retorna el nom de
la aplicació, nil en cas contrari.

L'ordre en que s'examinen les aplicacions no està definit."
  (let ((tpl-dir (file-name-directory (expand-file-name filename)))
        (project-buffer (arv/django-get-project-buffer)))
    (message "belongs-to-app: %s" tpl-dir)
    (if project-buffer
        (with-current-buffer project-buffer
          (car
           (-first
            (lambda (app) (s-starts-with-p (cdr app) tpl-dir))
            (python-django-info-get-app-paths)))))))

(defun arv/django-get-current-app ()
  "Retorna el nom de l'aplicació que conté el buffer
actiu. Retorna nil si el buffer no pertany a cap aplicació o no
s'ha carregat cap projecte django."
  (arv/django-file-belongs-to-app-p buffer-file-name))

(defun arv/django-get-app-names ()
  ""
  (with-current-buffer (arv/django-get-project-buffer)
    (sort (python-django-info-get-installed-apps) 'string-lessp)))

(defun arv/django--get-app-path (app-name)
  ""
    (with-current-buffer (arv/django-get-project-buffer)
      (python-django-info-get-app-path app-name)))

(defun arv/django-get-project-root ()
  (with-current-buffer (arv/django-get-project-buffer)
    python-django-project-root))

;;;###autoload
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

(defun arv-get-string-at-point ()
  "Retorna la cadena en el punt, ni si el punt no està sobre una
cadena."
  (if (in-string-p)
      (let ((start (save-excursion (while (in-string-p) (forward-char -1))
                                   (1+ (point))))
            (end  (save-excursion (while (in-string-p) (forward-char 1))
                                  (1- (point)))))
        (buffer-substring-no-properties start end))))

;;;###autoload
(defun arv/django-jump-to-template ()
  "Visita la plantilla en el punt.

Cal que:

* el punt es trobi sobre una cadena. La ruta de la plantilla es
  determina pel valor de la cadena sobre la que es troba el punt.

* la ruta de la plantilla sigui relativa al directori 'templates'
  de l'aplicació (seguint el conveni django).

* s'hagi carregat un projecte django per poder fer introspecció
  de les aplicacions.

La funció opera contruint una llista amb les aplicacions que
contenen la plantilla. Per simplificar la creació de plantilles
aquesta llista sempre contindrà el nom de l'aplicació
actual (l'aplicació que conté l'arxiu des del que s'ha cridat a
la funció) independenment de que contingui la plantilla. Si
aquesta llista només conté una aplicació (l'actual) s'obre la
plantilla directament (creant-la si és necessari). Si conté més
d'una aplicació permet triar quina obrir."
  (interactive)
  (let ((filename (arv-get-string-at-point))
        (project-buffer (arv/django-get-project-buffer))
        (current-app (arv/django-get-current-app))
        (candidates ()))
    (cond
     ((null filename)
      (message "Point must be over an string."))
     ((null project-buffer)
      (message "No open django project."))
     (t
      (progn
        (with-current-buffer project-buffer
          (dolist (app (python-django-info-get-app-paths))
            (let ((filename-full
                   (concat
                    (file-name-as-directory (cdr app))
                    (file-name-as-directory "templates")
                    filename)))
              (if (or (equal (car app) current-app)
                      (file-exists-p filename-full))
                  (setq candidates (cons (cons (symbol-name (car app)) filename-full) candidates))))))
        (find-file (cdr (assoc
                         (if (= (length candidates) 1)
                             (caar candidates)
                           (completing-read
                            "Choose app: "
                            candidates nil t nil))
                         candidates))))))))

;;;###autoload
(defun arv/django-insert-template-name ()
  "Insereix el nom de la plantilla.

El nom es calcula a partir del nom de la app actual i el nom del
buffer, sense extensió."
  (interactive)
  (let ((name (pyx/get-current-package-name)))
    (insert name
            "/"
            (file-name-sans-extension (file-name-base (buffer-file-name)))
            ".html")))

;;;###autoload
(defun arv/django-autopair-template-tag ()
  "Facilita introduir blocs '{% %}'."
  (interactive "")
  (let ((within-block (save-excursion
                        (backward-char)
                        (looking-at "{"))))
    (insert "%")
    (when within-block
      (insert "  %")
      (backward-char 2))))

(defun arv/django--ido-select-app ()
  (ido-completing-read "App: " (arv/django-get-app-names) nil t))


(defun arv/django--visit-file (dir-rel-path at-app-root)
  (let* ((app-name (arv/django--ido-select-app))
         (app-root (arv/django--get-app-path app-name)))
    (if at-app-root
        (setq app-root (file-name-directory app-root)))
    (setq app-root (concat app-root "/" dir-rel-path))
    (ido-file-internal ido-default-file-method nil app-root))
  )

(defun arv/django-visit-app ()
  "Permet selecionar app i obrir un arxiu dins l'arrel de la app."
  (interactive)
  (arv/django--visit-file "." nil))

(defun arv/django-visit-app-test-module ()
  "Permet selecionar app i obrir un arxiu de test."
  (interactive)
  (arv/django--visit-file "tests" nil))

(defun arv/django-visit-app-view-module ()
  "Permet selecionar app i obrir un arxiu de views."
  (interactive)
  (arv/django--visit-file "views" nil))

(defun arv/django-visit-app-template-file ()
  "Permet selecionar app i obrir un arxiu de template."
  (interactive)
  (arv/django--visit-file "tests" nil))

(defun arv/django-visit-project ()
  ""
  (interactive)
  (ido-file-internal ido-default-file-method nil (arv/django-get-project-root)))


(defvar arv/django-mode-map (make-sparse-keymap "arv/django-mode") "arv/django-mode keymap")

(defun arv/django-mode-setup-keymap ()
  "Setup a default keymap."
  (define-key arv/django-mode-map (kbd "C-c d v a") 'arv/django-visit-app)
  (define-key arv/django-mode-map (kbd "C-c d v t") 'arv/django-visit-app-test-module)
  (define-key arv/django-mode-map (kbd "C-c d v v") 'arv/django-visit-app-view-module)
  (define-key arv/django-mode-map (kbd "C-c d v T") 'arv/django-visit-app-template-file)
  (define-key arv/django-mode-map (kbd "C-c d v p") 'arv/django-visit-project)
)

(define-minor-mode arv/django-mode
  "Minor mode for working with django." nil " arv/django" arv/django-mode-map
  (arv/django-mode-setup-keymap))


(provide 'arv-python-django)

;;; 'arv-python-django.el ends here
