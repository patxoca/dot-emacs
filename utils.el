;;; utils.el --- funcions d'utilitat general
;;; (c) 1998 - 2005 Alexis Roda
;;; $Id$

;;; Commentary: funcions d'utilitat general utilitzades per altres
;;; moduls.  Aquest modul sols depen de funcionalitat subministrades
;;; per ell mateix o per emacs

;;; History:
;; consultar svn

;;; Code:

;; funcions utilitzades durant l'arranc

(defun startup-load-file (filename)
  "Carrega un arxiu.

Si 'filename' no es una ruta absoluta es considera relativa a
'emacs-startup-dir'."
  (let ((fullname (if (file-name-absolute-p filename)
                      filename
                    (concat emacs-startup-dir "/" filename))))
    (load (concat emacs-startup-dir "/" fullname))))

(defun startup-load-directory-in-order (dirname)
  "Carrega els arxius .el d'un directori en ordre lexicogràfic."
  (let* ((fullname (if (file-name-absolute-p dirname)
                       dirname
                     (concat emacs-startup-dir "/" dirname)))
         (arxius   (mapcar
                    'file-name-sans-extension
                    (directory-files fullname t "\.elc\?$" nil))))
    (mapc 'startup-load-file arxius)))


;; http://www.emacswiki.org/emacs/FullScreen#toc17
(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))


;; possiblement moltes d'aquestes funcions es podrien moure a algun
;; altre lloc

(defun unix-to-dos ()
  "Converteix un arxiu a format DOS."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))


(defun dos-to-unix ()
  "Converteix un arxiu a format DOS."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix))


;; dos exemples de Xah Lee que permeten navegar facilment entre els
;; buffers d'usuari
;; http://xahlee.org/emacs/elisp_examples.html
(defun xah-next-user-buffer ()
  "Switch to the next user buffer in cyclic order.\n
User buffers are those not starting with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer in cyclic order.\n
User buffers are those not starting with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))


;; versions "millorades" de funcions estàndard

(defun arv-region-or-symbol-at-point ()
  "Devuelve la region o simbolo sobre el que esta situado el cursor.
Si la marca esta activa devuelve la region y desactiva la marca, sino
devuelve el simbolo sobre el que esta situado el cursor."
  (if mark-active
      (prog1
          (buffer-substring-no-properties (region-beginning) (region-end))
        (deactivate-mark))
    (thing-at-point 'symbol)))

(defun arv-query-replace ()
  "Similar a `query-replace', pero ofrece un valor inicial.
Version simplificada de `query-replace' que ofrece como valor inicial
para el texto a sustituir la region (si esta activa) o el simbolo
situado bajo el cursor. A diferencia de `query-replace' la sustitucion
se realiza en todo el buffer."
  (interactive)
  (let* ((old-string (read-string "Replace: " (arv-region-or-symbol-at-point)))
         (new-string (read-string (concat "Replace " old-string " with: ") "")))
    (save-excursion
      (goto-char (point-min))
      (query-replace old-string new-string))))

(defun arv-kill-ring-save-word-at-point ()
  "Copia un text en el kill ring. El seu comportament varia
depenent de si la marca està o no activa:

* si està activa delega en `kill-ring-save' i es copia el text
  seleccionat.

* sinó es copia el símbol sobre el que està situat el cursor."
  (interactive)
    (if mark-active
        (call-interactively 'kill-ring-save)
      (kill-new (thing-at-point 'symbol))))


;; substitut per find-grep-dired

(require 'find-dired)

(defvar arv-find-grep-ignore-dirs
  '(".svn")
  "Llista de directoris ignorats en la cerca.")

(defvar arv-find-grep-ignore-extensions
  '(".pyc" "~" ".elc")
  "Llista d'extensions ignorades en la cerca.")

(defun arv-find-grep-build-params ()
  "Retorna una cadena amb els arguments per find."
  (let ((value '()))
    (dolist (item arv-find-grep-ignore-dirs)
      (setq value (cons (concat "\\( ! -name " item " -o -prune \\)")
                        value)))
    (dolist (item arv-find-grep-ignore-extensions)
      (setq value (cons (concat "! -name \\*" item )
                        value)))
    (mapconcat 'identity value " ")))

(defun arv-find-grep-dired (dir args)
  "Funcio semblant a `find-grep-dired`, pero permet filtrar els arxius
processats.

La variable `arv-find-grep-ignore-dirs` conte una llista amb noms de
directoris a ignorar. El contingut d'estos directoris sera ignorat
completament.

La variable `arv-find-grep-ignore-extensions` conte una llista amb
terminacions de noms d'arxius que seran ignorades. Cal incloure
explicitament el punt en cas de ser necessari."
  (interactive "DFind-grep (directory): \nsFind-grep (grep regexp): ")
  (find-dired dir
	      (concat (arv-find-grep-build-params) " -type f -exec grep " find-grep-options " "
		      args " {} \\\; ")))


;; funcions utilitzades principalment als snippets

(defun arv-uncamelize (text sep)
  "Retorna text sense 'camelitzar'.

Si 'text' es 'CamelCase' i 'sep' es '-' retorna 'camel-case'.

Es suposa que 'text' es un identificador valid escrit en
CamelCase. 'sep' es un string.
"
  (mapconcat 'downcase (s-split-words text) sep))

(defun arv-substring (text start end)
  "Retorna un substring.

Esta versio de substring s'ajusta als limits i, a diferencia de
substring, no produeix cap error si es sobrepassen.
"
  (substring text (max 0 start) (min end (length text))))

;; Toggle window dedication
;; http://stackoverflow.com/questions/5151620
;;
;; He afegit un indicador a la modeline per ressaltar les finestres
;; dedicades. El problema és que la modeline és una característica del
;; buffer, no de la finestra i per tant l'indicador es mostrarà en
;; qualsevol finestra, sigui o no dedicada, que mostri el buffer. En
;; la pràctica, almenys per l'us que hi tinc pensat, no crec que
;; suposi un problema.
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       (progn
         (setq mode-line-format (append mode-line-format '("[D]")))
         "Window '%s' is dedicated")
     (setq mode-line-format (remove "[D]" mode-line-format))
     "Window '%s' is normal")
   (current-buffer)))


;;; utils.el ends here
