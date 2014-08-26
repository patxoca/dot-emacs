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

(defun arv-query-replace (&optional arg)
  "Replacement for `query-replace'.

* It proposes the symbol at point as the initial value for the
  search string.

* If the region is active it contraints the replacement,
  otherwise operate on the whole buffer.

* Without prefix argument performs `query-replace'.

* With C-u performs `replace-string'.

In any case point is preserved."
  (interactive "*P")
  (let* ((old-string (read-string "Replace: " (thing-at-point 'symbol)))
         (new-string (read-string (concat "Replace " old-string " with: ") ""))
         (start (if mark-active (min (mark) (point)) (point-min)))
         (end   (if mark-active (max (mark) (point)) (point-max))))
    (save-excursion
      (if (not arg)
          (query-replace old-string new-string nil start end)
        (goto-char start)
        (while (search-forward old-string end t)
          (replace-match new-string))))))

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


(defun arv/generate-random-uuid ()
  (with-temp-buffer
    (shell-command "uuidgen -r" t)
    (beginning-of-buffer)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

;;; utils.el ends here
