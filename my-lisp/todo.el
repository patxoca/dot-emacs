;;; todo.el --- utilitat per consultar recordatoris
;;; (c) 2005 Alexis Roda
;;; $Id$

;;; Commentary:

;;; Utilitats per afegir i consultar recordatoris inclosos dins
;;; arxius. Un recordatori no es mes que una etiqueta (utilitzada per
;;; identificar el tipus del recordatori) seguida d'un text arbitrari
;;; formant un paragraf.
;;;
;;;   @FOO: text arbitrari
;;;
;;; Normalment un recordatori apareixera dins un comentari, amb el(s)
;;; caracter(s) de comentari a l'esquerra. Per eliminar-los el que es
;;; fa es definir el 'prefix del recordatori' com el text que apareix
;;; a l'esquerra de l'etiqueta. El prefix es eliminat de totes les
;;; linees que formen el recordatori. El recordatori finalitza quan es
;;; troba una linea que no te el prefix, o be una linea que, una
;;; vegada eliminat el prefix, es una linea en blanc o formada sols
;;; per espais.
;;;
;;; Exemples:
;;;
;;;    # @TODO: exemple de recordatori d'una linea
;;;    #
;;;
;;; el prefix es ' # ', la segona linea es una linea en blanc (despres
;;; d'eliminar el prefix)
;;;
;;;    # @FIXME: un recordatori format per varies
;;;    # linees. Els espais en blanc i l'almoadilla
;;;    # son descartats en totes les linees.
;;;  # Aquesta linea no comença pel mateix prefix i
;;;  # per tant finalitza el recordatori.
;;;
;;; en aquest cas la cuarta linea no comença pel prefix i per tant
;;; finalitza el recordatori.
;;;
;;;    # es recomanda que @TODO: aparegue junt al caracter
;;;    # de comentari
;;;
;;; en aquest el prefix es '   # es recomana que ', una mala idea
;;;
;;; En C es podria fer:
;;;
;;;   /*
;;;    * @TODO: bla bla bli blo blu bla bla bli
;;;    * blo blu bla bla bli
;;;    */
;;;
;;;
;;;  Els punt d'entrada son les funcions:
;;;
;;;   * srctool-show-reminders : mostra recordatoris
;;;
;;;   * srctool-insert-reminder : insereix un recordatori
;;;
;;; El funcionament es pot personalitzar a traves del grup srctools
;;; (customize-group 'srctools)
;;;
;;; Es reserva el prefix C-c C-a C-r per les combinacions de tecles.
;;;
;;; Defineix globalment les tecles:
;;;
;;; C-c C-a C-r i srctool-insert-reminder
;;; C-c C-a C-r s srctool-show-reminders
;;;
;;; Defineix localment al buffer *TODO* les tecles:
;;;
;;; q delete-window

;;; History:
;;
;; $Log: $
;;

;;; @FIXME: 2005-11-30 : parse_block no detecta correctament el final
;;; d'un bloc:
;;;    ./wxop/schema/baseproperty.py:150:@TODO:
;;;    @todo: veure el To Do de L{IPropertyType.validate()} """ raise
;;;    NotImplementedError()
;;; en aquest cas el todo apareix dins un docstring que no esta
;;; separat de la primera linea de la funcio. En aquest comentari,
;;; p.e. no puc separar l'exemple de l'explicacio i al generar el
;;; informe es perd el format... cal una definicio mes flexible de
;;; bloc
;;;
;;; @TODO: 2005-12-01 12:51:33 : seria interessant que
;;; srctool-show-reminders preguntes les etiquetes en les que estem
;;; interessats per cada execucio (per defecte totes)
;;;
;;; @TODO: 2005-12-06 00:33:19 : a algunes funcions (privades) s'els
;;; passa el buffer i d'altres treballen sobre el buffer actual. No se
;;; si val la pena homogenizar-ho.

;;; Code:
(message "==================================================================")
(message "Inici arxiu todo.el")

(require 'pymacs)

(pymacs-load "srctools.todo")

(defgroup srctools
  nil
  "Utilitats que ajuden a gestionar el codi font"
  :group 'srctools)


(defun srctool-todo-set-labels (symbol value)
  "Cridada per custom per assignar un valor a `srctool-todo-labels', a
mes d'assignar el valor actualitza la taula de etiquetes al costat
python."
  ; @FIXME: alex 2006-08-14 23:45:20 : aqui passa algo que no entec,
  ; quan es cutomize-variable des de .emacs la variable rete el valor
  ; original
  (message (format "srctool-todo-set-labels %s %s" (prin1-to-string symbol)
                   (prin1-to-string value)))
  (set symbol value)
  (todo-init-labels value)
  value)


(defcustom srctool-todo-labels
  '("@TODO:" "@FIXME:")
  "Llista de les etiquetes reconegudes.
Poden ser expressions regulars reconegudes per python"
  :group 'srctools
  :set 'srctool-todo-set-labels
  :type '(repeat string))

(defcustom srctool-todo-label-prefix
  "@"
  "Prefix afegit per `srctool-insert-reminder' si no s'especifica."
  :group 'srctools
  :type 'string)

(defcustom srctool-todo-label-suffix
  ":"
  "Sufix afegit per `srctool-insert-reminder' si no s'especifica."
  :group 'srctools
  :type 'string)

(defcustom srctool-todo-ignore-extensions
  '("~" ".bak" ".pyc" ".o" ".a" ".elc")
  "Llista de les extensions a ignorar, cal incloure el ."
  :group 'srctools
  :type '(repeat string))

(defcustom srctool-todo-ignore-directories
  '(".emacs~" "CVS" "RCS" ".svn")
  "Llista dels directoris a ignorar."
  :group 'srctools
  :type '(repeat string))

(defcustom srctool-todo-show-reminders-creates-new-buffer
  nil
  "Amb un valor diferent de nil cada execucio de
srctool-show-reminders crea un nou buffer."
  :group 'srctools
  :type 'boolean)


(defface srctool-filename-face
  '((((type tty) (class color))
     (:foreground "red"
      :weight bold))
    (((type tty) (class mono))
     (:inverse-video t
      :weight bold))
    (((class color) (background dark))
     (:foreground "red"
      :weight bold))
    (((class color) (background light))
     (:foreground "red"
      :weight bold))
    (t (:background "gray"
        :weight bold)))
  "face per pintar el nom de l'arxiu."
  :group 'srctools)

(defface srctool-reminder-face
  '((((type tty) (class color))
     (:foreground "black"))
    (((type tty) (class mono))
     (:inverse-video nil))
    (((class color) (background dark))
     (:foreground "white"))
    (((class color) (background light))
     (:foreground "black"))
    (t (:background "gray")))
  "face per pintar el contingut del recordatori."
  :group 'srctools)

(defface srctool-highlight-filename-face
  '((((type tty) (class color))
     (:foreground "red"
      :background "LightBlue"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:foreground "red"
      :background "LightBlue"
      :weight bold))
    (((class color) (background light))
     (:foreground "red"
      :background "LightBlue"
      :weight bold))
    (t (:background "gray")))
  "face per pintar el nom de l'arxiu resaltat."
  :group 'srctools)

(defface srctool-highlight-reminder-face
  '((((type tty) (class color))
     (:foreground "black"
      :background "LightBlue"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:foreground "black"
      :background "LightBlue"))
    (((class color) (background light))
     (:foreground "black"
      :background "LightBlue"))
    (t (:background "gray")))
  "face per pintar el contingut del recordatori resaltat."
  :group 'srctools)




(defvar srctool-todo-keymap ()
  "Teclat emprat a la llista de recordatoris.")

(if srctool-todo-keymap
    ()
  (setq srctool-todo-keymap (make-sparse-keymap))
  (define-key srctool-todo-keymap [mouse-1] 'srctool-mouse-goto-reminder)
  (define-key srctool-todo-keymap [double-mouse-1] 'srctool-mouse-open-file-new-window)
  (define-key srctool-todo-keymap [mouse-2] 'srctool-mouse-open-file-new-window)
  (define-key srctool-todo-keymap [return] 'srctool-open-file-new-window)
  (define-key srctool-todo-keymap [tab] 'srctool-goto-next-reminder)
  (define-key srctool-todo-keymap [(shift iso-lefttab)] 'srctool-goto-previous-reminder)
  (define-key srctool-todo-keymap [down] 'srctool-goto-next-reminder)
  (define-key srctool-todo-keymap [up] 'srctool-goto-previous-reminder)
  (define-key srctool-todo-keymap [right] 'srctool-goto-next-reminder)
  (define-key srctool-todo-keymap [left] 'srctool-goto-previous-reminder)
  (define-key srctool-todo-keymap "q" 'delete-window)
  (define-key srctool-todo-keymap "d" 'debug-info))


;;; @FIXME: 2005-12-01 : pel moment no recordo com duplicar una
;;; llista, sembla que concatenant la llista buida s'aconssegueix
;;; l'efecte
(defvar insert-reminder-history (append srctool-todo-labels ())
  "Historic de recordatoris inserits.")

(defvar srctool-local-reminders-list '()
  "Vector de recordatoris. Local a cada buffer *TODO*. Les funcions
`srctool-reminfo-start', `srctool-reminfo-filename-end',
`srctool-reminfo-end', `srctool-reminfo-filename' i
`srctool-reminfo-lineno' permeten accedir als components de cada item
del vector.

IMPORTANT: els valor es guarden en ordre invers, els recordatoris del
final del buffer apareixen al principi del vector.")
(make-variable-buffer-local 'srctool-local-reminders-list)


(defsubst srctool-reminfo-start (reminfo)
  "Retorna el 'point' d'inici."
  (car reminfo))

(defsubst srctool-reminfo-filename-end (reminfo)
  "Retorna el 'point' al final del nom de l'arxiu."
  (nth 4 reminfo))

(defsubst srctool-reminfo-end (reminfo)
  "Retorna el 'point' de final."
  (cadr reminfo))

(defsubst srctool-reminfo-filename (reminfo)
  "Retorna el nom de l'arxiu."
  (nth 2 reminfo))

(defsubst srctool-reminfo-lineno (reminfo)
  "Retorna el nro. de linea."
  (nth 3 reminfo))


(defvar srctool-local-current-reminder nil
  "Index del recordatori seleccionat actualment, nil si no en hi ha
cap de seleccionat.")
(make-variable-buffer-local 'srctool-local-current-reminder)



(defun srctool-insert-reminder (label)
  "Insereix una marca de recordatori.

Insereix una marca de recordatori. El format de les marques es:

 '@<LABEL>: data hora :'.

La marca s'inclou dins un comentari.

El prefix (@) esta definit per la variable `srctool-todo-label-prefix'
i es afegit automaticament si LABEL no l'inclou.

El sufix (:) esta definit per la variable `srctool-todo-label-suffix'
i es afegit automaticament si LABEL no l'inclou.

Argument LABEL etiqueta utilitzada pel recordatori"
  (interactive
   (list (read-string "Recordatori: "
                      nil
                      '(insert-reminder-history . 0))))
  (setq label (upcase (s-trim label)))
  (if (not (s-starts-with-p srctool-todo-label-prefix  label))
      (setq label (concat srctool-todo-label-prefix label)))
  (if (not (s-ends-with-p srctool-todo-label-suffix label))
      (setq label (concat label srctool-todo-label-suffix)))
  (insert (or comment-start "")
          " " label " "
          (user-login-name) " "
          (format-time-string "%Y-%m-%d %H:%M:%S") " : "
          comment-end))


(defun srctool-show-reminders (directory)
  "Genera un llistat amb recordatoris.

Genera un llistat amb tots els recordatoris inclosos dins arxius
localitzats al directori DIRECTORY i als seus subdirectoris.  Els
recordatoris venen identificats per les etiquetes definides a la
variable `srctool-todo-labels'.  Els arxius amb una extensio vetada
per la llista `srctool-todo-ignore-extensions' son ignorats.  De la
mateixa manera s'ignoraran els directoris llistats a la variable
`srctool-todo-ignore-directories'.

Argument DIRECTORY nom del directori (string)."
  (interactive "DDirectori a escanejar:")
  (save-current-buffer
    (set-buffer (if srctool-todo-show-reminders-creates-new-buffer
                    (generate-new-buffer "*TODO*")
                  (get-buffer-create "*TODO*")))
    (buffer-disable-undo)
    (setq buffer-read-only nil)
    (erase-buffer)
    (use-local-map srctool-todo-keymap)
    (setq srctool-local-reminders-list nil); aço fa la variable local al buffer
    (let ((reminders (todo-file-walker (expand-file-name directory)
                                       'srctool-file-filter
                                       'srctool-dir-filter)))
      (dolist (reminder reminders nil)
        (let ((file  (aref reminder 0))
              (line  (aref reminder 1))
              (label (aref reminder 2))
              (text  (aref reminder 3))
              (start (point))
              endfile)
          ;; (setq file (expand-file-name file))
          (insert file)
          (setq endfile (point))
          (insert (format ":%s:%s\n%s\n\n" line label text))
          (setq srctool-local-reminders-list
                (cons (list start (point) file line endfile)
                      srctool-local-reminders-list)))))
    ;; converteix la llista en un vector, mes eficient per fer cerques
    (setq srctool-local-reminders-list
          (vconcat srctool-local-reminders-list))
    ;; "pinta" els recordatoris normal
    (dotimes (index (length srctool-local-reminders-list) nil)
      (srctool-hilite-reminder index nil))
    ;; activa el primer recordatori
    (srctool-private-goto-reminder (- (length srctool-local-reminders-list) 1))
    (goto-char (point-min))
    (buffer-enable-undo)
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (display-buffer (current-buffer))))

(defun srctool-goto-next-reminder ()
  "Salta al seguent recordatori."
  (interactive)
  (if (> srctool-local-current-reminder 0)
      (srctool-private-goto-reminder (- srctool-local-current-reminder 1))
    (ding)))

(defun srctool-goto-previous-reminder ()
  "Salta al recordatori anterior."
  (interactive)
  (if (< srctool-local-current-reminder
         (- (length srctool-local-reminders-list) 1))
      (srctool-private-goto-reminder (+ srctool-local-current-reminder 1))
    (ding)))


(defun srctool-mouse-goto-reminder (event)
  "Salta al recordatori sobre el que s'ha produit EVENT."
  (interactive "e")
  (let* ((ev          (event-start event))
         (todo-buffer (window-buffer (posn-window ev)))
         (the-point   (posn-point ev))
         (index       (srctool-get-reminder-index todo-buffer the-point)))
    (save-current-buffer
      (set-buffer todo-buffer)
      (srctool-private-goto-reminder index))))

(defun srctool-mouse-open-file-new-window (event)
  "Obre l'arxiu corresponent a un recordatori.

Obre l'arxiu on es troba un recordatori i salta a la linea
corresponent.

Argument EVENT event que ha provocat l'execucio de la funcio."
  (interactive "e")
  (let* ((ev          (event-start event))
         (todo-buffer (window-buffer (posn-window ev)))
         (the-point   (posn-point ev))
         (tmp         (srctool-get-filename-and-line todo-buffer the-point))
         (filename    (car tmp))
         (lineno      (cadr tmp)))
    (message "%s" ev)
    (let ((buffer (find-file-noselect filename)))
      (if (eq (current-buffer) todo-buffer)
          (switch-to-buffer-other-window buffer)
        (switch-to-buffer buffer))
      (goto-char (point-min))
      (forward-line (1- lineno)))))

(defun srctool-open-file-new-window ()
  "Obre l'arxiu corresponent a un recordatori.

Obre l'arxiu on es troba un recordatori i salta a la linea
corresponent."
  (interactive)
  (let* ((tmp         (srctool-get-filename-and-line (current-buffer) (point)))
         (filename    (car tmp))
         (lineno      (cadr tmp)))
    (let ((buffer (find-file-noselect filename)))
      (switch-to-buffer-other-window buffer)
      (goto-char (point-min))
      (forward-line (1- lineno)))))


;;; definicio de tecles
(global-set-key [(control c) (control a) (control r) (i)] 'srctool-insert-reminder)
(global-set-key [(control c) (control a) (control r) (s)] 'srctool-show-reminders)



;;; ======================================================================
;;;
;;;                              PRIVAT
;;;
;;; Nota: les funcions que no tenen com a primer argument un buffer
;;; operen sobre el buffer actual

(defun srctool-private-goto-reminder (index)
  "Salta al recordatori especificat per INDEX (un enter entre 0 i
'nombre-recordatoris menys 1') i el resalta. Si hi ha un recordatori
resaltat el fica 'normal' abans de resaltar el nou. Si INDEX esta fora
de limits no fa res."
  (if (and (>= index 0)
           (< index (length srctool-local-reminders-list)))
      (progn
        (if srctool-local-current-reminder
            (srctool-hilite-reminder srctool-local-current-reminder nil))
        (let ((pm (aref srctool-local-reminders-list index)))
          (srctool-hilite-reminder index t)
          (setq srctool-local-current-reminder index)
          (goto-char (srctool-reminfo-start pm))))))

(defun srctool-get-filename-and-line (todo-buffer the-point)
  "Obte el nom de l'arxiu i el nro de linea.

Obte el nom de l'arxiu i el nro de linea corresponents al recordatori
que conte la posicio THE-POINT dins el buffer TODO-BUFFER. Si els
troba retorna una parella (nom-arxiu nro-linea), sino retorna la
llista buida."
  (let ((index (srctool-get-reminder-index todo-buffer the-point)))
    (if (>= index 0)
        (save-excursion
          (set-buffer todo-buffer)
          (let ((val (aref srctool-local-reminders-list index)))
            (list (srctool-reminfo-filename val)
                  (srctool-reminfo-lineno val))))
      ())))

(defun srctool-get-reminder-index (todo-buffer the-point)
  "Obte el index del recordatori dins la taula local.

Obte el index del recordatori dins la taula local corresponent al
recordatori que conte la posicio THE-POINT dins el buffer
TODO-BUFFER. Si no el troba retorna -1."
  (save-excursion
    (set-buffer todo-buffer)
    (let ((a 0)
          (b (- (length srctool-local-reminders-list) 1))
          (found nil)
          m
          pm)
      (while (and (not found)
                  (<= a b))
        (setq m (/ (+ a b) 2))
        (setq pm (aref srctool-local-reminders-list m))
        (cond
         ;; COMPTE el vector esta en ordre descendent
         ((>= the-point (srctool-reminfo-end pm))
          (setq b (- m 1)))
         ((< the-point (srctool-reminfo-start pm))
          (setq a (+ m 1)))
         (t
          (setq found t))))
      (if found
          m
        -1))))

(defun srctool-file-filter (dirname filename)
  "Callback utilitzat per la funcio todo-file-walker per filtrar els
arxius a processar."
  ;; probablement hi ha una manera mes "lispica" de fer-ho
  (let ((found nil)
        (list srctool-todo-ignore-extensions))
    (while (and (not found)
                list)
      (if (s-ends-with-p (car list) filename)
          (setq found t))
      (setq list (cdr list)))
    (not found)))

(defun srctool-dir-filter (dirname filename)
  "Callback utilitzat per la funcio todo-file-walker per filtrar els
directoris a processar."
  (not (member filename srctool-todo-ignore-directories)))


(defun srctool-hilite-reminder (index hilite)
  "Resalta el recordatori especificat per INDEX. Si HILITE 'es cert'
resalta el recordatori, sino el fica normal."
  (save-excursion
    ;;(set-buffer (get-buffer "*TODO*"))
    (setq buffer-read-only nil)
    (let* (oldbg
           (reminder (aref srctool-local-reminders-list index))
           (start    (srctool-reminfo-start reminder))
           (end      (srctool-reminfo-end reminder))
           (endfile  (srctool-reminfo-filename-end reminder)))
      (if hilite
          (progn
            (put-text-property start endfile
                               'face 'srctool-highlight-filename-face)
            (put-text-property endfile end
                               'face 'srctool-highlight-reminder-face))
        (put-text-property start endfile
                           'face 'srctool-filename-face)
        (put-text-property endfile end
                           'face 'srctool-reminder-face)))
;;         (set-text-properties start endfile
;;                              '(face srctool-filename-face
;;                                mouse-face highlight
;;                                help-echo  "mouse-2: visit this file and line"))))
    (setq buffer-read-only t)))

;; @FIXME: alex 2007-08-10 14:29:14 : idealment el resaltat de les
;; etiquetes es tindria que activar automaticament en tots el
;; modos. Pel momento no funciona, si tan sols executant la funcio
;; srctool-labels-font-locking des del hook dels modos.
;;
;; assignacio d'una face a les etiquetes
;; (defun srctool-labels-font-locking ()
;;   (font-lock-add-keywords
;;    nil
;;    (list
;;     (cons (concat "\\<\\(@" (regexp-opt srctool-todo-labels) "\\):")
;; 	  '(1 'font-lock-warning-face prepend))
;;     )))
;;
;; sols per emacs >= 22
;; (add-hook 'after-change-major-mode-hook 'srctool-labels-font-locking)
;;
;; aço tindria que funcionar amb emacs 21 (deixant de banda que no
;; controlo si s'afegeix mes d'una vegada) pero no funciona
;;
;; (defadvice font-lock-mode (before srctool-labels-font-locking-advice (&optional arg))
;;   "Defineix ..."
;;   (message "font-lock-mode advice %s" arg)
;;   (srctool-labels-font-locking))
;;
;; (ad-activate 'font-lock-mode)

(defun debug-info ()
  ""
  (interactive)
  (message "buffer: %s" (buffer-name))
  (message "  reminder-list: %s" srctool-local-reminders-list))


(provide 'todo)

(message "Final arxiu todo.el")
(message "==================================================================")

;;; todo.el ends here
