;;; arv-py.el --- Funcions d'utilitat per treballar amb codi python

;; $Id;$

;;; Commentary:
;; 

(require 'python)

;;; Code:

(defgroup arv-py nil
  "Extensions per python mode."
  :group 'python
  :version "24.3")

;;; Private helpers

(defun arv-py--is-toplevel-object-p ()
  "Determina si es tracta d'una definició de nivell superior.

Avalua a t si el punt es troba en una línia que conté una
element de nivell més alt.  Esta pensada per ser cridada al
començament de blocs (if, def, class ...).  En altes contexts pot
compotar-se de manera estranya."
  (save-excursion
    (let ((start (progn
		   (beginning-of-line)
		   (point)))
	  (indent (progn
		    (back-to-indentation)
		    (point))))
      (= start indent))))

(defun arv-py--info-looking-at-beginning-of-class ()
  "Determina si es tracta de la definició d'una classe.

Avalua a t si el punt es troba en una línia que conté la paraula
reservada `class' en el context de la definició d'una classe."
  (and
   (python-info-looking-at-beginning-of-defun)
   (save-excursion
     (back-to-indentation)
     (looking-at "class[[:space:]]+"))))

(defun arv-py--info-looking-at-beginning-of-def ()
  "Determina si es tracta de la definició d'una funció/mètode.

Avalua a t si el punt es troba en una línia que conté la paraula
reservada `def' en el context de la definició d'una
funció/mètode."
  (and
   (python-info-looking-at-beginning-of-defun)
   (save-excursion
     (back-to-indentation)
     (looking-at "def[[:space:]]+"))))


;;; Navigation

(defun arv-py-nav-beginning-of-class ()
  "Mou el punt al principi de la classe."
  (while (and (not (arv-py--info-looking-at-beginning-of-class))
	      (not (arv-py--is-toplevel-object-p)))
    (python-nav-beginning-of-defun 1)))

(defun arv-py-nav-beginning-of-def ()
  "Mou el punt al principi de la funció/mètode."
  (while (and (not (arv-py--info-looking-at-beginning-of-def))
	      (not (arv-py--is-toplevel-object-p)))
    (python-nav-beginning-of-defun 1)))


;;; Query program structure

(defun arv-py-info-enclosing-def-name ()
  "Determina el nom de la funció dins la que es troba el punt.

Retorna una cadena amb el nom de la funció més interna dins al
que es troba el punt o nil si el punt no es troba dins de cap
funció."
  (let ((result nil))
    (save-excursion
      (arv-py-nav-beginning-of-def)
      (back-to-indentation)
      (if (looking-at "def +\\(\\w+\\)\\>" )
        (setq result (match-string 1))))
    result))

(defun arv-py-info-enclosing-class-name ()
  "Determina el nom de la classe dins la que es troba el punt.

Retorna una cadena amb el nom de la classe més interna dins la
que es troba el punt o nil si el punt no es troba dins de cap
classe."
  (let ((result nil))
    (save-excursion
      (arv-py-nav-beginning-of-class)
      (back-to-indentation)
      (if (looking-at "class +\\(\\w+\\)\\>" )
        (setq result (match-string 1))))
    result))


;;; outline-mode

(defun arv-py-outline-level ()
  "Retorna el outline-level corresponent a elements d'un programa
python. És una prova de concepte per jugar amb outline (que te
certes limitacions.)"
  (interactive)
  (let ((level (- (match-end 0) (match-beginning 0) (length (match-string 1)))))
    (if (member (match-string 1) python-indent-dedenters)
        (+ level (/ python-indent-offset 2))
      level)))


;;; Assorted

(defcustom arv-py-electric-colon-insert-newline t
  "No-nil indica a `arv-py-electric-colon' que al inserir un :
cal inserir un salt de línia i sagnar la nova línia."
  :group 'arv-py
  :type  'boolean
  :safe  'booleanp)


(defun arv-py-electric-colon (arg)
  "Insereix un salt de línia després de :.

Aquesta funció amplia `python-indent-electric-colon' inserint un
salt de línia i sagnant la nova línia.

La comprovació de quan cal inserir-i-sagnar només té en compte
casos molt simples. S'anirà afinant sobre la marxa."
  (interactive "*P")
  (python-indent-electric-colon arg)
  (when arv-py-electric-colon-insert-newline
    (let ((bol (save-excursion
                 (beginning-of-line)
                 (point))))
      (if (and
           (eolp)
           (not (python-syntax-comment-or-string-p))
           (not (looking-back "\\[[^]]*" bol))
           (not (looking-back "{[^}]*" bol)))
          (newline-and-indent)))))

(provide 'arv-py)

;;; arv-py.el ends here
