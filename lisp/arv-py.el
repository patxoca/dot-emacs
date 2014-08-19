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

(defun arv-py--info-looking-at-import ()
  "Determina si el punt està sobre una instrucció import. Retorna
`t' en cas afirmatiu.

La comprovació és molt senzilla i assumeix que la sintaxi és
correcta: la línia conté la paraula `import' i no forma part de
una cadena ni de un comentari."
  (save-excursion
    (beginning-of-line)
    (and  (let ((case-fold-search nil))
                (search-forward-regexp "\\<import\\>" (line-end-position) t))
         (not (python-syntax-comment-or-string-p)))))

(defun arv-py--info-looking-at-comment-only-line ()
  "Determina si el punt està sobre una línia formada únicament
per un comentari."
  (save-excursion
   (beginning-of-line)
   (not (not (search-forward-regexp "^[[:space:]]*#.*$" (line-end-position) t)))))

(defun arv-py--info-looking-at-empty-line ()
  "Determina si el punt està sobre una línia en blanc."
  (save-excursion
   (beginning-of-line)
   (not (not (search-forward-regexp "^[[:space:]]*$" (line-end-position) t)))))


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

;;; D'acord amb el PEP8 els import s'agrupen en tres blocs: imports
;;; del sistema (path, sys ...), de mòduls de tercers i de
;;; l'aplicació. Els blocs es separen amb línies en blanc.

(defun arv-py-nav-goto-first-import ()
  "Mou el punt al començament del primer 'import' dins el buffer
actual. Guarda el punt anterior en el `mark-ring'. Si no troba
cap import deixa el punt al mateix lloc.

Retorna la posició del punt si s'ha trobat cap 'import' o `nil'
en cas contrari."
  (interactive)
  (let ((case-fold-search nil)
        (matched nil))
    (save-excursion
      (goto-char (point-min))
      (while (and
              (setq matched (search-forward-regexp "\\<import\\>" nil t))
              (python-syntax-comment-or-string-p))))
    (when matched
      (push-mark (point) t nil)
      (goto-char matched)
      (beginning-of-line)
      (point))))

(defun arv-py-nav-goto-next-import (traverse-group)
  "Mou el punt al següent import."
  (let ((matched (save-excursion
                   (while (progn
                            (forward-line)
                            (and (not (eobp))
                                 (or (arv-py--info-looking-at-comment-only-line)
                                     (and traverse-group
                                          (arv-py--info-looking-at-empty-line))))))
                   (if (arv-py--info-looking-at-import)
                       (point)))))
    (when matched
      (goto-char matched)
      (beginning-of-line)
      (point))))

(defun arv-py-nav-goto-last-import ()
  "Mou el punt al final de l'últim import."
  (interactive)
  (while (arv-py-nav-goto-next-import t))
  (end-of-line))

(defun arv-py-nav-goto-last-import-in-block ()
  "Mou el punt al final de l'últim import del bloc de imports
actual. Cal que el punt estigui sobre una sentència `import'."
  (interactive)
  (while (arv-py-nav-goto-next-import nil))
  (end-of-line))

(defun arv-py--read-import-string (default)
  "
from foo import bar
bar
bar.baz

import bar
bar
bar.baz

import foo as bar
bar
bar.baz

import bar.baz
bar.baz
bar.baz.spam

from foo import spam as bar
bar
bar.baz
"
  ;; demanar import: a triar entre `(car (string-split default "\\."))' i `default'
  ;;                 alternativament qualsevol identificador (pels alias)
  ;;
  ;; si import te almenys un punt utilitzar "import xxx.yyy"
  ;; sino
  ;;    demanar from: per defecte default menys ultim component si
  ;;                  coincideix amb import
  ;;    si import != (car default) demanar alias?

  (let* ((parts (split-string default "\\."))
         (import-candidate (car parts))
         import-name
         module-name
         alias-name)
    (setq import-name (completing-read "What to import? " (delete-dups (list default import-candidate))))
    (if (string-match-p "\\." import-name)
        (format "import %s" import-name)
      (setq module-name (read-string (format "import %s from? " import-name)))
      (if (string= import-candidate import-name)
          (format "from %s import %s" module-name import-name)
        (format "from %s import %s as %s" module-name import-name import-candidate)))))
;;      (format "import %s" (mapconcat 'identity (nbutlast parts 1) ".")))))

(defun arv-py-insert-import ()
  ""
  (interactive)
  (let ((import-statement (arv-py--read-import-string (python-info-current-symbol))))
    (save-excursion
      (arv-py-nav-goto-first-import)
      (arv-py-nav-goto-last-import)
      (newline-and-indent)
      (insert import-statement))))


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
           (not (looking-back "{[^}]*" bol))
           (not (looking-back "lambda.*" bol)))
          (newline-and-indent)))))

(provide 'arv-py)

;;; arv-py.el ends here
