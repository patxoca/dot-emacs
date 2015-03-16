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

(defun arv-py-nav-goto-first-import ()
  "Mou el punt al començament del primer 'import' guardant el
punt anterior en el `mark-ring'. Si no troba cap import deixa el
punt al mateix lloc.

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
      (beginning-of-line))))


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

;; (defun arv-py-outline-level ()
;;   "Retorna el outline-level corresponent a elements d'un programa
;; python. És una prova de concepte per jugar amb outline (que te
;; certes limitacions.)"
;;   (interactive)
;;   (let ((level (- (match-end 0) (match-beginning 0) (length (match-string 1)))))
;;     (if (member (match-string 1) python-indent-dedenters)
;;         (+ level (/ python-indent-offset 2))
;;       level)))


(defun arv/py-visit-setup-py ()
  ""
  (interactive)
  (let ((parent (locate-dominating-file (buffer-file-name) "setup.py")))
    (if (not parent)
        (message "'setup.py' not found")
      (find-file (concat (file-name-as-directory parent) "setup.py")))))

(defun arv/py-get-current-package-name ()
  "Return the current package name.

The package name is the name of the directory that contains the
'setup.py'. If no 'setup.py' is found nil is returned."
  (interactive)
  (let ((parent (locate-dominating-file (buffer-file-name) "setup.py")))
    (if (null parent)
        nil
      (file-name-base (directory-file-name parent)))))

(defun arv/py-insert-current-package-name ()
  "Insert the current package name.

The package name is the name of the directory that contains the
'setup.py'."
  (interactive)
  (let ((name (arv/py-get-current-package-name)))
    (if (null name)
        (error "'setup.py' not found")
      (insert name))))


(provide 'arv-py)

;;; arv-py.el ends here
