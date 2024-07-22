;;; arv-rst.el --- Extensions per rst-mode  -*- lexical-binding: t; -*-

;; Author: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Maintainer: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Version: 0,1,0
;; Created: 2024-07-21
;; Package-Requires:

;;; Install:

;; Afegir a l'arxiu de configuració d'Emacs:
;;
;;   (use-package arv-rst
;;     :ensure nil
;;     :load-path "conf.d/site-lisp"
;;   )

;;; Commentary:

;; Funcions variades que faciliten treballar amb documents rst.
;;
;; Manipulació de capçaleres:
;;
;; - arv/rst-underline-header
;; - arv/rst-promote-header-at-point
;; - arv/rst-demote-header-at-point
;;
;; Caràcters intel·ligents:
;;
;; - arv/rst-smart-grave
;; - arv/rst-smart-asterisk

;;; Hacking:

;; Internament les capçaleres es representen amb una cadena de
;; longitud 2. El primer caràcter correspon al caràcter utilitzar en
;; la overline (un espai en blanc si no hi ha overline) i el segon
;; correspon a la underline. L'absència de capçalera es representa amb
;; la capçalera nula, una cadena formada per dos espais.

;;
;;; Code:

(require 'rst)

(defvar arv/rst-header-recommended-adornments
  '("##" "**" " =" " -" " ^" " ~" " \"" " #" " *" " +")
  "Parelles de caràcters que es poden utilitzar per adornar les
seccions. El primer és l'utilitzat en la overline i el segon en
la underline. En el cas de la overline un espai en blanc indica
que no hi ha overline.")

(defconst arv/-rst-null-header "  ")

;;; ----------------------------------------------------------------------
;;;
;;; Funcions auxiliars generals

(defun arv/-rst-current-line-length ()
  (- (pos-eol) (pos-bol)))

(defun arv/-rst-get-previous-line ()
  (if (= (pos-bol) (point-min))
      ""
    (save-excursion
      (previous-line)
      (buffer-substring-no-properties (pos-bol) (pos-eol)))))

(defun arv/-rst-get-next-line ()
  (if (= (pos-eol) (point-max))
      ""
    (save-excursion
      (next-line)
      (buffer-substring-no-properties (pos-bol) (pos-eol)))))

(defun arv/-rst-after-role-p ()
  "Return t if point is after a role."
  (looking-back ":\\w+:" (line-beginning-position)))

(defun arv/-rst-header-get-header-level (header)
  "Retorna el valor numèric de la capçalera HEADER.

Retorna el nivell de la capçalera, comença a comptar en 1.
Retorna -1 si HEADER no correspon a cap de les capçaleres
enumerades en la variable arv/rst-header-recommended-adornments."
  (let ((level (cl-position header arv/rst-header-recommended-adornments :test #'equal)))
    (if (null level)
        -1
      (1+ level))))

(defun arv/-rst-header-get-promoted-header (header)
  "Retorna la capçalera resultant de promoure HEADER.

Retorna la capçalera prèvia a HEADER, segons l'ordre definit per
arv/rst-header-recommended-adornments.

Si HEADER no és una capçalera declarada en
arv/rst-header-recommended-adornments retorna la capçalera nula.

Si HEADER no es pot promoure retorna HEADER."
  (let ((level (arv/-rst-header-get-header-level header)))
    (cond
     ((= level -1) arv/-rst-null-header) ; capçalera no reconeguda
     ((= level 1) header)  ; capçalera de nivel màxim, no es pot promoure més
     (t (nth (- level 2) arv/rst-header-recommended-adornments)))))

(defun arv/-rst-header-get-demoted-header (header)
  "Retorna la capçalera resultant de degradar HEADER.

Retorna la capçalera posterior a HEADER, segons l'ordre definit
per arv/rst-header-recommended-adornments.

Si HEADER no és una capçalera declarada en
arv/rst-header-recommended-adornments retorna la capçalera nula.

Si HEADER no es pot degradar retorna HEADER."
  (let ((level (arv/-rst-header-get-header-level header)))
    (cond
     ((= level -1) arv/-rst-null-header) ; capçalera no reconeguda
     ((= level (length arv/rst-header-recommended-adornments)) header)
                                        ; capçalera de nivel mínim, no
                                        ; es pot degradar més
     (t (nth level arv/rst-header-recommended-adornments)))))

(defun arv/-rst-header-is-header-adornment-p (text)
  "Comprova si TEXT és una decoració en potència.

TEXT es considera una decoració en potència si és una cadena no
buida en que tots els caràcters son iguals."
  (if (string-empty-p text)
      nil
    (string= text (make-string (length text) (string-to-char text)))))

(defun arv/-rst-header-get-header-at-point ()
  "Retorna la capçalera actual.

Retorna la capçalera sobre la que es troba el cursor o la
capçalera nula si no estem en una capçalera.

Espera que el cursor estigui en la línia del text del títol, no
en la decoració."
  (let ((curr-line-length (arv/-rst-current-line-length))
        (prev-line (arv/-rst-get-previous-line))
        (next-line (arv/-rst-get-next-line))
        (overline-char " ")
        (underline-char " "))
    (when (> curr-line-length 0)
      (if (and (>= (length prev-line) curr-line-length)
               (arv/-rst-header-is-header-adornment-p prev-line))
          (setq overline-char (substring prev-line 0 1)))
      (if (and (>= (length next-line) curr-line-length)
               (arv/-rst-header-is-header-adornment-p next-line))
          (setq underline-char (substring next-line 0 1))))
    (if (or (string= overline-char " ")
            (string= overline-char underline-char))
        (concat overline-char underline-char)
      arv/-rst-null-header)))

(defun arv/-rst-header-empty-overline-p (header)
  (string= (substring header 0 1) " "))

(defun arv/-rst-null-header-p (header)
  (string= header arv/-rst-null-header))

(defun arv/-rst-header-update-header (header-length old-header new-header)
  "Actualitza la capçalera de OLD-HEADER a NEW-HEADER.

HEADER-LENGTH determina la longitud de la capçalera quan
s'insereix de cap de nou."
  (unless (or (arv/-rst-null-header-p new-header)
              (string= old-header new-header))

    ;; overline.
    ;; la overline és una mica més complicada ja que la decoració pot
    ;; apareixer quan es promou i desapareixer quan es degrada.
    (if (arv/-rst-header-empty-overline-p old-header)
        (unless (arv/-rst-header-empty-overline-p new-header)
          ;; abans no tenia overline i ara sí, cal inserir-la
          (save-excursion
            (previous-line)
            (newline)
            (insert (make-string header-length (aref new-header 0)))))
      (if (arv/-rst-header-empty-overline-p new-header)
          ;; abans tenia capçalera i ara no, cal esborrar-la
          (save-excursion
            (previous-line)
            (delete-region (pos-bol) (pos-eol))
            (delete-char 1))
        ;; abans tenia overline i ara també, cal actualitzar-la
        (save-excursion
          (previous-line)
          (delete-region (pos-bol) (pos-eol))
          (insert (make-string header-length (aref new-header 0))))))

    ;; underline
    ;; la underline sempre s'actualitza, no desapareix mai
    (save-excursion
      (next-line)
      (delete-region (pos-bol) (pos-eol))
      (insert (make-string header-length (aref new-header 1)))
      )))

;;; ----------------------------------------------------------------------
;;;
;;; Manipulació de capçaleres de seccions

;;;###autoload
(defun arv/rst-underline-header (caracter)
  (interactive "cCaracter: ")
  (let ((l (length (buffer-substring-no-properties (progn
                                                     (back-to-indentation)
                                                     (point))
                                                   (progn
                                                     (end-of-line)
                                                     (point)))))
        (indentation-level (progn
                             (back-to-indentation)
                             (current-column))))
    (when l
      (end-of-line)
      (insert "\n")
      (insert (make-string indentation-level ?\s))
      (insert (make-string l caracter))
      (insert "\n\n")
      (insert (make-string indentation-level ?\s)))))

;;;###autoload
(defun arv/rst-promote-header-at-point ()
  "Promou la capçalera actual.

Espera que el cursor estigui sobre el text de la capçalera. Si no
estem sobre una capçalera o ya està en el nivell màxim, no fa
res."
  (interactive)
  (let* ((header-length (arv/-rst-current-line-length))
         (old-header (arv/-rst-header-get-header-at-point))
         (new-header (arv/-rst-header-get-promoted-header old-header)))
    (arv/-rst-header-update-header header-length old-header new-header)))

;;;###autoload
(defun arv/rst-demote-header-at-point ()
  "Degrada la capçalera actual.

Espera que el cursor estigui sobre el text de la capçalera. Si no
estem sobre una capçalera o ya està en el nivell mínim, no fa
res."
  (interactive)
  (let* ((header-length (arv/-rst-current-line-length))
         (old-header (arv/-rst-header-get-header-at-point))
         (new-header (arv/-rst-header-get-demoted-header old-header)))
    (arv/-rst-header-update-header header-length old-header new-header)))

;;; ----------------------------------------------------------------------
;;;
;;; altres

;;;###autoload
(defun arv/rst-smart-grave ()
  "Tries to be smart about common ` usage patterns.

If point is after a role (like :xref:) inserts ``, elsewhere
inserts ```` (inline code literal). Point is left in the middle.

If the region is active surround it and point is left at the
end."
  (interactive)
  (let ((begin (point))
        (end   (point))
        (active (region-active-p))
        (delimiter))
    (when active
      (setq begin (min (region-beginning) (region-end)))
      (setq end   (max (region-beginning) (region-end))))
    (goto-char begin)
    (setq delimiter (if (arv/-rst-after-role-p)
                        "`"
                      "``"))
    (insert delimiter)
    (goto-char (+ end (length delimiter)))
    (insert delimiter)
    (unless active
      (backward-char (length delimiter)))))

;;;###autoload
(defun arv/rst-smart-asterisk ()
  "Tries to be smart about * usage.

If there's only withespace before point it assumes that it's a
list bullet and inserts '* ', otherwise it inserts '**' and
leaves point in the middle."
  (interactive)
  (if (string-match-p "^\s*$"
                      (buffer-substring-no-properties (line-beginning-position) (point)))
      (insert "* ")
    (insert "**")
    (backward-char 1)))


(provide 'arv-rst)

;;; arv-rst.el ends here
