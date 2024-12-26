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
;; - arv/rst-header-adjust-header-at-point
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

(defvar arv/-rst-header-adjust-header-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-+") #'arv/rst-header-adjust-header-at-point)
    (define-key map (kbd "C--") #'arv/rst-header-adjust-header-at-point)
    (define-key map (kbd "+") #'arv/rst-header-adjust-header-at-point)
    (define-key map (kbd "-") #'arv/rst-header-adjust-header-at-point)
    map)
  "transient keymap utilitzat per la comanda
arv/rst-header-adjust-header-at-point")

;;; ----------------------------------------------------------------------
;;;
;;; Funcions auxiliars

(defun arv/-rst-after-role-p ()
  "Return t if point is after a role."
  (looking-back ":\\w+:" (line-beginning-position)))

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
(defun arv/rst-header-adjust-header-at-point ()
  "Invoca `rst-adjust-section' i activa un menú transient que permet
repetir la comanda."
  (interactive)
  (let* ((ev last-command-event)
         (echo-keystrokes nil)
         (base (event-basic-type ev)))
    (if (= base ?+)
        (rst-adjust-section nil nil)
      (rst-adjust-section nil t))
    (set-transient-map arv/-rst-header-adjust-header-keymap)))

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
list bullet and inserts '* ', otherwise it surrounds the region
and leaves the point at the end, if the region is active, or
inserts '**' and leaves point in the middle otherwise."
  (interactive)
  (if (string-match-p "^\s*$"
                      (buffer-substring-no-properties (line-beginning-position) (point)))
      (insert "* ")
    (let ((begin (point))
          (end   (point))
          (active (region-active-p)))
      (when active
        (setq begin (min (region-beginning) (region-end)))
        (setq end   (max (region-beginning) (region-end))))
      (goto-char begin)
      (insert "*")
      (goto-char (1+ end))
      (insert "*")
      (unless active
        (backward-char (length delimiter))))))


(provide 'arv-rst)

;;; arv-rst.el ends here
