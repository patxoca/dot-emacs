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

;;
;;

;;; Code:

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

(defun arv/-rst-after-role-p ()
  "Return t if point is after a role."
  (looking-back ":\\w+:" (line-beginning-position)))

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
