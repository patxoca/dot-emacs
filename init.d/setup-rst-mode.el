;;; rst-mode.el --- configuracio de rst-mode
;;; $Id$

;;; Commentary:
;;


;;; History:
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


(eval-after-load "rst"
    '(progn
       (define-key rst-mode-map (kbd "*") 'arv/rst-smart-asterisk)
       (define-key rst-mode-map (kbd "`") 'arv/rst-smart-grave)
       (define-key rst-mode-map (kbd "<f9>") 'arv-sphinx-build-latexpdf)
       (define-key rst-mode-map (kbd "C-<f9>") 'arv-sphinx-run-doctest)
       (define-key rst-mode-map '[(control =)] 'arv/rst-underline-header)))

(add-hook 'rst-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (flyspell-mode 0)
            ;; activa el resaltat d'espai en blanc al final de línia
            (set-variable 'show-trailing-whitespace t)
            ;; customize-face (suposo) te problemes quan la llista
            ;; d'atributs esta buida, sembla que ho interpreta com "no
            ;; canviar res" enlloc de "borrar tots els atributs"
            (set-face-attribute 'rst-level-1 nil :background "unspecified")
            (set-face-attribute 'rst-level-2 nil :background "unspecified")
            (set-face-attribute 'rst-level-3 nil :background "unspecified")
            (set-face-attribute 'rst-level-4 nil :background "unspecified")
            (set-face-attribute 'rst-level-5 nil :background "unspecified")
            (set-face-attribute 'rst-level-6 nil :background "unspecified")
            ))

;;; rst-mode.el ends here
