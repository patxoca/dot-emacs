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

(defun arv/rst-smart-grave ()
  "Tries to be smart about common ` usage patterns.

After : inserts `` (like in :xref:`whatever`) elsewhere inserts
```` (inline code literal). In both cases point is left in the
middle."
  (interactive)
  (if (save-excursion
        (unless (bobp)
         (backward-char)
         (looking-at ":")))
      (progn
        (insert "``")
        (backward-char 1))
    (insert "````")
    (backward-char 2)))

(eval-after-load "rst"
    '(progn
       (define-key rst-mode-map (kbd "`") 'arv/rst-smart-grave)
       (define-key rst-mode-map '[f9] 'arv-sphinx-build-latexpdf)
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
