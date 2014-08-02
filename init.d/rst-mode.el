;;; rst-mode.el --- configuracio de rst-mode
;;; $Id$

;;; Commentary:
;;


;;; History:
;;

;;; Code:


(defun arv-rst-underline-previous-line (caracter)
  (interactive "cCaracter: ")
  (let (l)
    (save-excursion
      (if (= (forward-line -1) 0)
          (setq l (length (buffer-substring-no-properties (point)
                                                          (save-excursion
                                                            (end-of-line)
                                                            (point)))))))
    (if l
        (progn
          (beginning-of-line)
          (insert (make-string l caracter))
          (insert "\n\n")))))


(add-hook 'rst-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (flyspell-mode 1)
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
            (local-set-key '[(control =)] 'arv-rst-underline-previous-line)
            (local-set-key '[f9] 'arv-sphinx-build-latexpdf)
            ))

;;; rst-mode.el ends here
