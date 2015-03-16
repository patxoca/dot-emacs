;;; setup-highlight-indentation.el --- customization for highlight-indentation

;; $Id:$


;;; Commentary:
;;

;;; Local keybindings:
;;


;;; History:
;;


;;; Code:


;; highlight-indentation
(eval-after-load "highlight-identation"
  '(progn
     (set-face-attribute 'highlight-indent-face nil
                         :background "#303739")))


(defun arv/highlight-indentation-mode (&optional arg)
  "Enable/disable/toggle `highlight-indentation' and
`highlight-indentation-current-column' mode, but only if
available.

With no argument or nil toggles, 0 disables, 1 enables."
  (interactive "P")
  (if (not (fboundp 'highlight-indentation-mode))
      (message "ERROR: no es pot inicialitzar `highlight-indentation-mode'")
    (let ((arg (if (null arg) 'toggle arg)))
      (highlight-indentation-mode arg)
      (highlight-indentation-current-column-mode arg))))

;;; setup-highlight-indentation.el ends here
