;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Codi tret de:
;;
;; http://www.emacswiki.org/emacs/OutlineMinorMode
;;
;; Defineix funcions que fan l'us del outline-mode més intuitiu
;; (semblant al comportament de l'explorer de windows) i les assigna a
;; combinacions de tecles més accesibles.

(require 'outline)

(defun outline-body-p ()
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (and (not (eobp))
         (progn (forward-char 1)
                (not (outline-on-heading-p))))))

(defun outline-body-visible-p ()
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (not (outline-invisible-p))))

(defun outline-subheadings-p ()
  (save-excursion
    (outline-back-to-heading)
    (let ((level (funcall outline-level)))
      (outline-next-heading)
      (and (not (eobp))
           (< level (funcall outline-level))))))

(defun outline-subheadings-visible-p ()
  (interactive)
  (save-excursion
    (outline-next-heading)
    (not (outline-invisible-p))))

(defun outline-hide-more ()
  (interactive)
  (when (outline-on-heading-p)
    (cond ((and (outline-body-p)
                (outline-body-visible-p))
           (hide-entry)
           (hide-leaves))
          (t
           (hide-subtree)))))

(defun outline-show-more ()
  (interactive)
  (when (outline-on-heading-p)
    (cond ((and (outline-subheadings-p)
                (not (outline-subheadings-visible-p)))
           (show-children))
          ((and (not (outline-subheadings-p))
                (not (outline-body-visible-p)))
           (show-subtree))
          ((and (outline-body-p)
                (not (outline-body-visible-p)))
           (show-entry))
          (t
           (show-subtree)))))

(let ((map outline-mode-map))
  (define-key map (kbd "s-<left>") 'outline-hide-more)
  (define-key map (kbd "s-<right>") 'outline-show-more)
  (define-key map (kbd "s-<up>") 'outline-previous-visible-heading)
  (define-key map (kbd "s-<down>") 'outline-next-visible-heading))

(let ((map outline-minor-mode-map))
  (define-key map (kbd "s-<left>") 'outline-hide-more)
  (define-key map (kbd "s-<right>") 'outline-show-more)
  (define-key map (kbd "s-<up>") 'outline-previous-visible-heading)
  (define-key map (kbd "s-<down>") 'outline-next-visible-heading)
  ;; prova per moure blocs, no acava de funcionar
  ;;(define-key map (kbd "S-s-<up>") 'outline-move-subtree-up)
  ;;(define-key map (kbd "S-s-<down>") 'outline-move-subtree-down)
)


