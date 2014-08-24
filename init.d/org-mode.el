;;; org-mode.el --- org-mode customization

;; $Id$


;;; Commentary:
;;


;;; History:
;;

;;; Local keybindings:
;;
;; M-<f1> visit the org reference card (PDF format) if available

;;; Code:

(defvar -refcard-directory-root (arv/path-join data-directory "refcards"))
(defvar -org-refcard (arv/path-join -refcard-directory-root "orgcard.pdf"))

(when (file-readable-p -org-refcard)
  (eval-after-load "org"
    '(progn
       (define-key org-mode-map (kbd "M-<f1>")
         (lambda () (interactive) (find-file -org-refcard))))))


;; define 'ecfg:' links
(eval-after-load "org"
  '(progn
     (org-add-link-type "ecfg" 'arv/org-ecfg-open)
     (add-hook 'org-store-link-functions 'arv/org-ecfg-store-link)
     (define-key org-mode-map (kbd "C-c M-l") 'arv/org-ecfg-insert-link-at-point)
     ))


;; capture
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "gtd.org" "Tasks")
         (file "templates/todo.tmpl")
         :empty-lines 1 :clock-in t :clock-resume t)
        ("f" "Todo followup" entry (clock)
         (file "templates/todo_followup.tmpl")
         :empty-lines 1 :clock-in t :clock-resume t :prepend t)))


;; agenda
(setq org-agenda-files "~/.emacs.d/org/agenda_files")

(setq org-agenda-custom-commands
      '(("x" tags "TIPUS=\"error\"")
        ("v" todo "DONE|CANCELLED")))


;;; org-mode.el ends here
