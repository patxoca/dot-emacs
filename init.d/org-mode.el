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


;; keybindings
(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "/") (lambda () (interactive) (arv/org-emphasize ?/)))
     (define-key org-mode-map (kbd "*") (lambda () (interactive) (arv/org-emphasize ?*)))
     (define-key org-mode-map (kbd "_") (lambda () (interactive) (arv/org-emphasize ?_)))
     (define-key org-mode-map (kbd "=") (lambda () (interactive) (arv/org-emphasize ?=)))
     (define-key org-mode-map (kbd "~") (lambda () (interactive) (arv/org-emphasize ?~)))
     (define-key org-mode-map (kbd "+") (lambda () (interactive) (arv/org-emphasize ?+)))
     (define-key org-mode-map (kbd "C-c M-q") 'arv/org-remove-reduntant-tags)
     (define-key org-mode-map (kbd "C-c q")   'arv/org-add-inherited-tags)
     (define-key org-mode-map (kbd "C-c C-w") 'arv/org-refile)))

;; faces

(eval-after-load "org"
  '(progn
     (set-face-attribute 'org-mode-line-clock nil :background "green yellow")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "pink" :weight bold))
        ("STRT" . (:foreground "yellow" :weight bold))
        ("PAUS" . (:foreground "pale green" :weight bold))
        ("WAIT" . (:foreground "orange" :weight bold))
        ("DONE" . (:foreground "forest green" :weight bold))
        ("CANC" . (:foreground "red" :weight bold))))


;;; org-mode.el ends here
