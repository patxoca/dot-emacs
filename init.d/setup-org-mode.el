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
      '(("i" "Interrupcio" entry (file+headline "gtd.org" "Interrupcions")
         (file "templates/interrupt.tmpl")
         :empty-lines 1 :clock-in t :clock-resume nil)
        ("t" "Todo" entry (file+headline "gtd.org" "Tasks")
         (file "templates/todo.tmpl")
         :empty-lines 1 :clock-in t :clock-resume t)
        ("f" "Todo followup" entry (clock)
         (file "templates/todo_followup.tmpl")
         :empty-lines 1 :clock-in t :clock-resume t :prepend t)
        ("s" "Sibling" entry (function (lambda () (org-up-heading-safe) (org-end-of-subtree t)))
         (file "templates/sibling.tmpl")
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
     (define-key org-mode-map (kbd "C-c $") 'arv/org-archive-subtree)
     (define-key org-mode-map (kbd "C-c C-w") 'arv/org-refile)))


;; options
(eval-after-load "org"
  '(progn
     (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
     (setq org-id-track-globally t)))


;;; workflow
;;
;; TODO(t) STRT(s!) PAUS(p@) HOLD(i!) WAIT(w@) | DONE(d!) CANC(c@)
;;
;; - TODO: no action taken, just created the note.
;; - STRT: currently working on, there can be only one.
;; - PAUS: paused, still not finished.
;; - HOLD: interrupted, there can be only one.
;; - WAIT: paused, waiting for external feedback.
;; - DONE: finished, no further action is required.
;; - CANC: canceled, no further action is required.

(eval-after-load "org"
  '(progn
     (setq org-todo-keywords
           '((sequence "TODO(t)" "STRT(s!)" "PAUS(p@)" "HOLD(i!)"
                       "WAIT(w@)" "|" "DONE(d!)" "CANC(c@)")))
     (setq org-directory "~/.emacs.d/org")
     (setq org-startup-folded t)
     (require 'arv-org)
     (setq arv/org-interrupt-resumed-state "STRT")
     (setq arv/org-interrupt-interrupted-state "HOLD")
     (setq arv/org-interrupt-capture-key "i")
     (setq arv/org-sctc-entering-state-clocking-actions
           '(("STRT" . nil)
             ("PAUS" . nil)
             ("WAIT" . nil)))
     (setq arv/org-sctc-paused-state "PAUS")
     (arv/org-sctc-setup)))


;; faces
(eval-after-load "org"
  '(progn
     (set-face-attribute 'org-mode-line-clock nil :background "green yellow")
     (set-face-attribute 'org-document-title nil
                         :foreground "pale turquoise"
                         :weight 'bold
                         :height 1.5)
     (set-face-attribute 'org-level-1 nil
                         :weight 'bold
                         :height 1.2)
     (set-face-attribute 'org-block-begin-line nil
                         :weight 'normal
                         :background "#202020")
     (set-face-attribute 'org-block nil
                         :background "black")
     (set-face-attribute 'org-block-end-line nil
                         :weight 'normal
                         :background "#202020")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "pink" :weight bold))
        ("STRT" . (:foreground "yellow" :weight bold))
        ("HOLD" . (:foreground "yellow" :weight bold))
        ("PAUS" . (:foreground "pale green" :weight bold))
        ("WAIT" . (:foreground "orange" :weight bold))
        ("DONE" . (:foreground "forest green" :weight bold))
        ("CANC" . (:foreground "red" :weight bold))))

;;; org-mode.el ends here
