;;; global-key-map.el --- configuracio de tecles globals
;;; $Id$

;;; Commentary:
;;
;; reconfiguracio de tecles globalment.  Per evitar conflictes, els
;; comandos personals els configuro sota el prefix C-c C-a i segueixo
;; el conveni de que els moduls que escric defineixen el seu propi
;; "subespai" de tecles (p.e. todo.el utilitza C-c C-a C-r).
;;
;; Es defineixen les tecles següents:
;;
;; C-F1        invoca info Emacs
;; S-g         permet saltar a una linea (goto-line)
;; C-RET       insereix "yes\n"
;; C-TAB       executa `dabbrev-expand', busca completacions de la
;;             paraula que estem escribint al buffer actual i als
;;             buffers oberts
;; C-c C-a g   equivalent a H-g, per teclats sense la tecla windows
;; C-c C-a M-5 invoca query-replace-regexp (M-5 es query-replace)


;;; History:
;;

;;; Code:

;; disable some keybindings

;; disable C-x o while I get used to the window motion keybindings
(global-unset-key (kbd "C-x o"))

;; disable arrow  keys
;; (progn
;;   (global-unset-key (kbd "<up>"))
;;   (global-unset-key (kbd "<down>"))
;;   (global-unset-key (kbd "<left>"))
;;   (global-unset-key (kbd "<right>"))
;;   (global-unset-key (kbd "C-<up>"))
;;   (global-unset-key (kbd "C-<down>"))
;;   (global-unset-key (kbd "C-<left>"))
;;   (global-unset-key (kbd "C-<right>"))
;;   (global-unset-key (kbd "M-<up>"))
;;   (global-unset-key (kbd "M-<down>"))
;;   (global-unset-key (kbd "M-<left>"))
;;   (global-unset-key (kbd "M-<right>")))

;; (progn
;;   (global-set-key (kbd "M-n") 'forward-paragraph)
;;   (global-set-key (kbd "M-p") 'backward-paragraph))

;; ocasionally I press C-x C-c by accident, define a more complex
;; keybinding to kill emacs
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)

;; On X11 C-z serves no purpose since the wm defines keybindings to
;; minimize windows. OTOH is very annoying when pressed by
;; accident. In any case C-x C-z is still available.
(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'shell))


(global-set-key (kbd "M-u") 'arv/upcase-word)
(global-set-key (kbd "M-l") 'arv/downcase-word)
(global-set-key (kbd "s-y") 'copy-from-above-command)


;; other bindings

(global-set-key (kbd "<f1>") (lambda () (interactive) (info)))
(global-set-key (kbd "C-<f1>") (lambda () (interactive) (info "emacs")))
(global-set-key (kbd "s-g") 'arv/goto-line)
;;; es un incordio tener que teclear yes para confirmar
(global-set-key (kbd "C-<return>") "yes\C-m")

(global-set-key (kbd "M-%") 'arv-query-replace)
(global-set-key (kbd "M-w") 'arv-kill-ring-save-word-at-point)

(global-set-key (kbd "C-<tab>") 'dabbrev-expand)

(global-set-key (kbd "C-<prior>") 'xah-previous-user-buffer) ; Ctrl+PageDown
(global-set-key (kbd "C-<next>") 'xah-next-user-buffer) ; Ctrl+PageUp

(global-set-key (kbd "s-t") 'arv-switch-to-todo-or-visit-todo-file)

(global-set-key (kbd "C-x C-b") 'electric-buffer-list)
                                        ; originalment 'list-buffers'.
                                        ;
                                        ; Una altra alternativa si
                                        ; aquesta no em convenç és
                                        ; buffer-menu
                                        ;
                                        ; i una altra ibuffer (molt
                                        ; potent aparentment)

(global-set-key (kbd "<help>") (lambda () (interactive) (find-file-other-window "~/.emacs.d/org/emacs_novetats.org")))

;;; eproject
(global-set-key (kbd "C-c p o") 'eproject-open)
(global-set-key (kbd "C-c p c") 'eproject-close)
(global-set-key (kbd "C-c p a") 'eproject-add)
(global-set-key (kbd "C-c p r") 'eproject-remove)

;;; django
;; aquest keybinding no m'acava de convencer per dos motius:
;;
;; * envaeix l'espai de keybindinds de eproject. No especialment greu
;;   i en certa forma esta relacionat.
;;
;; * defineix globalment un keybinding que no té sentit en tots els
;;   contexts. Malauradament no es pot determinar si te sentit en un
;;   context fins que s'ha obert el projecte django.

(global-set-key (kbd "C-c p d") 'arv/django-switch-to-project-buffer)

;; els keybindings de ace-jump-mode em resulten dificils de recordar
;; i poc còmodes
(global-set-key (kbd "C-c j c") 'ace-jump-char-mode)
(global-set-key (kbd "C-c j l") 'ace-jump-line-mode)
(global-set-key (kbd "C-c j s") 'imenu)
(global-set-key (kbd "C-c j w") 'ace-jump-word-mode)

;; keybindings per subversion: C-c s
(global-set-key (kbd "C-c s =") 'svn-file-show-svn-diff)
(global-set-key (kbd "C-c s l") 'svn-status-show-svn-log)
(global-set-key (kbd "C-c s r") 'svn-file-revert)
(global-set-key (kbd "C-c s s") 'svn-status)

;; window motion
(global-set-key (kbd "H-e") 'windmove-up)
(global-set-key (kbd "H-d") 'windmove-down)
(global-set-key (kbd "H-s") 'windmove-left)
(global-set-key (kbd "H-f") 'windmove-right)

;; buffer-move
(global-set-key (kbd "H-E") 'buf-move-up)
(global-set-key (kbd "H-D") 'buf-move-down)
(global-set-key (kbd "H-S") 'buf-move-left)
(global-set-key (kbd "H-F") 'buf-move-right)


;; expand-region
(global-set-key (kbd "C-.") 'er/expand-region)

;; todo
(global-set-key (kbd "C-c r i") 'srctool-insert-reminder)
(global-set-key (kbd "C-c r s") 'srctool-show-reminders)

;; find-file-in-project
(global-set-key (kbd "C-x f") 'find-file-in-project)

;; org
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c o f") 'arv/org-visit-agenda-file)
(global-set-key (kbd "C-c o s") 'org-sort)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; toggle map
(global-set-key (kbd "C-c t c") 'column-number-mode)
(global-set-key (kbd "C-c t d") 'toggle-window-dedicated)
(global-set-key (kbd "C-c t f") 'auto-fill-mode)
(global-set-key (kbd "C-c t F") 'toggle-fullscreen)
(global-set-key (kbd "C-c t l") 'linum-mode)
(global-set-key (kbd "C-c t L") 'line-number-mode)
(global-set-key (kbd "C-c t r") 'read-only-mode)
(global-set-key (kbd "C-c t R") 'relative-linum-toggle)

;; word-mode
(global-set-key (kbd"C-,") 'arv/wm-cycle)

;;; global-key-map.el ends here
