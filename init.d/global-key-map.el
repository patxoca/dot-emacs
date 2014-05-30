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

(require 'todo-util)

;;; Code:

;; (global-set-key '[home]             'beginning-of-line)
;; (global-set-key '[(control home)]   'beginning-of-buffer)
;; (global-set-key '[end]              'end-of-line)
;; (global-set-key '[(control end)]    'end-of-buffer)
;; (global-set-key '[f1]               'info)
(global-set-key '[(control f1)]     (lambda () (interactive) (info "emacs")))
(global-set-key '[(super g)] 'goto-line)
;;; es un incordio tener que teclear yes para confirmar
(global-set-key '[(control return)] "yes\C-m")

(global-set-key [(control c) (control a) (g)] 'goto-line)
(global-set-key '[(meta %)] 'arv-query-replace)
(global-set-key [(control c) (control a) (meta 5)] 'query-replace-regexp)
(global-set-key '[(meta w)] 'arv-kill-ring-save-word-at-point)

(global-set-key '[(control tab)] 'dabbrev-expand)

(global-set-key [(control prior)] 'xah-previous-user-buffer) ; Ctrl+PageDown
(global-set-key [(control next)] 'xah-next-user-buffer) ; Ctrl+PageUp

(global-set-key [(super d)] 'delete-trailing-whitespace) ; super+d
(global-set-key [(super t)] 'arv-switch-to-todo-or-visit-todo-file) ; super+d

(global-set-key [(control x) (control b)] 'electric-buffer-list)
                                        ; originalment 'list-buffers'.
                                        ;
                                        ; Una altra alternativa si
                                        ; aquesta no em convenç és
                                        ; buffer-menu
                                        ;
                                        ; i una altra ibuffer (molt
                                        ; potent aparentment)

(global-set-key [f11] 'toggle-fullscreen)
(global-set-key [pause] 'toggle-window-dedicated)

;;; eproject
(global-set-key '[(control c) (p) (o)] 'eproject-open)
(global-set-key '[(control c) (p) (c)] 'eproject-close)
(global-set-key '[(control c) (p) (a)] 'eproject-add)
(global-set-key '[(control c) (p) (r)] 'eproject-remove)

;;; django
;; aquest keybinding no m'acava de convencer per dos motius:
;;
;; * envaeix l'espai de keybindinds de eproject. No especialment greu
;;   i en certa forma esta relacionat.
;;
;; * defineix globalment un keybinding que no té sentit en tots els
;;   contexts. Malauradament no es pot determinar si te sentit en un
;;   context fins que s'ha obert el projecte django.

(global-set-key '[(control c) (p) (d)] 'arv/django-switch-to-project-buffer)

;; En X11 C-z és més molest que altra cosa. El gestor de finestres
;; ofereix mecanismes per minimitzar/enviar al fons la finestra des
;; del teclat i en última instància es pot utilitzar C-x C-z
(if (display-graphic-p)
  (global-unset-key '[(control z)]))

;; els keybindings de ace-jump-mode em resulten dificils de recordar
;; i poc còmodes
(global-set-key '[(control c) (j) (c)] 'ace-jump-char-mode)
(global-set-key '[(control c) (j) (l)] 'ace-jump-line-mode)
(global-set-key '[(control c) (j) (w)] 'ace-jump-word-mode)

;; keybindings per subversion: C-c s
(global-set-key '[(control c) (s) (s)] 'svn-status)
(global-set-key '[(control c) (s) (=)] 'svn-file-show-svn-diff)

;; les combinacions "C-x o" i "C-u -1 C-x o" per moure's entre les
;; finestres sont molt farragoses. Mai he utilitzat "S-<cursor>", les
;; redefineixo per moure's entre finestres.
(global-set-key '[(shift up)] 'windmove-up)
(global-set-key '[(shift down)] 'windmove-down)
(global-set-key '[(shift left)] 'windmove-left)
(global-set-key '[(shift right)] 'windmove-right)

;; expand-region

(global-set-key '[(control ?.)] 'er/expand-region)

;;; global-key-map.el ends here
