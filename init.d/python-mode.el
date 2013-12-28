;;; python-mode.el --- personalitzacio del python mode
;;; (c) 2006 Alexis Roda
;;; $Id$

;;; Commentary:
;;;
;;;  arxiu de configuracio de python-mode

;;; History:
;;;
;;; consultar log de svn

;;; Code:

(require 'autopair)
(require 'python)
(require 'hippie-exp)
(require 'he-utils)
(require 'highlight-indentation)

(require 'arv-py)

(eval-after-load "python"
  '(progn
     (modify-syntax-entry ?_ "w" python-mode-syntax-table)))

(add-hook 'python-mode-hook
          #'(lambda ()
              ;; outline
              (outline-minor-mode t)
              (set
               (make-local-variable 'outline-regexp)
               "[\t ]*\\(class\\|def\\|if\\|elsif\\|else\\|while\\|for\\|try\\|except\\|finally\\)\\>")

              ;; autopair
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))

              ;; ressalta les línies on hi ha un breakpoint
              (highlight-lines-matching-regexp "^[[:space:]]*import i?pdb")
              (highlight-lines-matching-regexp "^[[:space:]]*i?pdb.set_trace()")

              (highlight-lines-matching-regexp "sefl")

              ;; recicla la comanda 'compile' per executar els tests amb
              ;; nose
              (set (make-local-variable 'compile-command) "nosetests")

              ;; company-mode (complete anything)
              (company-mode t)
              ;; ressalta els nivell d'indentacio
              (highlight-indentation)
              ;; local keymap
              (local-set-key '[(super tab)] 'python-indent-shift-right)
              (local-set-key '[(super iso-lefttab)] 'python-indent-shift-left)
              (local-set-key '[f7] 'flymake-goto-next-error)
              (local-set-key '[f8] 'flymake-goto-prev-error)
              (local-set-key '[f9] (lambda ()
                                     (interactive)
                                     (call-interactively 'compile)))
              (local-set-key '[(control tab)]
                             (make-hippie-expand-function
                              '(try-expand-dabbrev
                                try-expand-tags
                                try-expand-dabbrev-all-buffers) t))
              (local-set-key '[(:)] 'arv-py-electric-colon)
              (local-set-key '[(control m)] 'newline-and-indent)))




;;; python-mode.el ends here
