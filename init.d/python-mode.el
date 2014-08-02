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

(autoload 'company-complete "company" "" t nil)
(autoload 'next-error "simple" "" t nil)
(autoload 'previous-error "simple" "" t nil)
(autoload 'highlight-indentation "highlight-indentation" "" t nil)

(require 'arv-py)

(eval-after-load "python"
  '(progn
     (modify-syntax-entry ?_ "w" python-mode-syntax-table)
     (setq python-shell-virtualenv-path (getenv "VIRTUAL_ENV"))

     ;; keybindings locals
     (define-key python-mode-map (kbd "s-SPC") 'company-complete)
     (define-key python-mode-map '[(super l)] 'pylint)
     (define-key python-mode-map '[f7] 'next-error)
     (define-key python-mode-map '[f8] 'previous-error)
     (define-key python-mode-map '[f9] (lambda ()
                                         (interactive)
                                         (call-interactively 'compile)))
     (define-key python-mode-map '[(control tab)]
       (make-hippie-expand-function
        '(try-expand-dabbrev
          try-expand-tags
          try-expand-dabbrev-all-buffers) t))
     (define-key python-mode-map '[(super tab)] 'python-indent-shift-right)
     (define-key python-mode-map '[(super iso-lefttab)] 'python-indent-shift-left)
     (define-key python-mode-map '[(:)] 'arv-py-electric-colon)
     (define-key python-mode-map '[(control m)] 'newline-and-indent)
     (define-key python-mode-map '[(control c) (j) (i)] 'arv-py-nav-goto-first-import)))


;; defineix advices per obrir/tancar un projecte rope al obrir/tancar
;; un projecte eproject
(eval-after-load "eproject"
  '(progn
    (defadvice eproject-open (after open-rope-project a)
      (if (file-exists-p (concat prj-directory ".ropeproject"))
          (rope-open-project prj-directory)))
    (defadvice eproject-close (before close-rope-project)
      (if (file-exists-p (concat prj-directory ".ropeproject"))
          (rope-close-project)))
    (ad-activate 'eproject-open)
    (ad-activate 'eproject-close)))


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

              ;; company-mode
              (company-mode t)

              ;; ressalta els nivell d'indentacio
              (highlight-indentation)

              ;; pylint/flymake-pylint
              (pylint-add-menu-items)
              (pylint-add-key-bindings)))


;;; python-mode.el ends here
