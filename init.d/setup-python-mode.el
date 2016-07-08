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

(require 'arv-py)
(require 'elpy)
(require 'pytest)
(require 'pyx)
(require 'subword)
(require 'superword)

;;                             elpy

(eval-after-load "elpy"
  '(progn
     (setq elpy-modules
           '(elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv
             elpy-module-sane-defaults elpy-module-yasnippet))))


;;                             pylookup

(autoload 'pylookup-lookup "pylookup")
(autoload 'pylookup-update "pylookup")

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "C-c h") 'pylookup-lookup)))

(add-hook 'python-mode-hook
          (lambda()
            (let ((pylookup-dir (arv/startup-get-absolute-path "site-lisp/pylookup")))
              (setq pylookup-program (arv/path-join pylookup-dir "pylookup.py"))
              (setq pylookup-db-file (arv/path-join pylookup-dir "pylookup.db"))
              )))


;;                             flymake

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))


;;                            pydoc info

(autoload 'info-lookup-add-help "info-look" "" nil nil)

(eval-after-load "python"
  '(progn
     (info-lookup-add-help
      :mode 'python-mode
      :parse-rule 'pydoc-info-python-symbol-at-point
      :doc-spec
      '(("(python)Index" pydoc-info-lookup-transform-entry)
        ("(django14)Index" pydoc-info-lookup-transform-entry)))))


;;                               pep8

(eval-after-load "pep8"
  '(progn
     (setq python-pep8-options '("--repeat" "--ignore=E203,E201,E202,E123"))))


;;                              pylint
(eval-after-load "pylint"
  '(progn
     ;; NOTE: 20151126 pylint 1.01 depen de tramp però no el
     ;; requereix, resultant en l'error:
     ;;
     ;; pylint: Symbol's function definition is void: tramp-tramp-file-p
     ;;
     (defun require-tramp (&rest ignored)
       (require 'tramp))
     (add-function :before (symbol-function 'pylint) #'require-tramp)))


;;                              pytest
(eval-after-load "pytest"
  '(progn
     (setq pytest-mode-keymap-prefix "C-c m t")
     (define-key python-mode-map (kbd "<f12>") 'pytest-all)
     (define-key pytest-mode-map (kbd "<f12>") 'pytest-one)
     (elpy-use-ipython)
     (add-hook 'python-mode-hook 'pytest-mode-enable-if-test-module)))


;;                              python

(eval-after-load "python"
  '(progn
     (setq python-shell-virtualenv-path (getenv "VIRTUAL_ENV"))
     (setq python-indent-guess-indent-offset nil)

     (elpy-enable)

     ;; keybindings locals
     (define-key python-mode-map (kbd "s-SPC") 'company-complete)
     (define-key python-mode-map (kbd "C-c m d") 'pydoc)
     (define-key python-mode-map (kbd "C-c m m") 'pyx/make)
     (define-key python-mode-map (kbd "C-c m p") 'pylint)
     (define-key python-mode-map (kbd "C-c m 8") 'pep8)
     (define-key python-mode-map (kbd "C-c m w f") 'pyx/refactor-wrap-for)
     (define-key python-mode-map (kbd "C-c m w i") 'pyx/refactor-wrap-if-else)
     (define-key python-mode-map (kbd "C-c m w t") 'pyx/refactor-wrap-try-except)
     (define-key python-mode-map (kbd "C-c m w w") 'pyx/refactor-wrap-while)
     (define-key python-mode-map (kbd "C-c m i s d ") 'pyx/add-setup-dependency)
     (define-key python-mode-map (kbd "C-c m i p n") 'arv/py-insert-current-package-name)
     (define-key python-mode-map (kbd "C-c m i u") (lambda () "Insert random UUID" (interactive) (insert (arv/generate-random-uuid))))

     (define-key python-mode-map (kbd "M-<f1>") (lambda () (interactive) (info "(python.info) Python Module Index")))
     ;; TODO: aquests tres possiblement siguin globals (?)
     (define-key python-mode-map (kbd "<f7>") 'next-error)
     (define-key python-mode-map (kbd "<f8>") 'previous-error)
     (define-key python-mode-map (kbd "<f9>") 'pyx/make)
     (define-key python-mode-map (kbd "C-<tab>")
       (make-hippie-expand-function
        '(try-expand-dabbrev
          ;;; try-expand-tags
          try-expand-dabbrev-all-buffers) t))
     (define-key python-mode-map (kbd "s-<tab>") 'python-indent-shift-right)
     (define-key python-mode-map (kbd "s-<iso-lefttab>") 'python-indent-shift-left)
     (define-key python-mode-map (kbd ":") 'pyx/electric-colon)
     (define-key python-mode-map (kbd "`") 'pyx/smart-grave)
     (define-key python-mode-map (kbd "C-m") 'newline-and-indent)
     (define-key python-mode-map (kbd "C-c j S") 'arv/py-visit-setup-py)
     (define-key python-mode-map (kbd "C-c j i") 'arv-py-nav-goto-first-import)
     (define-key python-mode-map (kbd "C-=") 'arv/rst-underline-header)
     (define-key python-mode-map (kbd "C-,") 'arv/wm-cycle-2)
     ))


;; enable triple quote in autopair
;;
;; if emacs >= 24.4 electric-pair-mode is enabled instead of autopair
;; (see ./setup-autopair.el), so this code is executed only if
;; autopair is loaded.
(eval-after-load "autopair"
  '(add-hook 'python-mode-hook
             (lambda ()
               (setq autopair-handle-action-fns
                     (list #'autopair-default-handle-action
                           #'autopair-python-triple-quote-action)))))

(add-hook 'python-mode-hook
          (lambda ()
            ;; outline
            (outline-minor-mode t)
            (set
             (make-local-variable 'outline-regexp)
             "[\t ]*\\(class\\|def\\|if\\|elsif\\|else\\|while\\|for\\|try\\|except\\|finally\\)\\>")

            ;; ressalta les línies on hi ha un breakpoint
            (highlight-lines-matching-regexp "^[[:space:]]*import i?pdb")
            (highlight-lines-matching-regexp "^[[:space:]]*i?pdb.set_trace()")

            (highlight-lines-matching-regexp "sefl")

            ;; company-mode
            (company-mode t)

            ;; ressalta els nivell d'indentacio
            (arv/highlight-indentation-mode 0)

            ;; pylint/flymake-pylint
            (pylint-add-menu-items)

            (superword-mode 1)
            ))


;;; python-mode.el ends here
