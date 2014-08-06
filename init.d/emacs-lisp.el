;;; emacs-lisp.el --- configuració de emacs-lisp

;; $Id$


;;; Commentary:
;;


;;; History:
;;


;;; Code:


(autoload 'company-complete "company" "" t nil)


(defun arv/eval-buffer ()
  (interactive)
  (call-interactively 'eval-buffer)
  (message "Buffer has been evaluated"))

(defun arv/scratch ()
  (interactive)
  (let ((current-mode major-mode))
    (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (goto-char (point-min))
    (when (looking-at ";")
      (forward-line 4)
      (delete-region (point-min) (point)))
    (goto-char (point-max))
    (if (memq current-mode lisp-mode)
        (funcall current-mode))))

(defun arv/emacs-lisp-insert-grave ()
  "Inserts `' within a string or just ` otherwise."
  (interactive)
  (if (nth 8 (syntax-ppss))
      (progn
        (insert "`'")
        (backward-char 1))
    (insert "`")))

(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "C-<right>") 'right-word)
     (define-key paredit-mode-map (kbd "C-<left>") 'left-word)
     (define-key paredit-mode-map (kbd "M-<right>") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-<left>") 'paredit-forward-barf-sexp)))

(eval-after-load "lisp-mode"
  '(progn
     (define-key emacs-lisp-mode-map (kbd "s-SPC") 'company-complete)
     (define-key emacs-lisp-mode-map '[f9] (lambda () (interactive) (ert t)))
     (define-key emacs-lisp-mode-map '[(meta f1)] (lambda () (interactive) (info "elisp")))
     (define-key emacs-lisp-mode-map (kbd "`") 'arv/emacs-lisp-insert-grave)
     (define-key emacs-lisp-mode-map (kbd "s-c") 'arv/startup-byte-recompile)
     (define-key emacs-lisp-mode-map (kbd "C-c e b") 'arv/eval-buffer)
     (define-key emacs-lisp-mode-map (kbd "C-c e c") 'cancel-debug-on-entry)
     (define-key emacs-lisp-mode-map (kbd "C-c e d") 'debug-on-entry)
     (define-key emacs-lisp-mode-map (kbd "C-c e e") 'toggle-debug-on-error)
     (define-key emacs-lisp-mode-map (kbd "C-c e f") 'emacs-lisp-byte-compile-and-load)
     (define-key emacs-lisp-mode-map (kbd "C-c e l") 'find-library)
     (define-key emacs-lisp-mode-map (kbd "C-c e m") 'macrostep-mode)
     (define-key emacs-lisp-mode-map (kbd "C-c e p") 'paredit-mode)
     (define-key emacs-lisp-mode-map (kbd "C-c e r") 'eval-region)
     (define-key emacs-lisp-mode-map (kbd "C-c e s") 'arv/scratch)
     (define-key emacs-lisp-mode-map (kbd "C-c e v") 'find-variable)
     (define-key emacs-lisp-mode-map (kbd "C-h e V") 'apropos-value)
     (define-key emacs-lisp-mode-map (kbd "C-c e z") 'byte-recompile-directory)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (company-mode t)
            (eldoc-mode t)
            (paredit-mode 1)
            (autopair-mode 0) ;; coflicts with paredit
            (elisp-slime-nav-mode 1)
            (pretty-lambda-mode t)
            ))

;;; 'emacs-lisp.el ends here
