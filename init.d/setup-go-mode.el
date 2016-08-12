;;; setup-go-mode.el --- customization for go-mode

;; $Id$


;;; Commentary:
;;

;;; Local keybindings:
;;
;; C-M-q: does some amazing stuff

;;; History:
;;


;;; Code:

(require 'company)
(require 'company-go)
(require 'go-eldoc)
(require 'go-mode)
(require 'projectile)

(defun arv/gobuild ()
  "Jumps to project root and runs 'go build -v'."
  (interactive)
  (compile (format "cd %s && go build -v" (projectile-project-root)) 't))

(defun arv/goinstall ()
  "runs 'go install' on current package."
  (interactive)
  (compile (format "cd %s && go install" (file-name-directory (buffer-file-name))) 't))

(defun arv/gotest ()
  "runs 'go test' on current package."
  (interactive)
  (compile (format "cd %s && go test" (file-name-directory (buffer-file-name))) 't))

(eval-after-load "go-mode"
  '(progn
     (setq gofmt-command "goimports")
     ;; flymake
     (require 'flymake-go)
     ;; documentation
     (setq godoc-at-point-function 'godoc-gogetdoc)
     (define-key go-mode-map (kbd "C-c m d") 'godoc-at-point)
     ;; jumping around
     (define-key go-mode-map (kbd "C-c j a") 'go-goto-arguments)
     (define-key go-mode-map (kbd "C-c j d") 'go-goto-docstring)
     (define-key go-mode-map (kbd "C-c j i") 'go-goto-imports)
     (define-key go-mode-map (kbd "C-c j m") 'go-goto-method-receiver)
     (define-key go-mode-map (kbd "C-c j n") 'go-goto-function-name)
     (define-key go-mode-map (kbd "C-c j r") 'go-goto-return-values)
     (define-key go-mode-map (kbd "M-.") 'godef-jump)
     ;; compilation
     (define-key go-mode-map (kbd "<f7>") 'next-error)
     (define-key go-mode-map (kbd "<f8>") 'previous-error)
     (define-key go-mode-map (kbd "<f9>") 'arv/gobuild)
     (define-key go-mode-map (kbd "C-<f9>") 'arv/gotest)
     (define-key go-mode-map (kbd "M-<f9>") 'arv/goinstall)
     ))

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            ;; autocompletion. Depends on external command gocode
            (set (make-local-variable 'company-backends) '(company-go))
            (company-mode)
            ;; eldoc
            (go-eldoc-setup)))

;;; setup-go-mode.el ends here
