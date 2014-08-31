;;; grin.el --- customization for grin.el

;; $Id$


;;; Commentary:
;;

;;; Local keybindings:
;;

;;; History:
;;


;;; Code:

(require 'grin)

;; the default `grin' implementation performs the search in the
;; current directory. The `grin' function defined here wraps the real
;; `grin' asking for a directory to search from. It's a bit hackish
;; but it works.

(unless (boundp 'arv/grin--real-grin)
  (setq arv/grin--real-grin (symbol-function 'grin))

  (defun grin ()
    (interactive)
    (let ((default-directory (ido-read-directory-name "Directory: " nil nil t)))
      (funcall arv/grin--real-grin))))

;;; grin.el ends here
