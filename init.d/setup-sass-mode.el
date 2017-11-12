;;; setup-sass-mode.el --- customization for sass-mode

;; $Id$


;;; Commentary:
;;

;;; Local keybindings:
;;
;; C-M-q: does some amazing stuff

;;; History:
;;


;;; Code:

(require 'format-spec)
(require 'sass-mode)

(defvar sass-project-root nil "Arrel del projecte.")
(defvar sass-project-main-file nil "Arxius arrel del projecte.")
(defvar sass-project-output-file nil "Arxiu de sortida.")
(defvar sass-command "sass --style compressed %s %d"
  "Comanda per executar sass.

Permet interpolar alguns elements:

- %r: directori arrel del projecte
- %s: ruta absoluta de l'arxiu origen (source)
- %d: ruta absoluta de l'arxiu de sortida (dest)
")

(defun arv/sass-compile-on-save ()
  "Compile on save.

When a file with extension '.scss' is saved under the
`sass-project-root' directory the sass preprocessos is executed
on the file `sass-project-main-file' and the output saved in the
`sass-project-output-file'."
  (if (and (string-match-p "\.scss$" (buffer-file-name))
           (s-starts-with-p sass-project-root (buffer-file-name)))
      (compilation-start
       (format-spec sass-command
                    (list
                     (cons ?r sass-project-root)
                     (cons ?s (concat sass-project-root "/" sass-project-main-file))
                     (cons ?r (concat sass-project-root "/" sass-project-output-file))
                     ))
       t
       (lambda (mode) "*sass*"))))

(add-hook 'after-save-hook 'arv/sass-compile-on-save)

;;; setup-sass-mode.el ends here
