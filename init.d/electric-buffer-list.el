;;; electric-buffer-list.el --- configuracio de electric-buffer-list
;;; $Id$

;;; Commentary:
;;


;;; History:
;;


;;; Code:

(eval-after-load "ebuff-menu"
  '(progn
     (arv/generate-lab-faces "electric-buffer" 15)
     (setq buffer-menu-buffer-font-lock-keywords
           '(
             (".*Dired.*"             . 'electric-buffer-01-face) ; Dired
             ("^....[*]shell.*"       . 'electric-buffer-02-face) ; shell buff
             (".*[*]scratch[*].*"     . 'electric-buffer-03-face) ; scratch buffer
             ("^.*[*]svn-.*"          . 'electric-buffer-04-face) ; svn buffers
             ("^....[*].*"            . 'electric-buffer-05-face) ; "*" named buffers
             ("^..[*].*"              . 'electric-buffer-06-face) ; Modified
             ("^.[%].*"               . 'electric-buffer-07-face) ; Read only
             ("^....[*]Man .*Man.*"   . 'electric-buffer-08-face) ; Man page
             ))))

(defun buffer-menu-custom-font-lock  ()
  (let ((font-lock-unfontify-region-function
         (lambda (start end)
           (remove-text-properties start end '(font-lock-face nil)))))
    (font-lock-unfontify-buffer)
    (set (make-local-variable 'font-lock-defaults)
         '(buffer-menu-buffer-font-lock-keywords t))
    (font-lock-fontify-buffer)))

(add-hook 'buffer-menu-mode-hook 'buffer-menu-custom-font-lock)

;;; electric-buffer-list.el ends here
