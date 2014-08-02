;;; buffer-move.el --- configuracio de buffer-mode
;;; $Id$

;;; Commentary:
;;


;;; History:
;;


;;; Code:

(autoload 'buf-move-up    "buffer-move" "" t nil)
(autoload 'buf-move-down  "buffer-move" "" t nil)
(autoload 'buf-move-left  "buffer-move" "" t nil)
(autoload 'buf-move-right "buffer-move" "" t nil)

(global-set-key [(control shift up)]     'buf-move-up)
(global-set-key [(control shift down)]   'buf-move-down)
(global-set-key [(control shift left)]   'buf-move-left)
(global-set-key [(control shift right)]  'buf-move-right)

;;; buffer-move.el ends here
