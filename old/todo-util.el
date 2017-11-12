;;; todo-util.el --- configuracio de autopair

;;; Commentary:
;;


;;; History:
;;


;;; Code:

;;;###autoload
(defun arv-switch-to-todo-or-visit-todo-file ()
  (interactive)
  (switch-to-buffer (or (get-buffer "TODO")
                        (call-interactively 'find-file))))

(provide 'todo-util)

;;; todo-util.el ends here
