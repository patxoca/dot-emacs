;;; setup-projectile.el --- customization for projectile

;; $Id$


;;; Commentary:
;;

;;; History:
;;


;;; Code:

(require 'projectile)

;; native indexing is slower but allows for file/directory filtering
(setq projectile-indexing-method 'native)

;; enable caching in order to speedup native indexing
(setq projectile-enable-caching t)

(setq projectile-mode-line '(:eval (format " PRJ[%s]" (projectile-project-name))))

(projectile-global-mode)

;;; setup-projectile.el ends here
