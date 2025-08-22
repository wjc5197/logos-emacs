;;; -*- lexical-binding: t -*-

;; startup time
(defun +display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'+display-startup-time)

(defvar +init-files
  (list
   'init-custom-fn
   'init-custom-macro
   'init-custom-var
   'init-pacman
   'init-modal
   'init-complete
   ))


(let ((init-directory (expand-file-name "lisp/" user-emacs-directory)))
  (dolist (file +init-files)
    (when file
      (load-file (concat init-directory (symbol-name file) ".el")))))

(setq
 debug-on-error t
 )
(meow-global-mode)
