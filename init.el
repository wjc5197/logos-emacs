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
   ;; 'init-test

   'init-pacman
   'init-inspect
   'init-theme
   'init-font
   'init-text
   'init-sym
   'init-file
   'init-buffer
   'init-modeline
   'init-shell
   'init-tabbar
   'init-window

   'init-clipboard
   'init-complete
   'init-diff
   'init-highlight
   'init-history
   'init-ime
   'init-modal
   'init-spell
   'init-vc

   'init-project
   'init-doc
   'init-agenda
   'init-note
   'init-cite
   'init-crypt
   'init-prog
   'init-ai
   'init-browse
   'init-chat
   'init-feed))

(let ((init-directory (expand-file-name "lisp/" user-emacs-directory)))
  (dolist (file +init-files)
    (when file
      (load-file (concat init-directory (symbol-name file) ".el")))))

(require 'server)
(unless (server-running-p)
    (server-start))
(put 'magit-clean 'disabled nil)
