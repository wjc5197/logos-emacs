;;; -*- lexical-binding: t -*-

(setq
 ;; [kill-ring]
 ;; Do not add the duplicates that the same as the last one to kill-ring
 kill-do-not-save-duplicates t
 ;; Save clipboard contents into kill-ring before replace them
 save-interprogram-paste-before-kill t)

(use-package
 easy-kill
 :straight t
 :bind (([remap kill-ring-save] . easy-kill) ([remap mark-sexp] . easy-mark)))
