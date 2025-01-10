;; -*- lexical-binding: t; -*-

;; [ediff] Diff & patch
(use-package
 ediff
 :hook
 ((ediff-before-setup . +ediff-save-window-config)
  ((ediff-quit ediff-suspend) . +ediff-restore-window-config))
 :preface
 ;; Restore window config after quitting ediff
 (defvar +ediff-saved-window-config nil)
 (defun +ediff-save-window-config ()
   (setq +ediff-saved-window-config (current-window-configuration)))
 (defun +ediff-restore-window-config ()
   (when (window-configuration-p +ediff-saved-window-config)
     (set-window-configuration +ediff-saved-window-config)))
 :custom
 (ediff-window-setup-function 'ediff-setup-windows-plain)
 (ediff-split-window-function 'split-window-horizontally)
 (ediff-merge-split-window-function 'split-window-horizontally)
 (ediff-highlight-all-diffs t)
 ;; turn off whitespace checking
 (ediff-diff-options "-w"))
