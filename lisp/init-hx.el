;; -*- lexical-binding: t; -*-

;; [desktop] save partial status of Emacs when killed
;; (use-package
;;  desktop
;;  :hook (after-init . desktop-save-mode)
;;  :custom
;;  (desktop-auto-save-timeout 300)
;;  (desktop-path (list (expand-file-name "desktop" user-emacs-directory))))

;;; [recentf] recently visited files
(use-package
 recentf
 :bind (("C-x C-r" . recentf-open-files))
 :hook (after-init . recentf-mode)
 :hook
 ;; Add dired directories to recentf file list.
 (dired-mode . +dired--add-to-recentf-h)
 :preface
 (defun +dired--add-to-recentf-h ()
   (recentf-add-file default-directory))
 :custom (recentf-auto-cleanup 'never) (recentf-max-saved-items 200)
 (recentf-exclude
  (list
   "\\.?cache"
   ".cask"
   "url"
   "COMMIT_EDITMSG\\'"
   "bookmarks"
   "\\.?ido\\.last$"
   "\\.revive$"
   "/G?TAGS$"
   "/.elfeed/"
   "^/tmp/"
   "^/var/folders/.+$"
   "^/ssh:"
   (lambda (file) (file-in-directory-p file package-user-dir))
   (expand-file-name recentf-save-file)))
 :config (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
 ;; HACK: Text properties inflate the size of recentf's files, and there is
 ;; no purpose in persisting them (Must be first in the list!)
 (add-to-list 'recentf-filename-handlers #'substring-no-properties))

;;; [save-place-mode] save place lastly visited
(use-package
 saveplace
 :hook (after-init . save-place-mode)
 :config
 ;; HACK: `save-place-alist-to-file' uses `pp' to prettify the contents of its
 ;; cache, which is expensive and useless. replace it with `prin1'
 (+advice-pp-to-prin1! 'save-place-alist-to-file))

;; [saveplace-pdf-view] Recover last viewed position
(use-package
 saveplace-pdf-view
 :straight t
 :after pdf-tools
 ;; :if
 ;; (ignore-errors
 ;;   (pdf-info-check-epdfinfo)
 ;;   t)
 :autoload (saveplace-pdf-view-find-file-advice saveplace-pdf-view-to-alist-advice)
 :init
 (advice-add 'save-place-find-file-hook :around #'saveplace-pdf-view-find-file-advice)
 (advice-add 'save-place-to-alist :around #'saveplace-pdf-view-to-alist-advice))

;; [winner] Restore old window configurations
(use-package
 winner
 :hook (after-init . winner-mode)
 :custom (winner-dont-bind-my-keys t)
 (winner-boring-buffers
  '("*Completions*"
    "*Compile-Log*"
    "*inferior-lisp*"
    "*Fuzzy Completions*"
    "*Apropos*"
    "*Help*"
    "*cvs*"
    "*Buffer List*"
    "*Ibuffer*"
    "*esh command on file*")))

;;; [savehist] Save variables to file
;; (use-package
;;  savehist
;;  :hook (after-init . savehist-mode)
;;  :preface
;;  (defun +savehist--remove-string-properties-h ()
;;    (setq
;;     kill-ring (mapcar #'substring-no-properties (cl-remove-if-not #'stringp kill-ring))
;;     search-ring (mapcar #'substring-no-properties search-ring)
;;     regexp-search-ring (mapcar #'substring-no-properties regexp-search-ring)
;;     register-alist
;;     (cl-loop
;;      for
;;      (reg . item)
;;      in
;;      register-alist
;;      if
;;      (stringp item)
;;      collect
;;      (cons reg (substring-no-properties item))
;;      else
;;      collect
;;      (cons reg item))))
;;  :config
;;  (setq
;;   savehist-additional-variables
;;   '(mark-ring
;;     global-mark-ring search-ring regexp-search-ring
;;     ;kill-ring
;;     )
;;   savehist-autosave-interval 300)
;;
;;  (with-eval-after-load 'vertico
;;    (add-to-list 'savehist-additional-variables 'vertico-repeat-history))
;;
;;  ;; HACK: Remove text properties from variables to reduce savehist cache size.
;;  (add-hook savehist-save-hook #'+savehist--remove-string-properties-h))
