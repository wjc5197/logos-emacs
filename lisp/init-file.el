;; -*- lexical-binding: t; -*-

(setq

 ;; [lockfile]
 create-lockfiles nil

 ;; [backup] Use auto-save, which maintains a copy when a buffer is unsaved
 make-backup-files nil

 ;; [version-control]
 ;; In case I enable it later
 ;; version-control t
 ;; backup-by-copying t
 ;; delete-old-versions t
 ;; kept-new-versions 5
 ;; tramp-backup-directory-alist backup-directory-alist

 ;; Always follow link when visiting a [symbolic link]
 find-file-visit-truename t
 vc-follow-symlinks t)

;; [autorevert] TODO: Add hooks as what doom has done?
(use-package
 autorevert
 :hook (after-init . global-auto-revert-mode)
 :custom
 ;; Only prompts for confirmation when buffer is unsaved.
 (revert-without-query (list ".")))

(use-package
 auto-save
 :custom
 auto-save-default
 t
 auto-save-include-big-deletions
 t ; Don't auto-disable auto-save after deleting big chunks.
 auto-save-list-file-prefix
 (expand-file-name "autosaves/" user-emacs-directory)
 auto-save-file-name-transforms
 (list
  (list
   "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
   ;; Prefix tramp autosaves to prevent conflicts with local ones
   (concat auto-save-list-file-prefix "tramp-\\2") t)
  (list ".*" auto-save-list-file-prefix t)))

;; [dired] File manager
(use-package
 dired
 :custom
 ;; Always delete and copy recursively
 (dired-recursive-deletes 'top) (dired-recursive-copies 'always)
 ;; Move between two dired buffer quickly
 (dired-dwim-target t)
 ;; Ask whether destination dirs should get created when copying/removing files.
 (dired-create-destination-dirs 'ask)
 ;; don't prompt to revert, just do it
 (dired-auto-revert-buffer #'dired-buffer-stale-p)
 ;; symlink
 (dired-hide-details-hide-symlink-targets nil))

;; [dired-git-info] Show git info in dired
(use-package
 dired-git-info
 :straight t
 :after dired
 :bind (:map dired-mode-map ("g" . dired-git-info-mode) ("r" . revert-buffer))
 :custom
 (dgi-commit-message-format "%h %cs %s")
 (dgi-auto-hide-details-p nil))

;; [dired-aux] Extra Dired functionality
(use-package
 dired-aux
 :after dired
 :custom
 (dired-create-destination-dirs 'ask)
 (dired-vc-rename-file t))

;; [dired-x] Extra Dired functionality
(use-package
 dired-x
 :after dired
 :bind (:map dired-mode-map ("." . dired-omit-mode))
 :custom
 (dired-guess-shell-alist-user
  (let ((cmd
         (if (display-graphic-p)
             (+code-based-on-system
              '("start" ;; Windows
                "xdg-open" ;; Linux
                "open" ;; macOS
                "")) ;; Android
           "")))
    `((("\\.pdf\\'" ,cmd)
       ("\\.docx\\'" ,cmd)
       ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
       ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
       ("\\.\\(?:xcf\\)\\'" ,cmd)
       ("\\.csv\\'" ,cmd)
       ("\\.tex\\'" ,cmd)
       ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
       ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)))))
 (dired-omit-verbose nil)
 ;; hide dot files
 (dired-omit-files "^\\..*\\'")
 ;; Disable the prompt about killing the Dired buffer for a deleted directory.
 (dired-clean-confirm-killing-deleted-buffers nil))

;; [fd-dired] Using `fd' with `find-dired'
(use-package fd-dired :straight t :bind ([remap find-dired] . fd-dired))

;; [dired-hacks] Several additional extensions for dired
(use-package
 dired-hacks
 :straight (:files (:defaults "*.el"))
 :after dired
 :init
 (use-package
  dired-subtree
  :bind (:map dired-mode-map ("TAB" . dired-subtree-toggle))
  :custom (dired-subtree-line-prefix " | ")))

;; [diredfl] Make dired colorful
(use-package diredfl :straight t :hook (dired-mode . diredfl-mode))

;; [rg] support for ripgrep
(use-package rg :straight t)

;; [wgrep] Edit a grep buffer and apply changes to the file buffer
(use-package wgrep :straight t :custom (wgrep-auto-save-buffer t))
