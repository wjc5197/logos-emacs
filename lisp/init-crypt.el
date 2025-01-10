;;; -*- lexical-binding: t -*-

;; [org-crypt]
(use-package
  org-crypt
  :after org
  :bind (:map org-mode-map ("C-c o d" . org-decrypt-entry))
  :init
  ;; Auto-encrypt on save
  (org-crypt-use-before-save-magic)
  :custom (org-crypt-disable-auto-save t) (org-crypt-key +crypt-key))
