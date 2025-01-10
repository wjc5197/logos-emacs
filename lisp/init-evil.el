;;; -*- lexical-binding: t; -*-

(setq +evil-leader-states '(normal visual motion))

(use-package vundo :straight t :custom (vundo-compact-display t))

(use-package
 evil
 :straight t
 :init (evil-mode 1)
 :custom (evil-want-integration t)
 ;; no vim insert bindings
 (evil-want-keybinding nil)
 (evil-undo-system 'undo-redo)
 (evil-search-module 'evil-search)
 :config
 (evil-set-leader +evil-leader-states (kbd "SPC"))
 (evil-define-key
  +evil-leader-states
  'global
  (kbd "SPC TAB")
  (lambda ()
    (interactive)
    (switch-to-buffer (other-buffer)))
  (kbd "SPC SPC")
  'execute-extended-command
  (kbd "SPC aa")
  'org-agenda
  (kbd "SPC ac")
  'org-capture
  (kbd "SPC bb")
  'consult-buffer
  (kbd "SPC bd")
  'evil-delete-buffer
  (kbd "SPC eR")
  (lambda ()
    (interactive)
    (load user-init-file))
  (kbd "SPC ei")
  (lambda ()
    (interactive)
    (find-file-existing user-init-file))
  (kbd "SPC eI")
  (lambda ()
    (interactive)
    (find-file-existing early-init-file))
  (kbd "SPC ff")
  'find-file
  (kbd "SPC fj")
  'dired-jump
  (kbd "SPC fs")
  'save-buffer
  (kbd "SPC fS")
  'evil-write-all
  (kbd "SPC nd")
  'deft
  (kbd "SPC nf")
  'org-roam-node-find
  (kbd "SPC ng")
  'org-roam-graph
  (kbd "SPC nh")
  'hledger-jentry
  (kbd "SPC ni")
  'org-roam-node-insert
  (kbd "SPC nj")
  'org-journal-new-entry
  (kbd "SPC qq")
  (lambda ()
    (interactive)
    (save-some-buffers nil t)
    (kill-emacs))
  (kbd "SPC wd")
  'delete-window
  (kbd "q")
  nil
  (kbd "C-^")
  nil)

 (evil-define-key
  +evil-leader-states
  org-mode-map
  (kbd "SPC op")
  'org-latex-preview
  (kbd "SPC oi")
  'org-toggle-inline-images
  (kbd "SPC oy")
  'org-download-screenshot)

 (evil-define-key
  +evil-leader-states
  hledger-mode-map
  (kbd "SPC nb")
  'popup-balance-at-point
  (kbd "SPC nr")
  'hledger-run-command))

(use-package evil-collection :straight t :after evil :init (evil-collection-init))

;; [avy] Jump with several key strock
(use-package avy :straight t)

;; [avy-pinyin] Avy support for pinyin
(use-package ace-pinyin :straight t :after avy :init (ace-pinyin-global-mode t))

;; [avy-link] Avy support for links
(use-package
 ace-link
 :straight t
 :after avy
 :bind (("C-, j" . ace-link))
 :init (ace-link-setup-default (kbd "C-, j")))
