;;; -*- lexical-binding: t -*-

(use-package
  org-roam
  :straight t
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n j" . org-roam-dailies-capture-today))
  :custom (org-roam-v2-ack t)
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new
      (file+head "${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "** %<%H:%M> %?"
      :target (file+head+olp "%<%Y-%m>.org"
							 "#+TITLE: %<%Y-%m>\n#+STARTUP: overview\n"
							 ("%<%B %d, %Y>"))
      :jump-to-captured t)))
  :config
  (org-roam-db-autosync-mode 1)
  (use-package org-roam-protocol)
  (org-roam-setup)
  )

;; (use-package
;;   org-roam-ui
;;   :straight t
;;   :custom
;;   (org-roam-ui-sync-theme t)
;;   (org-roam-ui-follow t)
;;   (org-roam-ui-update-on-save t)
;;   (org-roam-ui-open-on-start t))

(use-package
  deft
  :straight t
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

;; Journal
;; (use-package
;;   org-journal
;;   :straight t
;;   :bind
;;   ("C-c n j" . org-journal-new-entry)
;;   :preface
;;   (defun org-journal-file-header-func (time)
;; 	"Custom function to create journal header."
;; 	(let ((formatted-time
;;            (format-time-string (pcase org-journal-file-type
;;                                  ('daily "%Y-%m-%d")
;;                                  ('weekly "%Y-%V")
;;                                  ('monthly "%Y-%m")
;;                                  ('yearly "%Y"))
;;                                time)))
;;       (concat "#+TITLE: " formatted-time "\n#+STARTUP: overview")))
;;   :custom (org-journal-file-type 'monthly)
;;   ;; integrate every journal to agenda-files automatically
;;   ;; (org-journal-enable-agenda-integration t)
;;   (org-journal-file-header 'org-journal-file-header-func)
;;   (org-journal-date-prefix "* ")
;;   (org-journal-time-prefix "** ")
;;   (org-journal-date-format "%B %d, %Y")
;;   (org-journal-file-format "%Y-%m.org")
;;   (org-journal-carryover-items ""))
(use-package anki-editor :straight (:host github :repo "anki-editor/anki-editor"))
(use-package org-noter :straight t)
(use-package
  org-media-note
  :straight (:host github :repo "yuchen-lea/org-media-note")

  ;; :hook (org-mode . org-media-note-mode)
  ;; :bind
  ;; (("H-v" . org-media-note-hydra/body))
  :custom (org-media-note-use-org-ref t) (org-media-note-use-refcite-first t))

(use-package
  hledger-mode
  :straight t
  :bind ("C-c n h" . hledger-jentry)
  :mode ("\\.hlg\\'")
  :commands (hledger-jentry)
  :hook
  ((hledger-mode
	.
	(lambda () (add-hook 'completion-at-point-functions 'hledger-completion-accounts))))
  :preface
  (defun hledger-completion-accounts ()
	(when-let ((bounds
				(and (boundp 'hledger-accounts-cache) (bounds-of-thing-at-point 'symbol))))
      (list (car bounds) (point) hledger-accounts-cache))))

(use-package
  hledger-input
  :hook
  ((hledger-input-post-commit . hledger-show-new-balances)
   (hledger-input-mode . auto-fill-mode)
   (hledger-input-mode
	.
	(lambda ()
      (make-local-variable 'company-idle-delay)
      (setq-local company-idle-delay 0.1))))
  :preface
  (defun popup-balance-at-point ()
	"Show balance for account at point in a popup."
	(interactive)
	(if-let ((account (thing-at-point 'hledger-account)))
		(message (hledger-shell-command-to-string (format " balance -N %s " account)))
      (message "No account at point")))
  :custom (hledger-input-buffer-height 20))
