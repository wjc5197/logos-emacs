;;; -*- lexical-binding: t -*-

(use-package
  org-agenda
  :bind ("C-c a" . org-agenda)
  :hook ((org-after-todo-state-change . +handle-todo-state-change)
		 (org-after-todo-statistics . +org-summary-todo))
  :preface
  (defun +log-todo-created-date ()
	"Log TODO created time in the property drawer under key 'CREATED'."
	(when (string= (org-get-todo-state) "TODO")
      (if (not (org-entry-get nil "CREATED"))
          (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (if (org-entry-get nil "ACTIVATED")
          (org-entry-delete nil "ACTIVATED"))))

  (defun +log-next-activated-date ()
	"Log NEXT activated time in the property drawer under key 'ACTIVATED'."
	(when (and (string= (org-get-todo-state) "NEXT") (not (org-entry-get nil "ACTIVATED")))
      (let ((current-time (format-time-string "[%Y-%m-%d %a %H:%M]")))
		(unless (org-entry-get nil "CREATED")
          (org-entry-put nil "CREATED" current-time))
		(org-entry-put nil "ACTIVATED" current-time))))

  (defun +remove-created-and-activated ()
	"Remove CREATED and ACTIVATED properties when changing to empty state."
	(when (string= (org-get-todo-state) nil)
      (org-entry-delete nil "CREATED")
      (org-entry-delete nil "ACTIVATED")))

  (defun +handle-todo-state-change ()
	"Handle state changes for TODO entries."
	(+log-todo-created-date)
	(+log-next-activated-date)
	(+remove-created-and-activated))

  (defun +org-summary-todo (n-done n-not-done)
	"Switch entry to DONE when all subentries are done, to TODO otherwise."
	(let (org-log-done org-todo-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  :custom (org-log-done 'time) (org-log-repeat 'time)
  ;; if (org-log-done 'note), shall set org-log-note-headings
  (org-refile-use-outline-path 'file) (org-outline-path-complete-in-steps nil)
  (org-refile-targets
   `((,+inbox-file :maxlevel . 1)
     (,+desire-file :maxlevel . 2)
     (,+known-file :maxlevel . 2)))
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "FAIL(f)")))
  (org-agenda-custom-commands
   '(("d" "Daily Agenda"
      ((agenda
		""
		((org-agenda-span 'day)
         (org-agenda-start-on-weekday nil)
         (org-agenda-start-day "+0d")))
       (todo "NEXT" ((org-agenda-overriding-header "Next")))
       (todo "TODO" ((org-agenda-overriding-header "Todo")))))))
  :config (add-to-list 'org-agenda-files org-agenda-dir)
  (let ((state-entry (assq 'state org-log-note-headings)))
	(when state-entry
      (setcdr state-entry "State %s -> %S at %t"))))

;; (use-package org-super-agenda :straight t :hook (org-agenda-mode . org-super-agenda-mode))

;; Capture
(use-package
  org-capture
  :custom
  (org-capture-templates `(("i" "inbox" entry (file ,+inbox-file) ,(concat "* %?")))))

;; Pomodoro
(use-package
  org-pomodoro
  :straight t
  :bind
  (:map
   org-mode-map
   ("C-c o p" . org-pomodoro))
  :custom
  (org-pomodoro-keep-killed-pomodoro-time t)
  (org-pomodoro-format "🍅 %s")
  (org-pomodoro-overtime-format "💆 %s")
  (org-pomodoro-short-break-format "☕ %s")
  (org-pomodoro-long-break-format "💤 %s")
  ;; :custom-face
  ;; (org-pomodoro-mode-line ((t (:foreground "#ffffff"))))
  ;; (org-pomodoro-mode-line-break ((t (:foreground "#13a10e"))))
  )

;; (use-package
;;  pomidor
;;  :bind (("<f12>" . pomidor))
;;  :config
;;  (defun pomidor-mode-line ()
;;    "Calculate what string should appear in the modeline."
;;    (if (not (pomidor-running-p))
;;        nil
;;      (cond
;;       ((pomidor-overwork-p)
;;        " 💆 TAKE BREAK")
;;       ((pomidor-break-over-p)
;;        " 🙅 BREAK OVER")
;;       ((pomidor-break-duration)
;;        (format-time-string " ☕️ %M:%S"
;;                            (time-subtract
;;                             (seconds-to-time
;;                              pomidor-break-seconds)
;;                             (pomidor-break-duration))
;;                            t))
;;       (t
;;        (format-time-string " 🍅 %M:%S" (pomidor-total-duration) t)))))
;;  (defun pomidor-silent ()
;;    (setq pomidor-sound-tick nil)
;;    (setq pomidor-sound-tack nil))
;;  (defun pomidor-unsilent ()
;;    (setq pomidor-sound-tick
;;          (expand-file-name (concat pomidor-dir "tick.wav")))
;;    (setq pomidor-sound-tack
;;          (expand-file-name (concat pomidor-dir "tack.wav"))))
;;  (setq
;;   pomidor-sound-tick nil
;;   pomidor-sound-tack nil)
;;  (add-to-list 'global-mode-string '(:eval (pomidor-mode-line)) t)
;;  (add-to-list 'evil-emacs-state-modes 'pomidor-mode)
;;  :hook
;;  (pomidor-mode
;;   .
;;   (lambda ()
;;     (display-line-numbers-mode -1)
;;     (setq
;;      left-fringe-width 0
;;      right-fringe-width 0)
;;     (setq
;;      left-margin-width 2
;;      right-margin-width 0)
;;     ;; force fringe update
;;     (set-window-buffer nil (current-buffer)))))
