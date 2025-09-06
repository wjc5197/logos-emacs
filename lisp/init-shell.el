;;; -*- lexical-binding: t -*-

;; [eshell] Emacs command shell
(use-package
  esh-mode
  :defines eshell-prompt-function
  :functions eshell/alias
  :hook
  ((eshell-mode . +eshell--set-window-ui-h)
   (eshell-first-time-mode . +eshell/define-alias))
  :bind
  (
   ;; ("C-`" . +eshell-toggle)
   :map
   eshell-mode-map ("C-l" . eshell/clear) ("M-h" . consult-history))
  ;;  :preface
  ;;  (defun +eshell-toggle (&optional arg)
  ;;    "Toggle a persistent eshell popup window.
  ;; If popup is visible but unselected, select it.
  ;; If popup is focused, kill it."
  ;;    (interactive "P")
  ;;    (if arg
  ;;        (gptel "GPT: gpt-popup")
  ;;      (require 'eshell)
  ;;      (if-let ((win (get-buffer-window "*Eshell-pop*")))
  ;;        (if (eq (selected-window) win)
  ;;            ;; If users attempt to delete the sole ordinary window. silence it.
  ;;            (ignore-errors
  ;;              (delete-window win))
  ;;          (select-window win))
  ;;        (let ((display-comint-buffer-action
  ;;               '(display-buffer-at-bottom (inhibit-same-window . nil)))
  ;;              (eshell-buffer-name "*Eshell-pop*"))
  ;;          (with-current-buffer (eshell)
  ;;            (add-hook 'eshell-exit-hook
  ;;                      #'(lambda ()
  ;;                          (ignore-errors
  ;;                            (delete-window win)))
  ;;                      nil t)))))
  :preface
  (defun +eshell--set-window-ui-h ()
	(set-window-fringes nil 0 0)
	(set-window-margins nil 1 nil)
	(set-display-table-slot standard-display-table 0 ?\ ))
  (defun +eshell/define-alias ()
	"Define alias for eshell"
	;; Aliases
	(defalias 'eshell-f 'find-file)
	(defalias 'eshell-fo 'find-file-other-window)
	(defalias 'eshell-d 'dired)
	(eshell/alias "l" "ls -lah $*")
	(eshell/alias "ll" "ls -laG $*")
	(defalias 'eshell-q 'eshell/exit)
	(eshell/alias "rg" "rg --color=always $*")
	(defalias 'eshell-clear 'eshell/clear-scrollback)
	;; Vim
	(defalias 'eshell-vim 'find-file)
	(defalias 'eshell-vi 'find-file)
	;; Git
	(eshell/alias "git" "git $*")
	(eshell/alias "gst" "git status $*")
	(eshell/alias "ga" "git add $*")
	(eshell/alias "gc" "git commit $*")
	(eshell/alias "gp" "git push $*")
	(eshell/alias "gb" "git branch $*")
	(eshell/alias "gch" "git checkout $*")
	(eshell/alias "gcb" "git checkout -b $*"))

 ;;; A bunch of eshell functions
  ;; [clear]
  (defun eshell/clear ()
	"Clear the eshell buffer."
	(interactive)
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (eshell-send-input)))

  ;; [emacs, e, ec, ecc]
  (defun eshell/emacs (&rest args)
	"Open a file (ARGS) in Emacs."
	(if (null args)
		;; If I just ran "emacs"
		(bury-buffer)
	  ;; We have to expand the file names or else naming a directory in an
	  ;; argument causes later arguments to be looked for in that directory,
	  ;; not the starting directory
	  (mapc #'find-file (mapcar #'expand-file-name (flatten-tree (reverse args))))))

  (defalias 'eshell/e #'eshell/emacs)
  (defalias 'eshell/ec #'eshell/emacs)
  (defalias 'eshell/ecc #'eshell/emacs)

  ;; [ebc]
  (defun eshell/ebc (&rest args)
	"Compile a file (ARGS) in Emacs. Use `compile' to do background make."
	(if (eshell-interactive-output-p)
		(let ((compilation-process-setup-function
			   (list
				'lambda nil
				(list
				 'setq 'process-environment (list 'quote (eshell-copy-environment))))))
		  (compile (eshell-flatten-and-stringify args))
		  (pop-to-buffer compilation-last-buffer))
	  (throw 'eshell-replace-command
			 (let ((l (eshell-stringify-list (flatten-tree args))))
			   (eshell-parse-command (car l) (cdr l))))))

  ;; [less, more]
  (defun eshell/less (&rest args)
	"Invoke `view-file' on a file (ARGS).
\"less +42 foo\" will go to line 42 in the buffer for foo."
	(while args
	  (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
		  (let* ((line (string-to-number (match-string 1 (pop args))))
				 (file (pop args)))
			(+eshell-view-file file)
			(forward-line line))
		(+eshell-view-file (pop args)))))
  (defalias 'eshell/more #'eshell/less)

  ;; [bat]
  (defun eshell/bat (file)
	"cat FILE with syntax highlight."
	(with-temp-buffer
	  (insert-file-contents file)
	  (let ((buffer-file-name file))
		(delay-mode-hooks
		  (set-auto-mode)
		  (font-lock-ensure)))
	  (buffer-string)))

  ;; [bd]
  (defun eshell/bd ()
	"cd to parent directory with completions."
	(let ((dir default-directory)
		  dirs)
	  (while (not (string-empty-p dir))
		(push (file-name-directory dir) dirs)
		(setq dir (substring dir 0 -1)))
	  (let ((dir (completing-read "Directory: " dirs nil t)))
		(eshell/cd dir))))

  ;; view file
  (defun +eshell-view-file (file)
	"View FILE.  A version of `view-file' which properly rets the eshell prompt."
	(interactive "fView file: ")
	(unless (file-exists-p file)
	  (error "%s does not exist" file))
	(let ((buffer (find-file-noselect file)))
	  (if (eq (get (buffer-local-value 'major-mode buffer) 'mode-class) 'special)
		  (progn
			(switch-to-buffer buffer)
			(message "Not using View mode because the major mode is special"))
		(let ((undo-window
			   (list
				(window-buffer) (window-start)
				(+ (window-point) (length (funcall eshell-prompt-function))))))
		  (switch-to-buffer buffer)
		  (view-mode-enter
		   (cons (selected-window) (cons nil undo-window)) 'kill-buffer)))))
  :custom
  ;; banner
  (eshell-banner-message "")

  ;; scrolling
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all)

  ;; exit
  (eshell-kill-processes-on-exit t)
  (eshell-hist-ignoredups t)

  (eshell-input-filter #'eshell-input-filter-initial-space)

  ;; em-glob
  (eshell-glob-case-insensitive t)
  (eshell-error-if-no-glob t)

  ;; prefer eshell functions
  (eshell-prefer-lisp-functions t)

  ;; Visual commands require a proper terminal. Eshell can't handle that
  (eshell-visual-commands '("top" "htop" "less" "more" "bat" "talnet"))
  (eshell-visual-subcommands '(("git" "help" "lg" "log" "diff" "show")))

  ;; Completion like bash
  (eshell-cmpl-ignore-case t)
  (eshell-cmpl-cycle-completions nil)
  :config
  ;; Don't auto-write our aliases! Let us manage our own `eshell-aliases-file' via elisp
  (advice-add #'eshell-write-aliases-list :override #'ignore)
  (put 'eshell/ebc 'eshell-no-numeric-conversions t))

;; [esh-syntax-highlighting] Fish-like syntax highlighting
(use-package
  eshell-syntax-highlighting
  :straight t
  :after eshell
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

;; [esh-help] `eldoc' support
(use-package
  esh-help
  :straight t
  :after eshell
  :hook (eshell-mode . setup-esh-help-eldoc))

;; [eshell-z] `cd' to frequent directory in `eshell'
(use-package eshell-z :straight t :after eshell :commands (eshell/z))

;; [eshell-up] Quickly navigating to a specific parent directory in eshell
(use-package
  eshell-up
  :straight t
  :after eshell
  :commands (eshell-up eshell-up-peek)
  :custom (eshell-up-ignore-case nil))

;; [vterm]
(use-package vterm
  :straight t
  :preface
  (defun old-version-of-vterm--get-color (index &rest args)
	"This is the old version before it was broken by commit
https://github.com/akermu/emacs-libvterm/commit/e96c53f5035c841b20937b65142498bd8e161a40.
Re-introducing the old version fixes auto-dim-other-buffers for vterm buffers."
	(cond
	 ((and (>= index 0) (< index 16))
	  (face-foreground
	   (elt vterm-color-palette index)
	   nil 'default))
	 ((= index -11)
	  (face-foreground 'vterm-color-underline nil 'default))
	 ((= index -12)
	  (face-background 'vterm-color-inverse-video nil 'default))
	 (t
	  nil)))
  :config
  (advice-add 'vterm--get-color :override #'old-version-of-vterm--get-color)
  )
