;;; -*- lexical-binding: t -*-

(setq
 ;; Larger process output buffer for LSP module
 read-process-output-max (* 3 1024 1024))

(use-package
  hideshow
  :bind (:map
		 hs-minor-mode-map
		 (("C-c @ c" . +hs-cycle)
		  ("C-c @ t" . +hs-toggle-all)))
  :hook (prog-mode . hs-minor-mode)
  :preface
  ;; More functions
  ;; @see https://karthinks.com/software/simple-folding-with-hideshow/
  (defun +hs-cycle (&optional level)
	(interactive "p")
	(let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
			('+hs-cycle (hs-hide-level 1) (setq this-command 'hs-cycle-children))
			('hs-cycle-children
             (save-excursion (hs-show-block)) (setq this-command 'hs-cycle-subtree))
			('hs-cycle-subtree (hs-hide-block))
			(_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
		(hs-hide-level level)
		(setq this-command 'hs-hide-level))))

  (defun +hs-toggle-all ()
	"Toggle hide/show all."
	(interactive)
	(pcase last-command
      ('+hs-toggle-all (save-excursion (hs-show-all)) (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

  ;; Display line counts
  (defun +hs-display-code-line-counts (ov)
	"Display line counts when hiding codes."
	(when (eq 'code (overlay-get ov 'hs))
      (overlay-put
       ov 'display
       (let ((lines (number-to-string (count-lines (overlay-start ov) (overlay-end ov)))))
         (concat
          " "
          (propertize (concat " ... L" lines " ")
                      'face
                      '(:inherit shadow :height 0.8 :box t))
          " ")))))
  :custom (hs-set-up-overlay #'+hs-display-code-line-counts))

;; [display-fill-column-indicator] Show a line at 80 char
;; (use-package
;;  display-fill-column-indicator
;;  :hook (prog-mode . display-fill-column-indicator-mode))

;; [mwim] Better C-a C-e for programming
(use-package
  mwim
  :straight t
  :bind
  (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
   ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; [compile]
(use-package
  compile
  :custom
  (compilation-always-kill t) ; kill compilation process before starting another
  (compilation-ask-about-save nil) ; save all buffers on `compile'
  (compilation-scroll-output 'first-error)

  :config
  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of Emacs.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))

;; [xref] Cross reference
(use-package
  xref
  :custom
  (xref-search-program 'ripgrep)
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  (xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (xref-history-storage 'xref-window-local-history)

  ;; (defadvice! +xref--push-marker-stack-a (&rest rest)
  ;;             :before '(find-function consult-imenu consult-ripgrep citre-jump)
  ;;             (xref-push-marker-stack (point-marker)))
  )

;; [imenu]
(use-package imenu :custom (imenu-auto-rescan t))

;; [Eglot] LSP support
(use-package
  eglot
  :hook
  ((c-mode
	c-ts-mode
	c++-mode
	c++-ts-mode
	clojure-mode
	go-mode
	haskell-mode
	java-mode
	js-mode
	lua-mode
	nix-mode
	python-mode
	python-ts-mode
	racket-mode
	rust-mode
	rust-ts-mode
	scala-mode
	typescript-mode
	typst-ts-mode
	zig-mode
	)
   . eglot-ensure)
  ;; :custom-face (eglot-highlight-symbol-face ((t (:underline t))))
  :bind
  (:map
   eglot-mode-map
   ("M-RET" . eglot-code-actions)
   ("M-/" . eglot-find-typeDefinition))
  :preface
  (defun +jdtls-command-contact (&optional interactive)
	(let* ((jdtls-cache-dir (file-name-concat user-emacs-directory ".cache" "lsp-cache"))
           (project-dir (file-name-nondirectory (directory-file-name (project-root (project-current)))))
           (data-dir (expand-file-name (file-name-concat jdtls-cache-dir (md5 project-dir))))
           (jvm-args `("-Xmx8G"
                       ;; "-XX:+UseG1GC"
                       "-XX:+UseZGC"
                       "-XX:+UseStringDeduplication"
                       ;; "-XX:FreqInlineSize=325"
                       ;; "-XX:MaxInlineLevel=9"
                       "-XX:+UseCompressedOops"))
           (jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg)) jvm-args))
           ;; tell jdtls the data directory and jvm args
           (contact (append '("jdtls") jvm-args `("-data" ,data-dir))))
      contact))
  :custom
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-connect-timeout 10)
  (eglot-autoshutdown t)
  (eglot-report-progress 'messages)
  :config
  (push '(java-mode . +jdtls-command-contact) eglot-server-programs)
  (push '(typst-ts-mode "tinymist") eglot-server-programs)
  ;; (push '(sql-mode "sqls") eglot-server-programs)
  ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; eglot has it's own strategy by default
  ;; (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly
  ;;             completion-at-point-functions (cl-nsubst
  ;;                                            (cape-capf-noninterruptible
  ;;                                             (cape-capf-buster #'eglot-completion-at-point
  ;;                                                               #'string-prefix-p))
  ;;                                            'eglot-completion-at-point
  ;;                                            completion-at-point-functions)
  ;;             )

  ;; ;; we call eldoc manually by C-h .
  ;; (add-hook eglot-managed-mode-hook
  ;;            (defun +eglot-disable-eldoc-mode ()
  ;;              (when (eglot-managed-p)
  ;;                (eldoc-mode -1))))
  )

(use-package
  eglot-tempel
  :straight t
  :after (eglot tempel)
  :hook (after-init eglot-tempel-mode))

(use-package
  eglot-x
  :straight (:host github :repo "nemethf/eglot-x")
  :hook (eglot-managed-mode . eglot-x-setup))

;; [Eldoc]
(use-package
  eldoc
  :bind (("C-h ." . eldoc))
  :custom
  (eldoc-echo-area-display-truncation-message t)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)
  (eglot-extend-to-xref t)
  )

;; [help]
;; (use-package help :bind (("s-?" . display-local-help)))

;; [consult-eglot] Eglot support for consult
(use-package
  consult-eglot
  :after
  consult
  eglot
  :straight t
  :bind (:map eglot-mode-map ([remap xref-find-apropos] . consult-eglot-symbols)))

;; [dumb-jump] Jump to definition (integrated with xref, a fallback of lsp)
(use-package
  dumb-jump
  :straight t
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read)
  (dumb-jump-aggressive t)
  (dumb-jump-default-project user-emacs-directory))

;; [citre] Ctags-infra
(use-package
  citre
  :straight t
  :bind
  (:map
   prog-mode-map
   ("C-c c j" . +citre-jump)
   ("C-c c k" . +citre-jump-back)
   ("C-c c p" . citre-peek)
   ("C-c c a" . citre-ace-peek)
   ("C-c c u" . citre-update-this-tags-file))
  :preface
  (defun +citre-jump ()
	"Jump to the definition of the symbol at point. Fallback to `xref-find-definitions'."
	(interactive)
	(condition-case _
		(citre-jump)
      (error (call-interactively #'xref-find-definitions))))
  (defun +citre-jump-back ()
	"Go back to the position before last `citre-jump'. Fallback to `xref-go-back'."
	(interactive)
	(condition-case _
		(citre-jump-back)
      (error (call-interactively #'xref-go-back))))
  :init (require 'citre-config)
  :custom
  (citre-auto-enable-citre-mode-modes '(prog-mode))
  (citre-default-create-tags-file-location 'global-cache)
  (citre-use-project-root-when-creating-tags t)
  (citre-prompt-language-for-ctags-command t)
  (citre-enable-capf-integration t)

  ;; Use Citre xref backend as a [fallback]
  ;; (defadvice! +citre--xref-fallback-a (fn &rest args)
  ;;             :around #'xref--create-fetcher
  ;;             (let ((fetcher (apply fn args))
  ;;                   (citre-fetcher
  ;;                    (let ((xref-backend-functions '(citre-xref-backend t)))
  ;;                      (ignore xref-backend-functions)
  ;;                      (apply fn args))))
  ;;               (lambda ()
  ;;                 (or (with-demoted-errors "%s, fallback to citre"
  ;;                       (funcall fetcher))
  ;;                     (funcall citre-fetcher)))))
  )

;; [quickrun] Run commands quickly
(use-package
  quickrun
  :straight t
  :bind (:map prog-mode-map ("C-c c r" . quickrun))
  :custom (quickrun-focus-p nil))

;; [flymake] On-the-fly syntax checker
(use-package
  flymake
  :hook ((prog-mode . flymake-mode))
  :bind
  (:map
   prog-mode-map
   (("C-c c ]" . flymake-goto-next-error)
	("C-c c [" . flymake-goto-prev-error)
	("C-c c b" . flymake-show-buffer-diagnostics))
   )
  :custom (flymake-diagnostic-functions nil))

(use-package format-all :straight t)

;; [agda]
(use-package
  agda
  :no-require t
  :if (executable-find "agda-mode")
  :init
  (load-file
   (let ((coding-system-for-read 'utf-8))
     (shell-command-to-string "agda-mode locate"))))

(use-package
  cc-mode
  :custom
  ;; (setq c-basic-offset 4)
  (c-set-offset 'case-label '+))

(use-package clojure-mode :straight t)
(use-package cmake-mode :straight t)
(use-package dockerfile-mode :straight t)
(use-package elisp-mode)
;; (use-package elisp-autofmt :straight t)
(use-package gnu-apl-mode :straight t)
(use-package go-mode :straight t)
(use-package haskell-mode :straight t)
;; (use-package java-mode :straight t)
(use-package js-mode :custom (js-indent-level 2))
(use-package json-mode :straight t)
(use-package julia-mode :straight t)
(use-package just-mode :straight t)
(use-package lua-mode :straight t)
(use-package nix-mode :straight t)
(use-package racket-mode :straight t)
(use-package rust-mode :straight t)
(use-package scala-mode :straight t)
(use-package sly
  :straight t
  :config
  (setq inferior-lisp-program "ros run")
  )
(use-package typescript-mode :straight t)
(use-package yaml-mode :straight t)
(use-package zig-mode :straight t)
;; [treesit]
;; (use-package
;;   treesit
;;   :when (treesit-available-p)
;;   :init
;;   (setq major-mode-remap-alist
;;         '((c-mode . c-ts-mode)
;;           (c++-mode . c++-ts-mode)
;;           (python-mode . python-ts-mode)
;;           (javascript-mode . javascript-ts-mode)
;;           (typescript-mode . typescript-ts-mode)
;;           ;; (rust-mode . rust-ts-mode)
;;           ))
;;
;;   (setq treesit-language-source-alist
;;         '((c "https://github.com/tree-sitter/tree-sitter-c")
;;           (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;           (python "https://github.com/tree-sitter/tree-sitter-python")
;;           (javascript
;;            "https://github.com/tree-sitter/tree-sitter-javascript"
;;            "master"
;;            "src")
;;           (typescript
;;            "https://github.com/tree-sitter/tree-sitter-typescript"
;;            "master"
;;            "typescript/src")
;;           (rust "https://github.com/tree-sitter/tree-sitter-rust")
;;           (typst "https://github.com/uben0/tree-sitter-typst")
;;           ))
;;
;;   (dolist (lang treesit-language-source-alist)
;;     (unless (treesit-language-available-p (car lang))
;;       (treesit-install-language-grammar (car lang))))
;;
;;   (setq treesit-font-lock-level 4)
;;   )

;; (use-package treesit-auto
;;   :straight t
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))
