;;; -*- lexical-binding: t -*-

(setq
 ;; Case insensitive completion
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t)

(use-package
 vertico
 :straight t
 :bind (:map vertico-map ("TAB" . minibuffer-complete) ("M-j" . vertico-quick-jump))
 :hook (after-init . vertico-mode)
 :custom (vertico-cycle t) (vertico-resize nil) (vertico-count 15)

 ;; WORKAROUND: https://github.com/minad/vertico#problematic-completion-commands
 (org-refile-use-outline-path 'file) (org-outline-path-complete-in-steps nil))

(use-package
 vertico-directory
 :after vertico
 :bind
 (:map
  vertico-map
  ("RET" . vertico-directory-enter)
  ("DEL" . vertico-directory-delete-char)
  ("M-DEL" . vertico-directory-delete-word))
 :hook
 ( ;; Cleans up path when moving directories with shadowed paths syntax
  rfn-eshadow-update-overlay
  . vertico-directory-tidy))

(use-package
 vertico-repeat
 :after vertico
 :hook (minibuffer-setup . vertico-repeat-save)
 :bind (:map vertico-map ("M-r" . vertico-repeat-select)))

(use-package savehist :hook (after-init . savehist-mode))

(use-package
 orderless
 :straight t
 :demand t
 :preface
 ;; Dispatchers
 (defun +vertico-orderless-dispatch (pattern _index _total)
   (cond
    ;; Ensure $ works with Consult commands, which add disambiguation suffixes
    ((string-suffix-p "$" pattern)
     `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
    ;; Ignore single !
    ((string= "!" pattern)
     `(orderless-literal . ""))
    ;; Without literal
    ((string-prefix-p "!" pattern)
     `(orderless-without-literal . ,(substring pattern 1)))
    ;; Character folding
    ((string-prefix-p "%" pattern)
     `(char-fold-to-regexp . ,(substring pattern 1)))
    ((string-suffix-p "%" pattern)
     `(char-fold-to-regexp . ,(substring pattern 0 -1)))
    ;; Initialism matching
    ((string-prefix-p "^" pattern)
     `(orderless-initialism . ,(substring pattern 1)))
    ((string-suffix-p "^" pattern)
     `(orderless-initialism . ,(substring pattern 0 -1)))
    ;; Literal matching
    ((string-prefix-p "=" pattern)
     `(orderless-literal . ,(substring pattern 1)))
    ((string-suffix-p "=" pattern)
     `(orderless-literal . ,(substring pattern 0 -1)))
    ;; Flex matching
    ((string-prefix-p "~" pattern)
     `(orderless-flex . ,(substring pattern 1)))
    ((string-suffix-p "~" pattern)
     `(orderless-flex . ,(substring pattern 0 -1)))
    ;; Annotations
    ((string-prefix-p "@" pattern)
     `(orderless-annotation . ,(substring pattern 1)))
    ((string-suffix-p "@" pattern)
     `(orderless-annotation . ,(substring pattern 0 -1)))))

 ;; Remote file completion
 (defun +vertico-basic-remote-try-completion (string table pred point)
   (and (vertico--remote-p string)
        (completion-basic-try-completion string table pred point)))

 (defun +vertico-basic-remote-all-completions (string table pred point)
   (and (vertico--remote-p string)
        (completion-basic-all-completions string table pred point)))

 (defun orderless+basic-all (str table pred point)
   (or (orderless-all-completions str table pred point)
       (completion-basic-all-completions str table pred point)))

 (defun orderless+basic-try (str table pred point)
   (or (completion-basic-try-completion str table pred point)
       (orderless-try-completion str table pred point)))
 :custom
 (completion-styles '(orderless basic))
 (completion-category-defaults nil)
 (completion-ignore-case t)
 ;; despite override in the name, orderless can still be used in find-file etc.
 (completion-category-overrides
  '((file (styles +vertico-basic-remote orderless basic)) (eglot (styles orderless))))
 (orderless-style-dispatchers '(+vertico-orderless-dispatch))
 (orderless-component-separator "[ &]")
 :config
 (add-to-list
  'completion-styles-alist
  '(+vertico-basic-remote
    +vertico-basic-remote-try-completion
    +vertico-basic-remote-all-completions
    "Use basic completion on remote files only"))
 (add-to-list
  'completion-styles-alist
  '(orderless+basic
    orderless+basic-try orderless+basic-all "Unholy mix of Orderless and Basic."))
 )

(use-package marginalia :straight t :hook (vertico-mode . marginalia-mode))

(use-package
 embark
 :straight t
 :bind
 (("C-;" . embark-act)
  ("C-h B" . embark-bindings)
  ;; ("C-c ; e" . embark-export)
  ;; ("C-c ; c" . embark-collect)
  :map
  embark-file-map
  ("g" . +embark-magit-status))
 :preface
 (defun +embark-magit-status (file)
   "Run `magit-status` on repo containing the embark target."
   (interactive "GFile: ")
   (magit-status (locate-dominating-file file ".git")))
 :custom (prefix-help-command 'embark-prefix-help-command))

(use-package
 consult
 :straight t
 :bind
 (([remap bookmark-jump] . consult-bookmark)
  ([remap list-registers] . consult-register)
  ([remap goto-line] . consult-goto-line)
  ([remap imenu] . consult-imenu)
  ([remap locate] . consult-locate)
  ([remap load-theme] . consult-theme)
  ([remap man] . consult-man)
  ([remap recentf-open-files] . consult-recent-file)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap yank-pop] . consult-yank-pop)
  ("C-c i" . consult-imenu)
  ("C-c I" . consult-imenu-multi)
  ;; ("C-c t r" . consult-ripgrep)
  ;; ("C-c t f" . consult-fd)
  )
 :hook (completion-list-mode . consult-preview-at-point-mode)
 :custom (consult-narrow-key "<") (consult-async-min-input 2)
 ;; [consult-register] Configure the register formatting.
 (register-preview-delay 0.5) (register-preview-function #'consult-register-format)
 ;; [consult-xref] Use Consult to select xref locations with preview
 (xref-show-xrefs-function #'consult-xref) (xref-show-definitions-function #'consult-xref)
 :config
 ;; replace multi-occur with consult-multi-occur
 (advice-add #'multi-occur :override #'consult-multi-occur)

 ;; This adds thin lines, sorting and hides the mode line of the window.
 (advice-add #'register-preview :override #'consult-register-window)

 ;; better preview
 (consult-customize
  consult-ripgrep
  consult-git-grep
  consult-grep
  consult-bookmark
  consult-recent-file
  consult--source-recent-file
  consult--source-project-recent-file
  consult--source-bookmark
  consult-buffer
  :preview-key "M-P")
 (consult-customize consult-theme :preview-key (list "M-P" :debounce 0.6 'any)))

;; [consult-dir] Insert path quickly in minibuffer
(use-package
 consult-dir
 :straight t
 :bind
 (([remap list-directory] . consult-dir)
  :map
  vertico-map
  ("C-x C-d" . consult-dir)
  ("C-x C-j" . consult-dir-jump-file))
 :config
 (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
 (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t))

(use-package
 embark-consult
 :straight t
 :after (embark consult)
 :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package
 tempel
 :straight t
 :hook
 (((prog-mode text-mode) . +tempel-setup-capf)
  ((prog-mode text-mode) . tempel-abbrev-mode))
 :preface
 (defun +tempel-setup-capf ()
   (push #'tempel-complete completion-at-point-functions))
 :custom (tempel-trigger-prefix nil))

(use-package tempel-collection :straight t :after tempel)

;; [corfu] compleletion frontend
(use-package
 corfu
 :straight (:files (:defaults "extensions/*.el"))
 :demand t
 :bind (:map corfu-map ("RET" . nil))
 :hook
 (((eshell-mode prog-mode shell-mode text-mode) . corfu-mode)
  ((eshell-mode shell-mode) . (lambda () (setq-local corfu-auto nil)))
  (minibuffer-setup . +corfu-enable-in-minibuffer))
 :preface
 ;; Completing in the minibuffer
 (defun +corfu-enable-in-minibuffer ()
   "Enable Corfu in the minibuffer if `completion-at-point' is bound."
   (when (where-is-internal #'completion-at-point (list (current-local-map)))
     (corfu-mode 1)))
 :custom
 (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
 (corfu-auto t) ;; Enable auto completion
 (corfu-separator ?&) ;; Orderless field separator
 (corfu-auto-prefix 1) ;; minimun prefix to enable completion
 (corfu-preview-current nil) (corfu-auto-delay 0.1)
 )

(use-package
 corfu-history
 :after
 corfu
 savehist
 :hook (after-init . corfu-history-mode)
 :config (cl-pushnew 'corfu-history savehist-additional-variables))

(use-package
 corfu-popupinfo
 :after corfu
 :hook (after-init . corfu-popupinfo-mode)
 :config (setq corfu-popupinfo-delay '(1.0 . 1.0)))

(use-package
 corfu-quick
 :after corfu
 :bind (:map corfu-map ("M-j" . corfu-quick-complete)))

(use-package
 corfu-terminal
 :straight t
 :if (not (display-graphic-p))
 :after corfu
 :hook (after-init . corfu-terminal-mode))

(use-package
 cape
 :straight t
 :hook
 ((corfu-mode . +corfu-add-cape-backends)
  ((LaTeX-mode markdown-mode org-mode TeX-mode) . +corfu-add-cape-tex-backends))
 :preface
 (defun +corfu-add-cape-backends ()
   (add-to-list 'completion-at-point-functions #'cape-file :append)
   (add-to-list 'completion-at-point-functions #'cape-dabbrev :append))

 (defun +corfu-add-cape-tex-backends ()
   (add-to-list 'completion-at-point-functions #'cape-tex :append)))

(use-package
 dabbrev
 :custom (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; (use-package
;;  consult-dash
;;  :straight t
;;  :bind (("M-s d" . consult-dash))
;;  :config
;;  ;; Use the symbol at point as initial search term
;;  (consult-customize consult-dash :initial (thing-at-point 'symbol)))
