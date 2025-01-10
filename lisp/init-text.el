;;; -*- lexical-binding: t -*-

(setq
 ;; smaller threshold to improve long line performance
 long-line-threshold 1000
 large-hscroll-threshold 1000
 syntax-wholeline-max 1000

 ;; Sentence end
 ;; sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
 sentence-end-double-space nil

 ;; Fix alignment problem
 truncate-string-ellipsis "…"

 ;; [tab]
 ;; Make `tabify' only affect indentation
 tabify-regexp "^\t* [ \t]+"
 tab-always-indent t

 ;; Disable [bidirectional text] scanning for a modest performance
 ;; Will improve long line display performance
 bidi-inhibit-bpa t

 ;; [Wrapping] words at whitespace, but do not wrap by default
 ;; don't do any wrapping by default since it's expensive
 truncate-partial-width-windows nil
 ;; better wrapping for cjk
 word-wrap-by-category t

 ;; Disable copy region blink
 ;; copy-region-blink-delay 0
 ;; delete-pair-blink-delay 0

 backward-delete-char-untabify-method 'hungry)

(setq-default
 bidi-paragraph-direction 'left-to-right
 bidi-display-reordering 'left-to-right
 fill-column 90
 ;; Indent with 4 space by default
 tab-width 4
 truncate-lines t
 ;; Wrap words at whitespace, rather than in the middle of a word.
 word-wrap t)

;; [ialign] Interactive align
(use-package ialign :straight t)

;; [re-builder]
(use-package
 re-builder
 ;; :commands re-builder
 :custom (reb-re-syntax 'string))

;; [so-long] Workaround for long one-line file
(use-package
 so-long
 :hook
 (((so-long-mode fundamental-mode) . +so-long-settings)
  (after-init . global-so-long-mode))
 :preface
 ;; improve long line performance
 (defun +so-long-settings ()
   (setq bidi-display-reordering nil))
 :config
 ;; Saveplace should not operate in large/long files
 (add-to-list 'so-long-variable-overrides '(save-place-alist . nil)))

;; [ws-butler] Remove trailing whitespace with lines touched
(use-package ws-butler :straight t :hook ((prog-mode text-mode) . ws-butler-mode))

;; [visual-line-mode] Soft line-wrapping
(use-package visual-line-mode :hook (text-mode . visual-line-mode))
