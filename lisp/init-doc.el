;;; -*- lexical-binding: t -*-

(use-package
  org
  :straight
  '(org
	:fork (:host nil :repo "https://git.tecosaur.net/tec/org-mode.git" :branch "dev")
	:files (:defaults "etc")
	:build t
	:pre-build
	(with-temp-file "org-version.el"
      (require 'lisp-mnt)
      (let ((version
             (with-temp-buffer
               (insert-file-contents "lisp/org.el")
               (lm-header "version")))
			(git-version
             (string-trim
              (with-temp-buffer
				(call-process "git" nil t nil "rev-parse" "--short" "HEAD")
				(buffer-string)))))
		(insert
         (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
         (format
          "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n"
          git-version)
         "(provide 'org-version)\n")))
	:pin nil)
  :bind
  (:map
   org-mode-map
   ("C-c o i" . org-toggle-inline-images)
   ("C-c o l" . org-latex-preview)
   ;; ("C-c o s" . +org-sort-buffer-alpha-ord)
   )
  :hook
  ((org-mode . org-indent-mode)
   (org-mode . org-latex-preview-auto-mode)
   ;; (org-mode . turn-on-auto-fill)
   )
  :preface
  (defun +sort-elem-alpha-ord (elem)
	(let ((elem-type (org-element-type elem)))
      (cond
       ((member elem-type '(headline plain-list))
		(org-element-map (org-element-contents elem) elem-type #'+sort-elem-alpha-ord nil nil elem-type)
		(goto-char (org-element-property :begin elem))
		(condition-case nil
			(cond
			 ((eq elem-type 'headline) (org-sort-entries nil ?a))
			 ((eq elem-type 'plain-list) (org-sort-list nil ?a)))
		  (user-error)
		  )
		)
       (t
		(message "Skipping to sort unsupported element type: %s" elem-type)))))
  (defun +org-sort-buffer-alpha-ord ()
	(interactive)
	(save-excursion
	  (dolist (elem-type '('headline 'plain-list))
		(org-element-map (org-element-parse-buffer) elem-type #'+sort-elem-alpha-ord nil nil elem-type)
		)
	  (goto-char (point-min))
	  (condition-case nil
		  (org-sort-entries nil ?a)
		(user-error))
	  )
	)
  :custom
  (org-sort-function #'string-lessp)
  ;; hide all emphasis markers, *bold* becomes bold
  (org-hide-emphasis-markers nil) ; currently set nil for org-appear
  ;; show entities as UTF8 char: super/subscripts, \lambda = λ
  (org-pretty-entities t)
  (org-use-sub-superscripts '{})

  ;; (org-latex-compiler "lualatex")
  (org-latex-preview-process-default 'dvisvgm)
  ;; org-startup-with-latex-preview t

  ;; for pandoc conversion, don't explicitly use packages which export newcommands
  (org-latex-packages-alist
   '(
	 ;; ("" "amssymb")
     ;; ("" "esint")
     ;; ("" "esvect")
     ;; ("" "geometry")
     ("" "xcolor")
     ;; ("" "mathtools")
	 )
   )
  (org-latex-preview-preamble
   (concat
	"\\documentclass{article}\n"
	"[DEFAULT-PACKAGES]\n"
	"[PACKAGES]\n"
	;; "\\newcommand{\\p}[1]{\\left(#1\\right)}\n"
	;; "\\newcommand{\\s}[1]{\\left[#1\\right]}"
	))
  (org-preview-latex-image-directory "/tmp/ltximg")
  (org-highlight-latex-and-related '(native))

  (org-list-allow-alphabetical t)
  (org-list-sort-ascending 'alphabetically)
  (org-plain-list-ordered-item-terminator ?\.)

  (org-startup-with-inline-images t)

  ;; prevent warning from using #+SETUPFILE
  (org-resource-download-policy 'always)
  )

;; [org-entities]
(use-package
  org-entities
  :custom
  (org-entities-user
   '(("square" "\\square" t "&square;" "square" "square" "□")
     ("top" "\\top" t "&top;" "top" "top" "⊤")
     ("bot" "\\bot" t "&bot;" "bot" "bot" "⊥")
     ("vdash" "\\vdash" t "&vdash;" "vdash" "vdash" "⊢")
     ("vDash" "\\vDash" t "&vDash;" "vDash" "vDash" "⊨")
     ("Vdash" "\\Vdash" t "&Vdash;" "Vdash" "Vdash" "⊩")
     ("Vvdash" "\\Vvdash" t "&Vvdash;" "Vvdash" "Vvdash" "⊪")
     ("nvdash" "\\nvdash" t "&nvdash;" "nvdash" "nvdash" "⊬")
     ("nvDash" "\\nvDash" t "&nvDash;" "nvDash" "nvDash" "⊭")
     ("nVdash" "\\nVdash" t "&nVdash;" "nVdash" "nVdash" "⊮")
     ("nVDash" "\\nVDash" t "&nVDash;" "nVDash" "nVDash" "⊯")
     ("subseteq" "\\subseteq" t "&subseteq;" "subseteq" "subseteq" "⊆")
     ("supseteq" "\\supseteq" t "&supseteq;" "supseteq" "supseteq" "⊇")
     ("subsetneq" "\\subsetneq" t "&subsetneq;" "subsetneq" "subsetneq" "⊊")
     ("supsetneq" "\\supsetneq" t "&supsetneq;" "supsetneq" "supsetneq" "⊋")
     ("subsetneqq" "\\subsetneqq" t "&subsetneqq;" "subsetneqq" "subsetneqq" "⊊")
     ("supsetneqq" "\\supsetneqq" t "&supsetneqq;" "supsetneqq" "supsetneqq" "⊋")
     ("nsubset" "\\nsubset" t "&nsubset;" "nsubset" "nsubset" "⊄")
     ("nsupset" "\\nsupset" t "&nsupset;" "nsupset" "nsupset" "⊅")
     ("nsubseteq" "\\nsubseteq" t "&nsubseteq;" "nsubseteq" "nsubseteq" "⊈")
     ("nsupseteq" "\\nsupseteq" t "&nsupseteq;" "nsupseteq" "nsupseteq" "⊉"))))

;; [org-babel]
(use-package
  ob
  :custom (org-src-tab-acts-natively t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   +ob-lang))

(use-package ox
  :custom
  (org-export-with-smart-quotes t)
  (org-export-with-latex t)
  (org-export-with-sub-superscripts '{})
  (org-html-validation-link nil)
  (org-html-htmlize-output-type 'css)
  (org-latex-prefer-user-labels t)
  )

(use-package
  org-bars
  :straight (:host github :repo "tonyaldon/org-bars")
  :hook ((org-mode . org-bars-mode)))
(use-package org-inline-anim :straight t :hook ((org-mode . org-inline-anim-mode)))
;; (use-package iscroll :hook (org-mode . iscroll-mode))

;; [org-appear] toggle visibility of hidden elems
(use-package
  org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :custom (org-appear-trigger 'always)
  ;; (org-appear-delay 0.1)

  ;; appear options
  (org-appear-autoemphasis t)
  (org-appear-autoentities t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-inside-latex t))

(use-package
  org-download
  :straight t
  :hook (org-mode . org-download-enable)
  :preface
  (defun delete-current-image-file ()
	"Delete the file corresponding to the current image buffer and kill the buffer."
	(interactive)
	(when (and (derived-mode-p 'image-mode) buffer-file-name)
      (let ((file buffer-file-name))
		(when (yes-or-no-p (format "Are you sure you want to delete the file: %s?" file))
          (delete-file file)
          (message "Deleted file: %s" file)
          (kill-buffer)))))
  (defun win-save-image-from-clipboard (dest-file-path)
	"This saves any image that might be in the windows clipboard to the file at DEST-FILE-PATH"
	(message dest-file-path)
	(let* ((win-fname (replace-regexp-in-string "/" "\\\\" dest-file-path))
           (cmd-str
			(concat
             "powershell.exe -Command \"(Get-Clipboard -Format image).Save('"
             win-fname
             "')\"")))
      (shell-command cmd-str)))
  :custom
  ;; (org-download-method 'directory)
  (org-download-heading-lvl nil) (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-download-screenshot-method
   (+code-based-on-system
	(list #'win-save-image-from-clipboard "scrot %s" "scrot %s" "")))
  (org-download-annotate-function
   (lambda (link)
     (previous-line 1)
     "")))

(use-package
  org-modern
  :straight t
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star 'replace)
  (org-modern-replace-stars '("▲" "■" "⬟" "●" "✳"))
  (org-modern-list '((?* . "•") (?+ . "➤") (?- . "━")))
  (org-modern-table-vertical 2))

(use-package org-web-tools :straight t)

(use-package ox-hugo
  :straight t
  :after ox
  :custom
  (org-hugo-section "posts")
  ;; (org-hugo-external-file-extensions-allowed-for-copying nil)
  )

;; [mardown-mode]
(use-package
  markdown-mode
  :straight t
  :custom
  (markdown-enable-math t)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-header-scaling t)
  (markdown-asymmetric-header t)
  (markdown-nested-imenu-heading-index t)
  (markdown-fontify-code-blocks-natively t)
  ;; :config
  ;;  (add-to-list 'markdown-code-lang-modes '("rust" . rustic-mode))
  ;;  (add-to-list 'markdown-code-lang-modes '("verilog" . verilog-mode))
  ;;  (add-to-list 'markdown-code-lang-modes '("agda" . agda2-mode))
  )

;; [auctex]
(use-package
  tex
  :straight auctex
  :custom
  (TeX-parse-self t) ; parse on load
  (TeX-auto-save t) ; parse on save
  ;; Use hidden directories for AUCTeX files.
  (TeX-auto-local ".auctex-auto")
  (TeX-style-local ".auctex-style")
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctex)
  ;; Don't start the Emacs server when correlating sources.
  (TeX-source-correlate-start-server nil)
  ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
  (TeX-electric-sub-and-superscript t)
  ;; Just save, don't ask before each compilation.
  (TeX-save-query nil))

;; [cdlatex]
(use-package
  cdlatex
  :straight t
  :hook ((org-mode . turn-on-org-cdlatex) ((latex-mode LaTeX-mode) . turn-on-cdlatex))
  :custom
  (cdlatex-env-alist
   '(
	 ;; ("axiom" "\\begin{axiom}\nAUTOLABEL\n?\n\\end{axiom}\n" nil)
     ;; ("theorem" "\\begin{theorem}\nAUTOLABEL\n?\n\\end{theorem}\n" nil)
     ("bmatrix" "\\begin{bmatrix}\n?\n\\end{bmatrix}\n" nil)))
  (cdlatex-math-symbol-alist
   '((?1 ("`" "" "" ""))
     (?< ("\\leftarrow" "\\Leftarrow" "\\longleftarrow" "\\Longleftarrow"))
     (?>
      ("\\rightarrow" "\\Rightarrow" "\\longrightarrow" "\\Longrightarrow"))
     (?: ("\\dashleftarrow" "\\dashrightarrow")))))

;; [nov] epub
(use-package nov :straight t :mode ("\\.epub\\'" . nov-mode))

;; [pdf-view] View PDF
(use-package
  pdf-tools
  :straight t
  :hook ((pdf-tools-enabled . pdf-view-auto-slice-minor-mode))
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :bind
  (:map
   pdf-view-mode-map
   ([remap scroll-up-command] . pdf-view-scroll-up-or-next-page)
   ([remap scroll-down-command] . pdf-view-scroll-down-or-previous-page))
  :custom
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)
  (pdf-annot-activate-created-annotations t)
  :config (pdf-tools-install t nil t nil))

(use-package
  pdf-isearch
  :after pdf-tools
  :hook (pdf-tools-enabled . pdf-isearch-minor-mode))

(use-package
  pdf-outline
  :after pdf-tools
  :hook (pdf-tools-enabled . pdf-outline-minor-mode))

;; (use-package pdf-annot
;;   :after pdf-tools
;;   :init
;;   (add-hook! kill-buffer-hook :local
;;     (defun +pdf-cleanup-annot-windows ()
;;       "Kill left-over annotation buffers when the document is killed."
;;       (when (buffer-live-p pdf-annot-list-document-buffer)
;;         (pdf-info-close pdf-annot-list-document-buffer))
;;       (when (buffer-live-p pdf-annot-list-buffer)
;;         (kill-buffer pdf-annot-list-buffer))
;;       (let ((contents-buffer (get-buffer "*Contents*")))
;;         (when (and contents-buffer (buffer-live-p contents-buffer))
;;           (kill-buffer contents-buffer)))))
;;   )

;; (use-package visual-fill-column
;;   :straight t
;;   :custom
;;   (visual-fill-column-center-text t))

(use-package typst-ts-mode
  :straight (:type git :host codeberg :repo "meow_king/typst-ts-mode" :branch "develop")
  :bind
  (:map
   typst-ts-mode-map
   ("C-c C-c" . typst-ts-tmenu))
  :custom
  (typst-ts-watch-options "--open")
  ;; (typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory))
  (typst-ts-mode-enable-raw-blocks-highlight t)
  )

(use-package adoc-mode
  :straight t)

(use-package olivetti
  :straight t)
