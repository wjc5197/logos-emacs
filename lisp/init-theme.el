;;; -*- lexical-binding: t -*-

(setq
 +amethyst "#8963b7"
 +apricot "#ebd3c5"
 +baby-blue "#89cff0"
 +coffee "#b19994"
 +dark-lavender "#5d4d7a"
 +default-black "#252525"
 +golden "#ffd700"
 +goldenrod "#eead0e"
 +lavender "#e6e6fa"
 +lavender-grey "#d3d3e7"
 +light-apricot "#efe6dd"
 +light-grey "#afafaf"
 +inactive-black "#171717"
 +inactive-purple "#170913"
 +mint-green "#13a10e"
 +onyx "#373737"
 +red-brown "#c35c35"
 +rose-red "#c21e56"
 +silver "#d0d0d0"
 +smoke-white "#f0f0f0"
 +snow-white "#fbf8ef"
 +white "#ffffff")

(use-package
  spacemacs-theme
  :straight t
  :init (load-theme 'spacemacs-dark t)
  :custom
  (spacemacs-theme-comment-bg nil)
  (spacemacs-theme-custom-colors
   `((base . ,+light-grey)
     (bg1 . ,+default-black)
     (head1 . ,+amethyst)
     (head2 . ,+golden)
     (head3 . ,+apricot)
     (head4 . ,+silver)
     (keyword . ,+amethyst)
     (cursor . ,+goldenrod)
     (lnum . ,+lavender-grey)
     (err . ,+rose-red)
     (comment . ,+mint-green)
     (comment-bg . ,+default-black)
     (type . ,+golden)
     (func . ,+smoke-white)
     (const . ,+apricot)
     (mat . ,+apricot)
     (meta . ,+snow-white)
     (var . ,+light-apricot)
	 )
   )
  (spacemacs-theme-underline-parens nil)
  :config
  (custom-set-faces
   `(ansi-color-bright-blue ((t (:foreground ,+baby-blue :background ,+baby-blue))))
   `(auto-dim-other-buffers ((t (:background ,+inactive-purple))))
   `(corfu-current ((t (:foreground "white" :background ,+dark-lavender))))
   `(orderless-match-face-0 ((t (:foreground ,+snow-white))))
   '(org-code ((t (:foreground unspecified :inherit default))))
   `(org-drawer ((t (:foreground ,+red-brown))))
   '(markdown-code-face ((t (:inherit unspecified))))
   `(mode-line ((t (:background ,+onyx))))
   `(mode-line-inactive ((t (:background ,+inactive-black :box nil))))
   `(show-paren-match ((t (:foreground ,+golden :weight ultra-bold))))
   `(tab-bar ((t (:background ,+dark-lavender))))
   '(tab-bar-tab ((t (:inherit mode-line))))
   '(tab-bar-tab-inactive ((t (:inherit mode-line-inactive))))
   )
  )

;; (use-package modus-themes
;;   :init
;;   (load-theme 'modus-vivendi t)
;;   :custom
;;   (modus-themes-headings
;;    '((1 . (1.5))
;;      (2 . (1.4))
;; 	 (3 . (1.3))
;; 	 (4 . (1.2))
;;      (agenda-date . (1.3))
;;      (agenda-structure . (1.5))
;;      (t . (1.1))))
;;   (modus-themes-mixed-fonts t)
;;   (modus-vivendi-palette-overrides
;;    `((bg-main  ,+default-black)
;;      (fg-main  ,+light-grey)
;;      (fg-prompt  ,+amethyst)
;; 	 (fg-alt ,+snow-white)
;;      (cursor  ,+goldenrod)
;;      (comment  ,+mint-green)
;;      (string  ,+mint-green)
;;      (constant  ,+apricot)
;;      (name  ,+light-apricot)
;;      (type  ,+golden)
;; 	 (variable ,+light-apricot)
;;      (fnname  ,+smoke-white)
;;      (keyword  ,+amethyst)
;; 	 (builtin ,+amethyst)
;;      (fg-heading-1  ,+amethyst)
;;      (fg-heading-2  ,+golden)
;;      (fg-heading-3  ,+apricot)
;;      (fg-heading-4  ,+silver)
;; 	 (fg-completion-match-1 ,+amethyst)
;; 	 (fg-completion-match-2 ,+golden)
;; 	 (fg-completion-match-3 ,+apricot)
;; 	 ;; (fg-completion-match-3 ,+silver)
;;      ;; (fg-line-number-active  ,+lavender-grey)
;;      (err  ,+rose-red))))
