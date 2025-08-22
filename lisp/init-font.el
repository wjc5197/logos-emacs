;;; -*- lexical-binding: t -*-

(setq
 +font-size 13.0

 ;; Font compacting can be terribly expensive, but may increase memory use
 inhibit-compacting-font-caches t

 ;; Inhibits fontification while receiving input, which should help a little with scrolling performance.
 redisplay-skip-fontification-on-input t

 ;; text-scale-remap-header-line t
 )

(when (display-graphic-p)
  (cl-loop
   for font in
   '("Iosevka"
     "Fira Code"
     "Source Code Pro"
     "SF Mono"
     "Menlo"
     "Monaco"
     "Dejavu Sans Mono"
     "Lucida Console"
     "Consolas"
     "SAS Monospace")
   when (+font-installed-p font) return
   (set-face-attribute 'default nil
                       :font
                       (font-spec
                        :family font
                        :weight 'normal
                        :slant 'normal
                        :size +font-size)))
  (cl-loop
   for
   font
   in
   '("Apple Color Emoji"
     "Noto Color Emoji"
     "Segoe UI Emoji"
     "EmojiOne Color"
     "Symbola"
     "Symbol")
   when
   (+font-installed-p font)
   return
   (set-fontset-font t 'unicode (font-spec :family font) nil 'prepend)
   )

  (cl-loop
   for font in
   '("Sarasa Gothic SC"
	 "LXGW WenKai Mono"
	 "Source Han Serif"
     "Source Han Sans"
	 "Noto Serif CJK SC"
	 "Noto Sans CJK SC"
     )
   when (+font-installed-p font) return
   (progn
	 (mapc (lambda (charset)
			 (set-fontset-font t charset (font-spec :name font :weight 'normal :slant 'normal)))
		   '(han hangul kana cjk-misc))
	 (add-to-list 'face-font-rescale-alist `(,font . 1.0) t)
	 )
   )

  (cl-loop
   for
   font
   in
   '("Amiri"
	 )
   when
   (+font-installed-p font)
   return
   (set-fontset-font t 'arabic (font-spec :family font) nil 'prepend)
   )

  (cl-loop
   for font in '("HanaMinB" "SimSun-ExtB")
   when (+font-installed-p font) return
   (set-fontset-font
    t '(#x20000 . #x2A6DF)
    (font-spec :name font :weight 'normal :slant 'normal))))
