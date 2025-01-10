;;; -*- lexical-binding: t -*-

;; [browse-url] Pass a URL to browser
(use-package browse-url
  :defines dired-mode-map
  :bind (("C-c b o" . browse-url-at-point)
         ("C-c b e" . browse-url-emacs))
  :custom
  (browse-url-browser-function #'browse-url-chrome)
  )

;; Google
(use-package google-this :straight t :bind (("C-c b g" . google-this)))

(use-package
  google-translate
  :straight t
  :bind (("C-c b t" . google-translate-smooth-translate))
  :init
  (setq google-translate-translation-directions-alist '(("en" . "zh-CN"))))

(use-package w3m :straight t)
(use-package zeal-at-point
  :straight t
  :bind
  (:map prog-mode-map ("C-c b z" . zeal-at-point))
  ;; :hook (prog-mode . zeal-at-point-mode)
  )
