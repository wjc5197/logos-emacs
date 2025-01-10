;;; -*- lexical-binding: t -*-

;; [comint] Command interpreter
(use-package
 comint
 :custom
 (comint-buffer-maximum-size 2048)
 (comint-prompt-read-only t)
 ;; No paging, `eshell' and `shell' will honoring.
 (comint-pager "cat")
 ;; better history search
 (comint-history-isearch 'dwim))

(use-package
 benchmark-init
 :straight t
 :init (benchmark-init/activate)
 :hook ((after-init . benchmark-init/deactivate)))

(use-package
 helpful
 :straight t
 :bind
 (("C-h k" . helpful-key)
  ("C-h f" . helpful-callable)
  ("C-h F" . helpful-function)
  ("C-h o" . helpful-symbol)
  ("C-h v" . helpful-variable)
  ("C-h x" . helpful-command))
 ;; :custom
 ;; (helpful-max-buffers 1)
 )

;; [gcmh] Optimize GC
(use-package
 gcmh
 :straight t
 :hook (emacs-startup . gcmh-mode)
 :custom
 (gcmh-high-cons-threshold #x64000000)
 (gcmh-idle-delay 'auto)
 (gcmh-auto-idle-delay-factor 10))

;; [timeout] debounce and throttle
(use-package timeout
  :straight (:host github :repo "karthink/timeout" :branch "master"))
