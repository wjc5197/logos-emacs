;;; -*- lexical-binding: t -*-

(use-package mu4e
  :straight nil
  :demand t
  :custom
  ;; (mu4e-completing-read-function #'ivy-completing-read)
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-modeline-support nil)
  (mail-user-agent #'mu4e-user-agent)
  (message-mail-user-agent t)
  )

(use-package telega
  :straight t
  :custom
  (telega-proxies
   `((:server ,proxy-server :port ,proxy-port :enable t :type (:@type "proxyTypeHttp"))))
  (telega-translate-to-language-by-default "zh")

  (telega-chat-show-avatars nil)
  (telega-user-show-avatars nil)
  (telega-root-show-avatars nil)

  (telega-idle-delay 5)
  (telega-chat-fill-column 70)
  (telega-chat-input-markups '(nil "org"))

  (telega-sticker--use-thumbnail t)
  (telega-emoji-use-images nil)
  (telega-use-images nil)
  ;; (telega-mode-line-mode 1)

  ;; root page
  (telega-root-fill-column 70)
  (telega-root-auto-fill-mode nil)

  ;; filters
  (telega-filters-custom nil)
  (telega-filter-custom-show-folders nil))

(use-package ement
  :straight t
  ;; :demand t
  :bind (:map ement-room-image-keymap
			  ("RET" . +ement-room-image-show))
  :preface
  ;; not pop up new frame for image show
  (defun +ement-room-image-show (pos)
	"Show image at POS in a new buffer."
	(interactive "d")
	(pcase-let* ((image (copy-sequence (get-text-property pos 'display)))
				 (ement-event (ewoc-data (ewoc-locate ement-ewoc pos)))
				 ((cl-struct ement-event id) ement-event)
				 (buffer-name (format "*Ement image: %s*" id)))
      (when (fboundp 'imagemagick-types)
		;; Only do this when ImageMagick is supported.
		;; FIXME: When requiring Emacs 27+, remove this (I guess?).
		(setf (image-property image :type) 'imagemagick))
      (setf (image-property image :scale) 1.0
			(image-property image :max-width) nil
			(image-property image :max-height) nil)
      (unless (get-buffer buffer-name)
		(with-current-buffer (get-buffer-create buffer-name)
          (erase-buffer)
          (insert-image image)
          (image-mode)))
      ;; Switch to the buffer in the current window
      (switch-to-buffer buffer-name)))
  :custom
  (ement-save-sessions t)
  )

(use-package mastodon
  :straight t
  :custom
  (mastodon-instance-url "https://mastodon.social")
  (mastodon-active-user "WJC5197"))
