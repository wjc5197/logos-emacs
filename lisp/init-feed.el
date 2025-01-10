;; -*- lexical-binding: t; -*-

;; [elfeed] Read rss within Emacs
(use-package elfeed
  :straight t
  :config
  (let ((opml-files (directory-files-recursively +feed-dir "\\.opml$")))
    (dolist (file opml-files)
      (elfeed-load-opml file)
      ))
  )
