;;; -*- lexical-binding: t -*-

(setq
 proxy-server "localhost"
 proxy-port "7890"
 url-proxy-services
 `(("no_proxy" . "^\\(localhost\\|10.*\\)")
   ("http" . ,(concat proxy-server ":" proxy-port))
   ("https" . ,(concat proxy-server ":" proxy-port)))
 ring-bell-function 'ignore
 native-comp-async-report-warnings-errors 'silent

 ;; Use y-or-n to replace yes-or-no
 use-short-answers t

 ;; Update UI slowly
 idle-update-delay 1.0

 +doc-dir (+code-based-on-system '("D:/OneDrive/Library" "~/documents" "" ""))
 +emacs-dir (expand-file-name "emacs" +doc-dir)
 +blog-dir (expand-file-name "blog" +doc-dir)
 +test-dir (expand-file-name "test" +emacs-dir)
 +feed-dir (expand-file-name "feed" +doc-dir)
 +bib-dir (expand-file-name "bib" +doc-dir)
 org-directory (expand-file-name "org" +emacs-dir)
 org-roam-directory (expand-file-name "roam" +emacs-dir)
 org-roam-dailies-directory "journal"
 ;; org-journal-dir (expand-file-name "journal" org-roam-directory)
 org-agenda-dir (expand-file-name "agenda" org-roam-directory)
 ;; org-download-image-dir (expand-file-name "img" org-roam-directory)
 ;; org-media-note-screenshot-image-dir org-download-image-dir
 ebib-default-directory +bib-dir
 citar-bibliography (file-expand-wildcards (expand-file-name "*.bib" +bib-dir))
 ;; citar-notes-paths (list (file-name-concat +blog-dir "orgs/paper-notes"))
 ;; ebib-reading-list-file (expand-file-name "test.org" org-agenda-dir)
 org-hugo-base-dir +blog-dir
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 hledger-jfile (expand-file-name "hledger/account.hlg" +doc-dir)
 +inbox-file (expand-file-name "inbox.org" org-agenda-dir)
 +desire-file (expand-file-name "desire.org" org-agenda-dir)
 +known-file (expand-file-name "known.org" org-agenda-dir)
 ;;	All crypt with the same key
 +crypt-key "wjc5197"
 +ob-lang '((emacs-lisp . t)
			(haskell . t)
			(latex . t)
			(plantuml . t)
			(python . t)
			(shell . t))

 +punc-replace-pairs
 '(("，" . ",")
   ("。" . ".")
   ("．" . ".")
   ("？" . "?")
   ("！" . "!")
   ("；" . ";")
   ("：" . ":")
   ("“" . "\"")
   ("”" . "\"")
   ("‘" . "'")
   ("’" . "'")
   ("（" . "(")
   ("）" . ")")
   ("《" . "\"")
   ("》" . "\"")
   ("〈" . "\"")
   ("〉" . "\"")
   ("【" . "[")
   ("】" . "]")
   ("『" . "\"")
   ("』" . "\"")
   ("「" . "\"")
   ("」" . "\"")
   ("——" . "—")
   ("…" . "...")
   ("、" . ","))
 +tex-replace-pairs
 '(;; replace $...$ with \(...\)
   ("\\$\\([^$]+\\)\\$" . "\\\\(\\1\\\\)")
   )
 +logos-replace-pairs
 '(("\\([^']\\):" . "\\1':")
   )
 )

(blink-cursor-mode -1)
(column-number-mode 1)
