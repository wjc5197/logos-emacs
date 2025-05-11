;;; -*- lexical-binding: t -*-

;; [ispell] spell checker
(use-package
  ispell
  :hook ((org-mode . +org-skip-region-alist) (markdown-mode . +markdown-skip-region-alist))
  :preface
  ;; Don't spellcheck org blocks
  (defun +org-skip-region-alist ()
	(make-local-variable 'ispell-skip-region-alist)
	(dolist (pair
             '((org-property-drawer-re)
               ("~" "~")
               ("=" "=")
               ("^#\\+BEGIN_SRC" "^#\\+END_SRC")
               ("\\\\(" "\\\\)")
               ("\\[" "\\]")
               ("^\\\\begin{[^}]+}" "^\\\\end{[^}]+}")))
      (add-to-list 'ispell-skip-region-alist pair)))

  (defun +markdown-skip-region-alist ()
	(make-local-variable 'ispell-skip-region-alist)
	(dolist (pair
             '(("`" "`")
               ("^```" "^```")
               ("{{" "}}")
               ("\\\\(" "\\\\)")
               ("\\[" "\\]")
               ("^\\\\begin{[^}]+}" "^\\\\end{[^}]+}")))
      (add-to-list 'ispell-skip-region-alist pair)))
  :custom
  ;; (text-mode-ispell-word-completion nil)
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--run-together"))
  (ispell-dictionary "en_US")
  (ispell-alternate-dictionary (getenv "WORDLIST"))
  ;; aspell config
  (ispell-aspell-dict-dir (ispell-get-aspell-config-value "dict-dir"))
  (ispell-aspell-data-dir (ispell-get-aspell-config-value "data-dir"))
  (ispell-personal-dictionary (expand-file-name "ispell/.pws" user-emacs-directory)))
