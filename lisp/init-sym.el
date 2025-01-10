;;; -*- lexical-binding: t -*-

;; [elec-pair] Automatic parenthesis pairing
(use-package
  elec-pair
  :hook ((minibuffer-mode prog-mode text-mode) . electric-pair-mode)
  :custom (electric-pair-inhibit-predicate 'electric-pair-default-inhibit))

;; [goto-addr] Click to open URL
(use-package
  goto-addr
  :hook ((prog-mode . goto-address-prog-mode) (text-mode . goto-address-mode)))

;; [prettify-symbols] works for prog-mode
(use-package
  prettify-symbols
  :hook (prog-mode . prettify-symbols-mode)
  :custom (prettify-symbols-unprettify-at-point t))

;; [puni]
(use-package
  puni
  :straight t
  :hook ((eval-expression-minibuffer-setup prog-mode) . puni-mode)
  :bind (:map puni-mode-map ("DEL" . +puni-hungry-delete))
  :preface
  (defun +puni-hungry-delete ()
	(interactive)
	(if (looking-back "^[[:blank:]]+")
		(let* ((puni-mode nil)
               (original-func (key-binding (kbd "DEL"))))
          ;; original-func is what `DEL' would be if puni-mode were disabled
          (if (eq original-func 'delete-backward-char)
              (backward-delete-char-untabify 1)
			(call-interactively original-func)))
      (puni-backward-delete-char))))

;; [subword] Handling capitalized subwords
(use-package subword :hook ((minibuffer-setup prog-mode text-mode) . subword-mode))

;; [symbol-overlay] Symbol selection & highlight
(use-package
  symbol-overlay
  :straight t
  :bind
  (:map
   prog-mode-map
   ("C-c s i" . symbol-overlay-put)
   ("C-c s n" . symbol-overlay-switch-forward)
   ("C-c s p" . symbol-overlay-switch-backward)
   ("C-c s c" . symbol-overlay-remove-all))
  (:map
   text-mode-map
   ("C-c s i" . symbol-overlay-put)
   ("C-c s n" . symbol-overlay-switch-forward)
   ("C-c s p" . symbol-overlay-switch-backward)
   ("C-c s c" . symbol-overlay-remove-all))
  :hook ((prog-mode text-mode) . symbol-overlay-mode)
  :custom (symbol-overlay-temp-highlight-on-region t))

;; [embrace] Add/change/delete pairs of symbol
;; (use-package embrace
;;   :straight t
;;   :bind ("C-." . embrace-commander)
;;   :hook (org-mode . embrace-org-mode-hook)
;;   )
