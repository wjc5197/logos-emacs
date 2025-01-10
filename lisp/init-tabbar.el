;; -*- lexical-binding: t; -*-

(use-package
  tab-bar
  :hook (after-init . tab-bar-mode)
  ;; :preface
  ;; (defun +switch-to-tab (n)
  ;; 	"Switch to the N-th tab (1-based index)."
  ;; 	(interactive "Tab ord (1-9): ")
  ;; 	(when (and (>= n 1) (<= n 9))
  ;;     (tab-bar-select-tab n)))
  :custom
  ;; (tab-bar-select-tab-modifiers '(meta))
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-auto-width nil)
  (tab-bar-separator "")
  ;; (tab-bar-show 1) ;; hide bar if <= 1 tabs open
  (tab-bar-close-button-show nil) ;; hide tab close / X button
  (tab-bar-new-tab-choice "*dashboard*") ;; buffer to show in new tabs
  (tab-bar-tab-hints t) ;; show tab numbers
  (tab-bar-tab-name-format-function
   (lambda (tab i)
     (let ((face (funcall tab-bar-tab-face-function tab)))
       (concat
		(propertize " " 'face face)
		(propertize (number-to-string i)
					'face
					`(:inherit ,face :weight ultra-bold :underline t))
		(propertize (concat " " (alist-get 'name tab) " ") 'face face)))))
  (tab-bar-format '(tab-bar-format-tabs tab-bar-format-add-tab))
  ;; :config
  ;; (dotimes (i 9)
  ;; 	(let ((key (kbd (format "M-%d" (1+ i)))))
  ;;     (global-set-key key `(lambda ()
  ;; 							 (interactive)
  ;; 							 (+switch-to-tab ,(1+ i))))))
  )
