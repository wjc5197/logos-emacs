;;; -*- lexical-binding: t -*-

(defun +code-based-on-system (code)
  "Code block based on the current system type."
  (cond
   ((eq system-type 'windows-nt)
    (car code))
   ((eq system-type 'gnu/linux)
    (cadr code))
   ((eq system-type 'darwin)
    (caddr code))
   ((eq system-type 'android)
    (cadddr code))))

;; (defun +eval-on-dir-files (code dir)
;;   "Execute code block for every file in dir recursively."
;;   (dolist (file (directory-files dir t "\\`[^.]"))
;;     (if (file-directory-p file)
;;         (+eval-on-dir-files code file)
;;       (when (and (file-regular-p file) (file-readable-p file))
;;         (funcall code file)))))

(defun +query-replace-regexps (regexp-replacement-pairs)
  "Replace things after point based on query-replace-regexp & regexp-replacement-pairs."
  (interactive
   (list (eval-expression (read--expression "Query replace regexp-replacement-pairs: ")))
   )
  (dolist (pair regexp-replacement-pairs)
	(save-excursion
	  (query-replace-regexp (regexp-quote (car pair)) (cdr pair))
	  )
	)
  )

(defun +replace-chinese-punctuation-on-current-file ()
  (interactive)
  (+replace-chinese-punctuation-on-file buffer-file-name))

(defun +call-fn-with-pp-to-prin1 (fn &rest args)
  "Call FN with ARGS, map `pp' to `prin1' when called."
  (cl-letf (((symbol-function #'pp) #'prin1)
            ((symbol-function #'pp-to-string) #'prin1-to-string))
    (apply fn args)))

(defun +temp-buffer-p (buffer)
  "Return t if BUFFER is temporary."
  (string-match-p "^ " (buffer-name buffer)))

(defun +load-init-file ()
  (interactive)
  (load user-init-file))

(defun +kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  )
