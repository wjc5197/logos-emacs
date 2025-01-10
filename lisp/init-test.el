;;; -*- lexical-binding: t -*-
(defun +replace-chinese-punctuation-on-file (file)
  "Replace all Chinese punctuation with corresponding English punctuation for file."
  (let ((punctuation-pairs
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
           ("《" . "[")
           ("》" . "]")
           ("〈" . "[")
           ("〉" . "]")
           ("【" . "[")
           ("】" . "]")
           ("『" . "\"")
           ("』" . "\"")
           ("「" . "\"")
           ("」" . "\"")
           ("——" . " - ")
           ("…" . "...")
           ("、" . ","))))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((content (buffer-string)))
        (dolist (pair punctuation-pairs)
          (setq content
                (replace-regexp-in-string (regexp-quote (car pair)) (cdr pair) content)))
        (erase-buffer)
        (insert content)
		(write-region (point-min) (point-max) file)
        ))))

(interactive
 (let ((common
		(query-replace-read-args
		 (concat "Query replace"
				 (if current-prefix-arg
					 (if (eq current-prefix-arg '-) " backward" " word")
				   "")
				 " regexp"
				 (if (use-region-p) " in region" ""))
		 t)))
   (list (nth 0 common) (nth 1 common) (nth 2 common)
		 ;; These are done separately here
		 ;; so that command-history will record these expressions
		 ;; rather than the values they had this time.
		 (use-region-beginning) (use-region-end)
		 (nth 3 common)
		 (use-region-noncontiguous-p))))

(defun +sort-elem-alpha-ord (elem)
  (let ((elem-type (org-element-type elem))
		(content (org-element-contents elem)))
	(cond
	 ;; If elem is a headline, sort entries.
	 ((eq elem-type 'headline)
	  (dolist (subelem elem)
		(when (and (listp subelem) (eq (car subelem) 'headline))
		  (org-element-map subelem 'headline #'+sort-elem-alpha-ord nil nil 'headline)
		  )
		)
	  (goto-char (org-element-property :begin elem))
	  (org-sort-entries nil ?a)
	  )

	 ;; If elem is a plain list, sort the list.
	 ((eq elem-type 'plain-list)
	  (dolist (subelem elem)
		(when (and (listp subelem) (eq (car subelem) 'plain-list))
		  (org-element-map subelem 'plain-list #'+sort-elem-alpha-ord nil nil 'plain-list)
		  )
		)
	  (goto-char (org-element-property :begin elem))
	  (org-sort-list nil ?a))

	 (t
	  (message "Skipping to sort unsupported element type: %s" elem-type))))
  )

(defun +sort-elem-alpha-ord-test (elem)
  (let ((elem-type (org-element-type elem)))
    (cond
     ((member elem-type '(headline plain-list))
	  (org-element-map (org-element-contents elem) elem-type #'+sort-elem-alpha-ord-test nil nil elem-type)
      (goto-char (org-element-property :begin elem))
	  (condition-case nil
		  (cond
		   ((eq elem-type 'headline) (org-sort-entries nil ?a))
		   ((eq elem-type 'plain-list) (org-sort-list nil ?a)))
		(user-error)
		)
	  )
     (t
      (message "Skipping to sort unsupported element type: %s" elem-type)))))

(defun +org-sort-buffer-alpha-ord ()
  (interactive)
  (save-excursion
	(condition-case nil
		(progn
		  (org-element-map (org-element-parse-buffer) 'headline #'+sort-elem-alpha-ord nil nil 'headline)
		  (org-element-map (org-element-parse-buffer) 'plain-list #'+sort-elem-alpha-ord nil nil 'headline)
		  (goto-char (point-min))
		  (org-sort-entries nil ?a)
		  )
	  (user-error)
	  )
	)
  )

(org-map-entries (lambda ()
                   (condition-case x
					   (progn
						 (message "%s" (org-element-property :raw-value (org-element-at-point)))
						 (org-sort-entries nil ?a)
						 )
                     (user-error))))
(condition-case nil
	(dolist (elem (org-element-map (org-element-parse-buffer) 'headline #'identity nil nil 'headline))
	  (message "%s" (org-element-property :raw-value elem)))
  (user-error)
  )
(org-element-map (org-element-parse-buffer) 'headline #'identity nil nil 'headline)
(org-element-map (org-element-parse-buffer) 'plain-list #'identity nil nil 'plain-list)
(length (org-element-map (org-element-parse-buffer) 'headline #'identity nil nil 'headline))
(condition-case nil
	(ignore-errors (org-element-map (org-element-parse-buffer) 'headline #'+sort-elem-alpha-ord-test nil nil 'headline))
  (user-error)
  )
(progn
  (setq +current-time (float-time))
  (org-element-map (org-element-parse-buffer) 'plain-list #'+sort-elem-alpha-ord-test nil nil 'plain-list)
  (message "%s" (- (float-time) +current-time))
  )
(progn
  (setq +current-time (float-time))
  (org-element-map (org-element-parse-buffer) 'plain-list #'+sort-elem-alpha-ord-test nil nil 'plain-list)
  (org-element-map (org-element-parse-buffer) 'headline #'+sort-elem-alpha-ord-test nil nil 'headline)
  (goto-char (point-min))
  (condition-case nil
	  (org-sort-entries nil ?a)
	(user-error))
  (message "%s" (- (float-time) +current-time))
  )

(progn
  (setq +current-time (float-time))
  (org-element-map (org-element-parse-buffer) 'plain-list #'+sort-elem-alpha-ord-test nil nil 'plain-list)
  (org-map-entries (lambda ()
					 (condition-case nil
						 (org-sort-entries nil ?a)
					   (user-error))))
  (goto-char (point-min))
  (condition-case nil
	  (org-sort-entries nil ?a)
	(user-error))
  (message "%s" (- (float-time) +current-time))
  )
(condition-case nil
	(org-element-map (org-element-parse-buffer) 'plain-list #'+sort-elem-alpha-ord-test nil nil 'plain-list)
  (user-error)
  )
(dolist (elem (org-map-entries #'identity))
  (message "%s" (org-element-property :raw-value elem)))

(org-element-map (org-element-parse-buffer) 'headline (lambda (elem)
														(message "%s" (org-element-property :raw-value elem))
														(goto-char (org-element-property :begin elem))
														(condition-case nil
															(org-sort-entries nil ?a)
														  (user-error)
														  )
														)
				 nil nil 'headline)

(
 (item
  (:standard-properties [92 92 94 129 129 0 ... item nil nil nil nil ...] :bullet "- " :checkbox nil :counter nil :pre-blank 0 :tag nil)
  (paragraph (:standard-properties [94 94 94 102 102 0 nil nil nil nil nil nil ...]) #("😡
  wjc
" 0 8 ...))
  (plain-list (:standard-properties [102 102 102 129 129 0 nil nil nil nil nil nil ...] :type unordered) (item ... ...) (item ... ...) (item ... ...)))
 (item
  (:standard-properties [129 129 131 220 220 0 ... item nil nil nil nil ...] :bullet "- " :checkbox nil :counter nil :pre-blank 0 :tag nil)
  (paragraph (:standard-properties [131 131 131 220 220 0 nil nil nil nil nil nil ...]) #("Cauchy's Criterion
  " 0 21 ...) (latex-fragment ...) #("
" 0 1 ...)))
 (item (:standard-properties [220 220 222 437 437 0 ... item nil nil nil nil ...] :bullet "- " :checkbox nil :counter nil :pre-blank 0 :tag nil)
	   (paragraph (:standard-properties [222 222 222 361 361 0 nil nil nil nil nil nil ...]) #("Complete Space
  A metric space " 0 32 ...) (latex-fragment ...) #("is said to be complete if every Cauchy sequence of points in " 0 61 ...) (latex-fragment ...) #("has a limit that is also in " 0 28 ...) (latex-fragment ...) #("
" 0 1 ...))
	   (plain-list (:standard-properties [361 361 361 437 437 0 nil nil nil nil nil nil ...] :type unordered) (item ... ...)))
 (item (:standard-properties [437 437 439 626 626 0 ... item nil nil nil nil ...] :bullet "- " :checkbox nil :counter nil :pre-blank 0 :tag nil)
	   (paragraph (:standard-properties [439 439 439 502 502 0 nil nil nil nil nil nil ...]) #("Diameter of " 0 12 ...) (latex-fragment ...) #("
  " 0 3 ...) (latex-fragment ...) #("
" 0 1 ...))
	   (plain-list (:standard-properties [502 502 502 626 626 0 nil nil nil nil nil nil ...] :type unordered) (item ... ...) (item ... ...)))
 (item (:standard-properties [626 626 628 905 905 0 ... item nil nil nil nil ...] :bullet "- " :checkbox nil :counter nil :pre-blank 0 :tag nil)
	   (paragraph (:standard-properties [628 628 628 905 905 0 nil nil nil nil nil nil ...]) #("Nested Interval Thm
  If " 0 25 ...)
				  (latex-fragment ...) #("is a sequence of nonempty compact sets in a metric space " 0 57 ...)
				  (latex-fragment ...) #("such that " 0 10 ...)
				  (latex-fragment ...) #("for " 0 4 ...)
				  (latex-fragment ...) #("and if " 0 7 ...)
				  (latex-fragment ...) ...))
 )
