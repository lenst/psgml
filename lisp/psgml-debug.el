;;;;\filename dump.el
;;;\Last edited: Mon May  9 01:18:23 1994 by lenst@lysita (Lennart Staflin)
;;;\RCS $Id$
;;;\author {Lennart Staflin}
;;;\maketitle

;;\begin{codeseg}
(require 'psgml)
(require 'psgml-parse)
;;(require 'psgml-dtd)
(autoload 'sgml-translate-model "psgml-dtd" "" nil)

;;;; Debugging

(define-key sgml-mode-map "\C-c," 'sgml-goto-cache)
(define-key sgml-mode-map "\C-c\C-x" 'sgml-dump)

(defun sgml-this-element ()
  (interactive)
  (let ((tree (sgml-find-element-of (point))))
    (sgml-dump-rec tree)))

(defun sgml-goto-cache ()
  (interactive)
  (sgml-find-start-point (point))
  (message "%s" (sgml-context-as-string)))

(defun sgml-dump (arg)
  (interactive "P")
  (when arg
    (sgml-parse-to-here))
  (with-output-to-temp-buffer "*Dump*"
    (sgml-dump-rec sgml-top-tree)))

(defun sgml-dump-rec (u)
  (while u
    (princ
     (format
      "%s%s start:%s(%s) end:%s(%s)\n"
      (make-string (sgml-tree-level u) ?. )
      (sgml-element-name u)
      (sgml-tree-start u) (sgml-tree-stag-len u)
      (sgml-tree-end u) (sgml-tree-etag-len u)))
    (sgml-dump-rec (sgml-tree-content u))
    (setq u (sgml-tree-next u))))

;;;; For edebug

(put 'when 'edebug-form-hook t)
(put 'unless 'edebug-form-hook t)
(put 'push 'edebug-form-hook '(form sexp))
(put 'setf 'edebug-form-hook '(sexp form))


;;;; dump

(defun dump-dtd ()
  (interactive )
  (with-output-to-temp-buffer "*DTD dump*"
    (let ((l sgml-buffer-eltype-map))
      (while l
	(sgml-dp-element (cdr (car l)))
	(setq l (cdr l))))))

(defun dump-element (el-name)
  (interactive
   (list (completing-read "Element: "
			  (mapcar (lambda (p) (cons (symbol-name (car p)) nil))
				  sgml-buffer-eltype-map)
			  nil t)))
  (with-output-to-temp-buffer "*Element dump*"
    (sgml-dp-element (sgml-lookup-element el-name))))


(defun sgml-dp-element (el)
  (princ (format "Element %s:\n" (sgml-eltype-name el)))
  (cond
   ((sgml-model-group-p (sgml-eltype-model el))
    (sgml-dp-model (sgml-eltype-model el)))
   (t
    (prin1 (sgml-eltype-model el))
    (terpri))))

(defun sgml-dp-model (model &optional indent)
  (or indent (setq indent 0))
  (let ((sgml-code-xlate (sgml-translate-model model)))
    (loop
     for i from 0
     for x in sgml-code-xlate do
     (cond ((sgml-normal-state-p (car x))
	    (princ (format "%s%d: opts=%s reqs=%s\n"
			   (make-string indent ? ) i
			   (sgml-untangel-moves (sgml-state-opts (car x)))
			   (sgml-untangel-moves (sgml-state-reqs (car x))))))
	   (t				; &node
	    (princ (format "%s%d: &node next=%d\n"
			   (make-string indent ? ) i
			   (sgml-code-xlate (sgml-&node-next (car x)))))
	    (loop for m in (sgml-&node-dfas (car x))
		  do (sgml-dp-model m (+ indent 2))))))))

(defun sgml-untangel-moves (moves)
  (loop for m in moves
	collect (list (sgml-move-token m)
		      (sgml-code-xlate (sgml-move-dest m)))))


;;;; Build autoloads for all interactive functions in psgml-parse

(defun sgml-build-autoloads ()
  (interactive)
  (with-output-to-temp-buffer "*autoload*"
    (set-buffer (find-file-noselect "psgml-parse.el"))
    (goto-char (point-min))
    (while (and
	    (not (eobp))
	    (re-search-forward "^(defun +\\([^ ]+\\)" nil t))
      (let ((name (buffer-substring (match-beginning 1)
				    (match-end 1)))
	    doc)
	(forward-sexp 1)		; skip argument list
	(skip-chars-forward " \n\t")
	(when (eq ?\" (following-char))	; doc string
	  (setq doc (buffer-substring (point)
				      (progn (forward-sexp 1)
					     (point)))))
	(skip-chars-forward " \n\t")
	(when (looking-at "(interactive")
	  (if (null doc)
	      (message "No doc for %s" name))
	  (princ (format
		  "(autoload '%s \"psgml-parse\" %s t)\n"
		  name doc)))))))

;¤¤\end{codeseg}
