;;;;\filename dump.el
;;;\Last edited: Thu Sep  8 23:55:05 1994 by lenst@lysita (Lennart Staflin)
;;;\RCS $Id$
;;;\author {Lennart Staflin}
;;;\maketitle

;;\begin{codeseg}
(require 'psgml)
(require 'psgml-parse)
(require 'psgml-edit)
;;(require 'psgml-dtd)
(autoload 'sgml-translate-model "psgml-dtd" "" nil)

;;;; Debugging

(define-key sgml-mode-map "\C-c," 'sgml-goto-cache)
(define-key sgml-mode-map "\C-c\C-x" 'sgml-dump-tree)

(defun sgml-this-element ()
  (interactive)
  (let ((tree (sgml-find-element-of (point))))
    (sgml-dump-rec tree)))

(defun sgml-goto-cache ()
  (interactive)
  (setq sgml-dtd-info (sgml-pstate-dtd sgml-buffer-parse-state)
	sgml-top-tree (sgml-pstate-top-tree sgml-buffer-parse-state))
  (sgml-find-start-point (point))
  (message "%s" (sgml-element-context-string sgml-top-tree)))

(defun sgml-dump-tree (arg)
  (interactive "P")
  (when arg
    (sgml-parse-to-here))
  (with-output-to-temp-buffer "*Dump*"
    (sgml-dump-rec (sgml-pstate-top-tree sgml-buffer-parse-state))))

(defun sgml-comepos (epos)
  (if (sgml-strict-epos-p epos)
      (format "%s:%s"
	      (sgml-entity-name (sgml-eref-entity (sgml-epos-eref epos)))
	      (sgml-epos-pos epos))
    (format "%s" epos)))

(defun sgml-dump-rec (u)
  (while u
    (princ
     (format
      "%s%s start:%s(%s) end:%s(%s) epos:%s/%s net:%s\n"
      (make-string (sgml-tree-level u) ?. )
      (sgml-element-name u)
      (sgml-element-start u) (sgml-tree-stag-len u)
      (if (sgml-tree-etag-epos u) (sgml-tree-end u)) (sgml-tree-etag-len u)
      (sgml-comepos (sgml-tree-stag-epos u))
      (sgml-comepos (sgml-tree-etag-epos u))
      (sgml-tree-net-enabled u)))
    (sgml-dump-rec (sgml-tree-content u))
    (setq u (sgml-tree-next u))))

;;;; For edebug

;;(put 'when 'edebug-form-hook t)
;;(put 'unless 'edebug-form-hook t)
;;(put 'push 'edebug-form-hook '(form sexp))
;;(put 'setf 'edebug-form-hook '(sexp form))

(eval-when (load)
  (def-edebug-spec sgml-with-parser-syntax (&rest form))
  (def-edebug-spec sgml-skip-upto (sexp))
  (def-edebug-spec sgml-check-delim (sexp &optional sexp))
  (def-edebug-spec sgml-parse-delim (sexp &optional sexp))
  (def-edebug-spec sgml-is-delim (sexp &optional sexp sexp sexp)))

;;;; dump

(defun sgml-dump-dtd ()
  (interactive )
  (with-output-to-temp-buffer "*DTD dump*"
    (loop for et being the symbols of
	  (sgml-dtd-eltypes (sgml-pstate-dtd sgml-buffer-parse-state))
	  do (sgml-dp-element et))))

(defun sgml-dump-element (el-name)
  (interactive
   (list (completing-read "Element: "
			  (sgml-dtd-eltypes
			   (sgml-pstate-dtd sgml-buffer-parse-state))
			  nil t)))
  (with-output-to-temp-buffer "*Element dump*"
    (sgml-dp-element (sgml-lookup-eltype el-name))))


(defun sgml-dp-element (el)
  (princ (format "Element %s %s %s%s:\n"
		 (sgml-eltype-name el)
		 (if (sgml-eltype-stag-optional el) "O" "-")
		 (if (sgml-eltype-etag-optional el) "O" "-")
		 (if (sgml-eltype-mixed el) " mixed" "")))
  (cond
   ((sgml-model-group-p (sgml-eltype-model el))
    (sgml-dp-model (sgml-eltype-model el)))
   (t
    (prin1 (sgml-eltype-model el))
    (terpri)))
  (princ (format "Exeptions: +%s -%s\n"
		 (sgml-eltype-includes el)
		 (sgml-eltype-excludes el)))
  (princ (format "Attlist: %S\n" (sgml-eltype-attlist el)))
  (princ (format "Plist: %S\n" (symbol-plist el)))
  (terpri))


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
    (loop
     for file in '("psgml-parse" "psgml-edit" "psgml-dtd")
     do
     (set-buffer (find-file-noselect (concat file ".el")))
     (goto-char (point-min))
     (while (and
	     (not (eobp))
	     (re-search-forward "^(defun +\\([^ ]+\\)" nil t))
       (let ((name (buffer-substring (match-beginning 1)
				     (match-end 1)))
	     doc)
	 (forward-sexp 1)		; skip argument list
	 (skip-chars-forward " \n\t")
	 (when (eq ?\" (following-char)) ; doc string
	       (setq doc (buffer-substring (point)
					   (progn (forward-sexp 1)
						  (point)))))
	 (skip-chars-forward " \n\t")
	 (when (looking-at "(interactive")
	       (if (null doc)
		   (message "No doc for %s" name))
	       (princ (format
		       "(autoload '%s \"%s\" %s t)\n"
		       name file doc))))))))

;;;; Profiling

(defun sgml-instrument-parser ()
  (interactive)
  (require 'elp)
  (setq elp-function-list
	'(
	  sgml-parse-to
	  sgml-parser-loop
	  sgml-parse-s
	  sgml-parse-markup-declaration
	  sgml-parse-processing-instruction
	  sgml-pop-entity
	  sgml-is-enabled-net
	  sgml-do-end-tag
	  sgml-deref-shortmap
	  sgml-element-mixed
	  sgml-do-start-tag
	  sgml-parse-general-entity-ref
	  sgml-set-markup-type
	  sgml-pcdata-move
	  sgml-parse-pcdata
	  ;; In sgml-set-markup-type
	  sgml-set-face-for
	  ;; In sgml-do-end-tag
	  sgml-lookup-eltype
	  sgml-check-tag-close
	  sgml-implied-end-tag
	  sgml-close-element
	  sgml-check-name
	  sgml-eltype-name
	  sgml-final
	  sgml-element-gi
	  sgml-required-tokens
	  ))
  (elp-restore-all)
  (elp-instrument-list))


;¤¤\end{codeseg}
