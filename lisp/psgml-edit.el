;;;; psgml-edit.el --- Editing commands for SGML-mode with parsing support
;; $Id$

;; Copyright (C) 1994 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;;; Commentary:

;; Part of major mode for editing the SGML document-markup language.


;;;; Code:

(provide 'psgml-edit)
(require 'psgml)
(require 'psgml-parse)


;;;; Variables

(defvar sgml-split-level nil
  "Used by sgml-split-element")


;;;; SGML mode: structure editing

(defun sgml-last-element ()
  "Return the element where last command left point.
This either uses the save value in `sgml-last-element' or parses the buffer
to find current open element."
  (setq sgml-markup-type nil)
  (if (and (memq last-command sgml-users-of-last-element)
	   sgml-last-element)		; Don't return nil
      sgml-last-element
    (setq sgml-last-element (sgml-find-context-of (point))))  )

(defun sgml-set-last-element (&optional el)
  (if el (setq sgml-last-element el))
  (sgml-show-context sgml-last-element))

(defun sgml-beginning-of-element ()
  "Move to after the start-tag of the current element.
If the start-tag is implied, move to the start of the element."
  (interactive)
  (goto-char (sgml-element-stag-end (sgml-last-element)))
  (sgml-set-last-element))

(defun sgml-end-of-element ()
  "Move to before the end-tag of the current element."
  (interactive)
  (goto-char (sgml-element-etag-start (sgml-last-element)))
  (sgml-set-last-element))

(defun sgml-backward-up-element ()
  "Move backward out of this element level.
That is move to before the start-tag or where a start-tag is implied."
  (interactive)
  (goto-char (sgml-element-start (sgml-last-element)))
  (sgml-set-last-element (sgml-element-parent sgml-last-element)))

(defun sgml-up-element ()
  "Move forward out of this element level.
That is move to after the end-tag or where an end-tag is implied."
  (interactive)
  (goto-char (sgml-element-end (sgml-last-element)))
  (sgml-set-last-element (sgml-element-parent sgml-last-element)))

(defun sgml-forward-element ()
  "Move forward over next element."
  (interactive)
  (let ((next
	 (sgml-find-element-after
	  (point)
	  (if (memq last-command sgml-users-of-last-element)
	      sgml-last-element))))
    (goto-char (sgml-element-end next))
    (sgml-set-last-element (sgml-element-parent next))))

(defun sgml-backward-element ()
  "Move backward over previous element at this level.
With implied tags this is ambigous."
  (interactive)
  (let ((prev				; previous element
	 (sgml-find-previous-element
	  (point)
	  (if (memq last-command sgml-users-of-last-element)
	      sgml-last-element))))
    (goto-char (sgml-element-start prev))
    (sgml-set-last-element (sgml-element-parent prev))))

(defun sgml-down-element ()
  "Move forward and down one level in the element structure."
  (interactive)
  (goto-char
   (sgml-element-stag-end
    (setq sgml-last-element
	  (sgml-find-element-after
	   (point)
	   (if (memq last-command sgml-users-of-last-element)
	       sgml-last-element)))))
  (sgml-set-last-element
   (if (sgml-element-empty sgml-last-element)
       (setq sgml-last-element (sgml-element-parent sgml-last-element))
     sgml-last-element)))

(defun sgml-kill-element ()
  "Kill the element following the cursor."
  (interactive "*")
  (kill-region (point)
	       (sgml-element-end (sgml-find-element-after (point)))))

(defun sgml-transpose-element ()
  "Interchange element before point with element after point, leave point after."
  (interactive "*")
  (let ((pre (sgml-find-previous-element (point)))
	(next (sgml-find-element-after (point)))
	s1 s2 m2)
    (goto-char (sgml-element-start next))
    (setq m2 (point-marker))
    (setq s2 (buffer-substring (point)
			       (sgml-element-end next)))
    (delete-region (point) (sgml-element-end next))
    (goto-char (sgml-element-start pre))
    (setq s1 (buffer-substring (point) (sgml-element-end pre)))
    (delete-region (point) (sgml-element-end pre))
    (insert-before-markers s2)
    (goto-char m2)
    (insert s1)
    (sgml-message "")))

(defun sgml-mark-element ()
  "Set mark after next element."
  (interactive)
  (push-mark (sgml-element-end (sgml-find-element-after (point))) nil t))

(defun sgml-mark-current-element ()
  "Set mark at end of current element, and leave point before current element."
  (interactive)
  (let ((el (sgml-find-element-of (point))))
    (goto-char (sgml-element-start el))
    (push-mark (sgml-element-end el) nil t)))


(defun sgml-change-element-name (gi)
  "Replace the name of the current element with a new name.
Eventual attributes of the current element will be translated if 
possible."
  (interactive
   (list (let ((el (sgml-find-element-of (point))))
	   (goto-char (sgml-element-start el))
	   (sgml-read-element-name
	    (format "Change %s to: " (sgml-element-name el))))))
  (when (or (null gi) (equal gi ""))
    (error "Illegal name"))
  (let* ((element (sgml-find-element-of (point)))
	 (attspec (sgml-element-attribute-specification-list element))
	 (oldattlist (sgml-element-attlist element)))
    (unless (sgml-element-empty element)
      (goto-char (sgml-element-end element))
      (delete-char (- (sgml-element-etag-len element)))
      (insert (sgml-end-tag-of gi)))
    (goto-char (sgml-element-start element))
    (delete-char (sgml-element-stag-len element))
    (insert (sgml-start-tag-of gi))
    (forward-char -1)
    (let* ((newel (sgml-find-element-of (point)))
	   (newattlist (sgml-element-attlist newel))
	   (newasl (sgml-translate-attribute-specification-list
		    attspec oldattlist newattlist)))
      (sgml-insert-attributes newasl newattlist))))

(defun sgml-translate-attribute-specification-list (values from to)
  "Translate attribute specification from one element type to another.
Input attribute values in VALUES using attlist FROM is translated into
a list using attlist TO."
  (let ((new-values nil)
	(sgml-show-warnings t)
	tem)
    (loop for attspec in values 
	  as from-decl = (sgml-lookup-attdecl (sgml-attspec-name attspec) from)
	  as to-decl   = (sgml-lookup-attdecl (sgml-attspec-name attspec) to)
	  do
	  (cond
	   ;; Special case ID attribute
	   ((and (eq 'id (sgml-attdecl-declared-value from-decl))
		 (setq tem (sgml-attribute-with-declared-value to 'id)))
	    (push
	     (sgml-make-attspec (sgml-attdecl-name tem)
				(sgml-attspec-attval attspec))
	     new-values))
	   ;; Use attribute with same name if compatible type
	   ((equal (sgml-attdecl-declared-value from-decl)
		   (sgml-attdecl-declared-value to-decl))
	    (push attspec new-values))
	   (to-decl
	    (sgml-log-warning
	     "Attribute %s has new declared-value"
	     (sgml-attspec-name attspec))
	    (push attspec new-values))
	   (t
	    (sgml-log-warning "Can't translate attribute %s = %s"
			      (sgml-attspec-name attspec)
			      (sgml-attspec-attval attspec)))))
    new-values))

(defun sgml-untag-element ()
  "Remove tags from current element."
  (interactive "*")
  (let ((el (sgml-find-element-of (point))))
    (when (or (sgml-strict-epos-p (sgml-element-stag-epos el))
	      (sgml-strict-epos-p (sgml-element-etag-epos el)))
      (error "Current element has some tag inside an entity reference"))
    (goto-char (sgml-element-etag-start el))
    (delete-char (sgml-element-etag-len el))
    (goto-char (sgml-element-start el))
    (delete-char (sgml-element-stag-len el))))

(defun sgml-kill-markup ()
  "Kill next tag, markup declaration or process instruction."
  (interactive "*")
  (let ((start (point)))
    (sgml-with-parser-syntax
     (sgml-parse-s)
     (setq sgml-markup-start (point))
     (cond ((sgml-parse-markup-declaration 'ignore))
	   ((sgml-parse-processing-instruction))
	   ((sgml-skip-tag)))
     (kill-region start (point)))))


;;;; SGML mode: folding

(defun sgml-fold-region (beg end &optional unhide)
  "Hide (or if prefixarg unhide) region.
If called from a program first two arguments are start and end of
region. And optional third argument true unhides."
  (interactive "r\nP")
  (let ((mp (buffer-modified-p))
	(inhibit-read-only t)		;
	(buffer-read-only nil)		; should not need this, but
					; perhaps some old version of
					; emacs does not understand
					; inhibit-read-only
	(before-change-function nil)
	(after-change-function nil))
    (setq selective-display t)
    (unwind-protect
	(subst-char-in-region beg end
			      (if unhide ?\r ?\n)
			      (if unhide ?\n ?\r)
			      'noundo)
      (when sgml-buggy-subst-char-in-region
	(set-buffer-modified-p mp)))))

(defun sgml-fold-element ()
  "Fold the lines comprising the current element, leaving the first line visible.
This uses the selective display feature."
  (interactive)
  (sgml-parse-to-here)
  (cond ((and (eq sgml-current-tree sgml-top-tree) ; outside document element
	      sgml-markup-type)
	 (sgml-fold-region sgml-markup-start
			   (save-excursion
			     (sgml-parse-to (point))
			     (point))))
	((and (eq sgml-current-tree sgml-top-tree) ; outside document element
	      (looking-at " *<!"))
	 (sgml-fold-region (point)
			   (save-excursion
			     (skip-chars-forward " \t")
			     (sgml-parse-to (1+ (point)))
			     (point))))

	(t
	 (let ((el (sgml-find-element-of (point))))
	   (when (eq el sgml-top-tree)
	     (error "No element here"))
	   (save-excursion
	     (goto-char (sgml-element-end el))
	     (when (zerop (sgml-element-etag-len el))
	       (skip-chars-backward " \t\n"))
	     (sgml-fold-region (sgml-element-start el)
			       (point)))))))

(defun sgml-fold-subelement ()
  "Fold all elements current elements content, leaving the first lines visible.
This uses the selective display feature."
  (interactive)
  (let* ((el (sgml-find-element-of (point)))
	 (start (sgml-element-start el))
	 (end (sgml-element-end el))
	 (c (sgml-element-content el)))
    (while c
      (sgml-fold-region (sgml-element-start c)
			(sgml-element-end c))
      (setq c (sgml-element-next c)))))

(defun sgml-unfold-line ()
  "Show hidden lines in current line."
  (interactive)
  (let ((op (point)))
    (beginning-of-line)
    (push-mark)
    (end-of-line)
    (exchange-point-and-mark)
    (sgml-fold-region (point) (mark) 'unhide)
    (goto-char op)))

(defun sgml-unfold-element ()
  "Show all hidden lines in current element."
  (interactive)
  (let* ((element (sgml-find-element-of (point))))
    (sgml-fold-region (sgml-element-start element)
		      (sgml-element-end element)
		      'unfold)))

(defun sgml-expand-element ()
  "As sgml-fold-subelement, but unfold first."
  (interactive)
  (sgml-unfold-element)
  (sgml-fold-subelement))

(defun sgml-unfold-all ()
  "Show all hidden lines in buffer."
  (interactive)
  (sgml-fold-region (point-min)
		    (point-max)
		    'unfold))

;;;; SGML mode: indentation and movement

(defun sgml-indent-line (&optional col element)
  "Indent line, calling parser to determine level unless COL or ELEMENT
is given.  If COL is given it should be the column to indent to.  If
ELEMENT is given it should be a parse tree node, from which the level
is determined."
  (when sgml-indent-step
    (let ((here (point-marker)))
      (back-to-indentation)
      (unless (or col element)
	;; Determine element
	(setq element (if (eobp)
			  (sgml-find-context-of (point))
			(sgml-find-element-of (point)))))
      (when (eq element sgml-top-tree)	; not in a element at all
	(setq element nil)		; forget element
	(goto-char here)		; insert normal tab insted
	(insert-tab))
      (when element
	(sgml-with-parser-syntax
	 (let ((stag (sgml-is-start-tag))
	       (etag (sgml-is-end-tag)))
	   (when (or sgml-indent-data
		     (not (sgml-element-data-p
			   (if stag
			       (sgml-element-parent element)
			     element))))
	     (setq col
		   (* sgml-indent-step
		      (+ (if (or stag etag) -1 0)
			 (sgml-element-level element))))))))
      (when (and col (/= col (current-column)))
	(beginning-of-line 1)    
	(delete-horizontal-space)
	(indent-to col))
      (when (< (point) here)
	(goto-char here))
      col)))

(defun sgml-next-data-field ()
  "Move forward to next point where data is allowed."
  (interactive)
  (when (eobp)
    (error "End of buffer"))
  (let ((sgml-throw-on-warning 'next-data)
	(avoid-el (sgml-last-element)))
    ;; Avoid stopping in current element, unless point is in the start
    ;; tag of the element
    (when (< (point) (sgml-element-stag-end avoid-el))
      (setq avoid-el nil))
    (catch sgml-throw-on-warning
      (while (progn
	       (sgml-parse-to (1+ (point)))
	       (setq sgml-last-element
		     (if (not (eq ?< (following-char)))
			 (sgml-find-element-of (point))
		       sgml-current-tree))
	       (or (eq sgml-last-element avoid-el)
		   (not (sgml-element-data-p sgml-last-element)))))
      (sgml-set-last-element))))

(defun sgml-next-trouble-spot ()
  "Move forward to next point where something is amiss with the structure."
  (interactive)
  (push-mark)
  (sgml-note-change-at (point))		; Prune the parse tree
  (sgml-parse-to (point))
  (let ((sgml-last-trouble-spot (point))
	(sgml-throw-on-warning 'trouble))
    (or (catch sgml-throw-on-warning
	  (sgml-parse-until-end-of nil t))
	(message "Ok"))))



;;;; SGML mode: information display

(defun sgml-list-valid-tags ()
  "Display a list of the contextually valid tags."
  (interactive)
  (sgml-parse-to-here)
  (let ((model (sgml-element-model sgml-current-tree)))
    (with-output-to-temp-buffer "*Tags*"
      (princ (format "Current element: %s\n"
		     (sgml-element-name sgml-current-tree)))
      (cond ((or (sgml-current-mixed-p)
		 (eq model sgml-any))
	     (princ "Current element has mixed content")
	     (when (eq model sgml-any)
	       (princ " [ANY]"))
	     (terpri))
	    ((sgml-model-group-p model)
	     (princ "Current element has element content\n"))
	    (t
	     (princ (format "Current element has declared content: %s\n"
			    model))))
      (cond ((sgml-final-p sgml-current-state)
	     (princ "Valid end-tags: ")
	     (loop for e in (sgml-current-list-of-endable-eltypes)
		   do (princ (sgml-end-tag-of e)) (princ " "))
	     (terpri))
	    (t
	     (princ "Current element can not end here\n")))
;;;      (let ((s (sgml-tree-shortmap sgml-current-tree)))
;;;	(when s
;;;	  (princ (format "Current shortref map: %s\n" s))))
      (princ "Valid start-tags\n")
      (sgml-print-valid-tags "In current element:"
			     sgml-current-tree sgml-current-state))))

(defun sgml-print-valid-tags (prompt tree state &optional exclude omitted-stag)
  (if (not (sgml-model-group-p state))
      (princ (format "%s (in %s)\n" prompt state))
    (let* ((req (sgml-required-tokens state))
	   (elems (nconc req
			 (delq sgml-pcdata-token
			       (sgml-optional-tokens state))))
	   (in (sgml-tree-includes tree))
	   (ex (append exclude (sgml-tree-excludes tree))))
      ;; Modify for exceptions
      (while in
	(unless (memq (car in) elems)
	  (setq elems (nconc elems (list (car in)))))
	(setq in (cdr in)))
      (while ex
	(setq elems (delq (car ex) elems))
	(setq ex (cdr ex)))
      ;; 
      (setq elems (sort elems (function string-lessp)))
      (sgml-print-list-of-tags prompt elems)
      ;; Check for omissable start-tags
      (when (and req (null (cdr req)))
	;; *** Assumes tokens are eltypes
	(let ((el (sgml-fake-open-element tree (car req))))
	  (when (sgml-element-stag-optional el)
	    (sgml-print-valid-tags
	     (format "If omitting %s:" (sgml-start-tag-of el))
	     el
	     (sgml-element-model el)
	     (append exclude elems)
	     'omitted-stag))))
      ;; Check for omissable end-tag
      (when (and (not omitted-stag)
		 (sgml-final-p state)
		 (sgml-element-etag-optional tree))
	(sgml-print-valid-tags
	 (format "If omitting %s:" (sgml-end-tag-of tree))
	 (sgml-element-parent tree)
	 (sgml-element-pstate tree)
	 (append exclude elems))))))

(defun sgml-print-list-of-tags (prompt list)
  (when list
    (princ prompt)
    (let ((col (length prompt))
	  (w   (1- (frame-width))))
      (loop for e in list
	    as str = (sgml-start-tag-of e)
	    do
	    (setq col (+ col (length str) 2))
	    (cond ((>= col w)
		   (setq col (+ (length str) 2))
		   (terpri)))
	    (princ "  ")
	    (princ str))
      (terpri))))

(defun sgml-show-context (&optional element)
  "Display where the cursor is in the element hierarchy."
  (interactive)
  (let* ((el (or element (sgml-last-element)))
	 (model (sgml-element-model el)))
    (sgml-message "%s %s" 
		  (cond
		   ((and (null element)	; Don't trust sgml-markup-type if
					; explicit element is given as argument
			 sgml-markup-type))
		   ((sgml-element-mixed el)
		    "#PCDATA")
		   ((not (sgml-model-group-p model))
		    model)
		   (t ""))
		  (if (eq el sgml-top-tree)
		      "in empty context"
		    (sgml-element-context-string el)))))

(defun sgml-what-element ()
  "Display what element is under the cursor."
  (interactive)
  (let ((el (sgml-find-element-of (point))))
    (assert (not (null el)))
    (message "%s %s"
	     (cond ((eq el sgml-top-tree)
		    "outside document element")
		   ((< (point) (sgml-element-stag-end el))
		    "start-tag")
		   ((>= (point) (sgml-element-etag-start el))
		    "end-tag")
		   (t
		    "content"))
	     (sgml-element-context-string el))))

;;;; SGML mode: keyboard inserting

(defun sgml-insert-tag (tag &optional silent no-nl-after)
  "Insert a tag, reading tag name in minibuffer with completion.
If the variable sgml-balanced-tag-edit is t, also inserts the
corresponding end tag. If sgml-leave-point-after-insert is t, the point
is left after the inserted tag(s), unless the element has som required
content.  If sgml-leave-point-after-insert is nil the point is left
after the first tag inserted."
  (interactive 
   (list
    (completing-read "Tag: " (sgml-completion-table) nil t "<" )))
  (sgml-find-context-of (point))
  (assert (null sgml-markup-type))
  ;; Fix white-space before tag
  (unless (sgml-element-data-p (sgml-parse-to-here))
    (skip-chars-backward " \t")
    (cond ((bolp)
	   (if (looking-at "^\\s-*$")
	       (fixup-whitespace)))
	  (t
	   (insert "\n"))))
  (insert tag)
  (sgml-indent-line)  
  (unless no-nl-after
    (save-excursion
      (unless (sgml-element-data-p (sgml-parse-to-here))
	(unless (eolp)
	  (save-excursion (insert "\n"))))))
  (or silent (sgml-show-context)))

(defun sgml-insert-element (name &optional after silent)
  "Reads element name from minibuffer and inserts start and end tags."
  (interactive (list (sgml-read-element-name "Element: ")
		     sgml-leave-point-after-insert))
  (let (stag-end			; position after start tag
	element				; inserted element
	(sgml-show-warnings nil))
    (when (and name (not (equal name "")))
      (sgml-insert-tag (sgml-start-tag-of name) 'silent)
      (forward-char -1)
      (setq element (sgml-find-element-of (point)))
      (sgml-insert-attributes nil (sgml-element-attlist element))
      (forward-char 1)
      (setq stag-end (point))
      (when (not (sgml-element-empty element))
	(when (and sgml-auto-insert-required-elements
		   (sgml-model-group-p sgml-current-state))
	  (let (tem newpos)
	    (while (and (setq tem (sgml-required-tokens sgml-current-state))
			(null (cdr tem)))
	      (setq tem (sgml-insert-element (car tem) t t))
	      (setq newpos (or newpos tem))
	      (sgml-parse-to-here))
	    (when tem			; more than one req elem
	      (insert (format "\n<!-- one of %s -->" tem))
	      (sgml-indent-line nil element))
	    (if newpos (setq stag-end newpos))))
	(sgml-insert-tag (sgml-end-tag-of name) 'silent)
	(unless after
	  (goto-char stag-end)
	  (sgml-end-of-element))
	(unless silent (sgml-show-context)))
      stag-end)))

(defun sgml-tag-region (element start end)
  "Reads element name from minibuffer and inserts start and end tags."
  (interactive
   (list
    (save-excursion (goto-char (region-beginning))
		    (sgml-read-element-name "Tag region with element: "))
    (region-beginning)
    (region-end)))
  (save-excursion
    (when (and element (not (equal element "")))
      (goto-char end)
      (insert (sgml-end-tag-of element))
      (goto-char start)
      (sgml-insert-tag (sgml-start-tag-of element)))))

(defun sgml-insert-attributes (avl attlist)
  "Insert the attributes with values AVL and declarations ATTLIST.
AVL should be a assoc list mapping symbols to strings."
  (let (name val dcl def tem)
    (loop for attspec in attlist do
	  (setq name (sgml-attspec-name attspec)
		val (cdr-safe (sgml-lookup-attspec name avl))
		dcl (sgml-attdecl-declared-value attspec)
		def (sgml-attdecl-default-value attspec))
	  (unless val			; no value given
	    ;; Supply the default value if a value is needed
	    (cond ((sgml-default-value-type-p 'required def)
		   (setq val ""))
		  ((and (not (or sgml-omittag sgml-shorttag))
			(consp def))
		   (setq val (sgml-default-value-attval def)))))
	  (cond 
	   ((null val))			; Ignore
	   ;; Ignore attributes with default value
	   ((and (consp def)		
		 (eq sgml-minimize-attributes 'max)
		 (or sgml-omittag sgml-shorttag)
		 (equal val (sgml-default-value-attval def))))
	   ;; No attribute name for token groups
	   ((and sgml-minimize-attributes sgml-shorttag
		 (member (sgml-general-case val)
			 (sgml-declared-value-token-group dcl)))
	    (insert " " val))
	   (t
	    (insert " " name "=" (sgml-quote-attribute-value val)))))))

(defun sgml-quote-attribute-value (value)
  "Add quotes to the string VALUE unless minimization is on."
  (let ((quote ""))
	(cond ((and (not sgml-always-quote-attributes)
		    sgml-shorttag
		    (string-match "\\`[.A-Za-z0-9---]+\\'" value))
	       ) ; no need to quote
	      ((not (string-match "\"" value)) ; can use "" quotes
	       (setq quote "\""))
	      (t			; use '' quotes
	       (setq quote "'")))
	(concat quote value quote)))

(defun sgml-completion-table (&optional avoid-tags-in-cdata)
  (sgml-parse-to-here)
  (when sgml-markup-type
    (error "No tags allowed"))
  (cond ((or (sgml-model-group-p sgml-current-state)
	     (eq sgml-current-state sgml-any))
	 (append
	  (mapcar (function (lambda (x) (cons (sgml-end-tag-of x) x)))
		  (sgml-current-list-of-endable-eltypes))
	  (mapcar (function (lambda (x) (cons (sgml-start-tag-of x) x)))
		  (sgml-current-list-of-valid-eltypes))))
	(t
	 (sgml-message "%s" sgml-current-state)
	 nil)))

(defun sgml-insert-end-tag ()
  "Insert end-tag for the current open element."
  (interactive "*")
  (sgml-parse-to-here)
  (cond
   ((eq sgml-current-tree sgml-top-tree)
    (sgml-error "No open element"))
   ((not (sgml-final-p sgml-current-state))
    (sgml-error "Can`t end element here"))
   (t
    (when (and sgml-indent-step
	       (not (sgml-element-data-p sgml-current-tree)))
      (delete-horizontal-space)
      (unless (bolp)
	(insert "\n")))
    (when (prog1 (bolp)
	    (insert (if (eq t (sgml-element-net-enabled sgml-current-tree))
			"/"
		      (sgml-end-tag-of sgml-current-tree))))
      (sgml-indent-line)))))

(defun sgml-insert-start-tag (name asl attlist &optional net)
  (insert "<" name)
  (sgml-insert-attributes asl attlist)
  (insert (if net "/" ">")))

(defun sgml-change-start-tag (element asl)
  (let ((name (sgml-element-gi element))
	(attlist (sgml-element-attlist element)))
    (assert (sgml-bpos-p (sgml-element-stag-epos element)))
    (goto-char (sgml-element-start element))
    (delete-char (sgml-element-stag-len element))
    (sgml-insert-start-tag name asl attlist
			   (eq t (sgml-element-net-enabled element)))))

(defun sgml-read-attribute-value (attdecl curvalue)
  (assert attdecl)
  (let* ((name (sgml-attdecl-name attdecl))
	 (dv (sgml-attdecl-declared-value attdecl))
	 (tokens (sgml-declared-value-token-group dv))
	 (notations (sgml-declared-value-notation dv))
	 (type (cond (tokens "token")
		     (notations "notation")
		     (t (symbol-name dv))))
	 (prompt
	  (format "Value for %s (%s%s): "
		  name type
		  (if curvalue
		      (format " Default: %s" curvalue)
		    "")))
	 value)
    (setq value 
	  (if (or tokens notations)
	      (completing-read prompt
			       (mapcar 'list (or tokens notations))
			       nil t)
	    (read-string prompt)))
    (if (and curvalue (equal value ""))
	curvalue value)))

(defun sgml-non-fixed-attributes (attlist)
  (loop for attdecl in attlist
	unless (sgml-default-value-type-p 'fixed 
					  (sgml-attdecl-default-value attdecl))
	collect attdecl))

(defun sgml-insert-attribute (name value)
  "Read attribute name and value from minibuffer and insert attribute spec."
  (interactive
   (let* ((el (sgml-find-attribute-element))
	  (name
	   (completing-read
	    "Attribute name: "
	    (mapcar (function (lambda (a) (list (sgml-attdecl-name a))))
		    (sgml-non-fixed-attributes (sgml-element-attlist el)))
	    nil t)))
     (list name
	   (sgml-read-attribute-value
	    (sgml-lookup-attdecl name (sgml-element-attlist el))
	    (sgml-element-attval el name)))))
  ;; Body
  (assert (stringp name))
  (assert (or (null value) (stringp value)))
  (let* ((el (sgml-find-attribute-element))
	 (asl (cons (sgml-make-attspec name value)
		    (sgml-element-attribute-specification-list el)))
	 (in-tag (< (point) (sgml-element-stag-end el))))
    (sgml-change-start-tag el asl)
    (when in-tag (forward-char -1))))

(defun sgml-split-element ()
  "Split the current element at point.
If repeated, the containing element will be split before the beginning
of then current element."
  (interactive "*")
  (setq sgml-split-level
	(if (eq this-command last-command)
	    (1+ sgml-split-level)
	  0))
  (let ((u (sgml-find-context-of (point)))
	(start (point-marker))
	before)
    (loop repeat sgml-split-level do
	  (goto-char (sgml-element-start u))
	  (setq u (sgml-element-parent u)))
    ;; Verify that a new element can be started
    (unless (and (sgml-element-pstate u) ; in case of top element
		 (sgml-get-move (sgml-element-pstate u)
				(sgml-element-name u)))
      
      (sgml-error "The %s element can't be split"
		  (sgml-element-name u)))
    ;; Do the split
    (sgml-insert-end-tag)
    (sgml-insert-tag (sgml-start-tag-of u) 'silent)
    (skip-chars-forward " \t\n")
    (sgml-indent-line)
    (when (> sgml-split-level 0)
      (goto-char start))
    (or (eq sgml-top-tree
	    (setq u (sgml-element-parent u)))
	(sgml-message
	 "Repeat the command to split the containing %s element"
	 (sgml-element-name u)))))

;;; David Megginson's custom menus for keys

(defun sgml-custom-dtd (doctype)
  "Insert a DTD declaration from the sgml-custom-dtd alist."
  (interactive
   (list (completing-read "Insert DTD: " sgml-custom-dtd nil t)))
  (apply 'sgml-doctype-insert (cdr (assoc doctype sgml-custom-dtd))))

(defun sgml-custom-markup (markup)
  "Insert markup from the sgml-custom-markup alist."
  (interactive
   (list (completing-read "Insert Markup: " sgml-custom-markup nil t)))
  (sgml-insert-markup (cadr (assoc markup sgml-custom-markup))))


;;;; SGML mode: Menu inserting

(defun sgml-tags-menu (event)
  "Pop up a menu with valid tags and insert the choosen tag.
If the variable sgml-balanced-tag-edit is t, also inserts the
corresponding end tag. If sgml-leave-point-after-insert is t, the point
is left after the inserted tag(s), unless the element has som required
content.  If sgml-leave-point-after-insert is nil the point is left
after the first tag inserted."
  (interactive "*e")
  (let ((end (sgml-mouse-region)))
    (sgml-parse-to-here)
    (cond
     ((eq sgml-markup-type 'start-tag)
      (sgml-attrib-menu event))
     (end
      (sgml-tag-region (sgml-menu-ask event 'element) (point) end))
     (sgml-balanced-tag-edit
      (sgml-insert-element (sgml-menu-ask event 'element)))
     (t
      (sgml-insert-tag (sgml-menu-ask event 'tags))))))

(defun sgml-element-menu (event)
  "Pop up a menu with valid elements and insert choice.
If sgml-leave-point-after-insert is nil the point is left after the first 
tag inserted."
  (interactive "*e")
  (sgml-insert-element (sgml-menu-ask event 'element)))

(defun sgml-start-tag-menu (event)
  "Pop up a menu with valid start-tags and insert choice."
  (interactive "*e")
  (sgml-insert-tag (sgml-menu-ask event 'start-tag)))

(defun sgml-end-tag-menu (event)
  "Pop up a menu with valid end-tags and insert choice."
  (interactive "*e")
  (sgml-insert-tag (sgml-menu-ask event 'end-tag)))

(defun sgml-tag-region-menu (event)
  "Pop up a menu with valid elements and tag current region with the choice."
  (interactive "*e")
  (sgml-tag-region (sgml-menu-ask event 'element)
		   (region-beginning)
		   (region-end)))


(defun sgml-menu-ask (event type)
  (sgml-parse-to-here)
  (let (tab
	(title (capitalize (symbol-name type))))
    (cond
     (sgml-markup-type)
     ((eq type 'element)
      (setq tab
	    (mapcar (function symbol-name)
		    (sgml-current-list-of-valid-eltypes))))
     (t
      (unless (eq type 'start-tag)
	(setq tab
	      (mapcar (function sgml-end-tag-of)
		      (sgml-current-list-of-endable-eltypes))))
      (unless (eq type 'end-tag)
	(setq tab
	      (nconc tab
		     (mapcar (function sgml-start-tag-of)
			     (sgml-current-list-of-valid-eltypes)))))))
    (or tab
	(error "No valid %s at this point" type))
    (or
     (sgml-popup-menu event
		      title
		      (mapcar (function (lambda (x) (cons x x)))
			      tab))
     (signal 'quit nil))))

(defun sgml-entities-menu (event)
  (interactive "*e")
  (sgml-need-dtd)
  (let ((menu
	 (mapcar (function (lambda (x) (cons x x)))
		 (sort (sgml-map-entities (function sgml-entity-name)
					  (sgml-dtd-entities sgml-dtd-info)
					  t)
		       (function string-lessp))))
	choice)
    (unless menu
      (error "No entities defined"))
    (setq choice (sgml-popup-menu event "Entities" menu))
    (when choice
      (insert "&" choice ";"))))

(defun sgml-doctype-insert (doctype &rest vars)
  "Insert string DOCTYPE (ignored if nil) and set variables in &rest VARS.
VARS should be a list of variables and values.
For backward compatibility a singel string instead of a variable is 
assigned to sgml-default-dtd-file.
All variables are made buffer local and are also added to the
buffers local variables list."
  (when doctype
    (unless (bolp)
      (insert "\n"))
    (unless (eolp)
      (insert "\n")
      (forward-char -1))
    (sgml-insert-markup doctype))
  (while vars
    (cond ((stringp (car vars))
	   (sgml-set-local-variable 'sgml-default-dtd-file (car vars))
	   (setq vars (cdr vars)))
	  ((car vars)			; Avoid nil
	   (sgml-set-local-variable (car vars) (cadr vars))
	   (setq vars (cddr vars)))))
  (setq sgml-top-tree nil))

(defun sgml-attrib-menu (event)
  "Pop up a menu of the attributes of the current element
\(or the element whith start-tag before point)."
  (interactive "e")
  (let* ((el (sgml-find-attribute-element))
	 (attlist (sgml-non-fixed-attributes (sgml-element-attlist el)))
	 tokens menu other)
    (or attlist
	(error "No non-fixed attributes for element"))
    (loop for attdecl in attlist
	  do (setq tokens
		   (or (sgml-declared-value-token-group
			(sgml-attdecl-declared-value attdecl))
		       (sgml-declared-value-notation
			(sgml-attdecl-declared-value attdecl))))
	  (cond
	   (tokens
	    (push (cons
		   (sgml-attdecl-name attdecl)
		   (nconc
		    (loop for val in tokens collect			  
			  (cons val (cons (sgml-attdecl-name attdecl)
					  val)))
		    (if (sgml-default-value-type-p
			 'implied (sgml-attdecl-default-value attdecl))
			(list ""
			      (list "#IMPLIED" (sgml-attdecl-name attdecl))))))
		  menu))
	   (t;; No tokens
	    (push (cons (sgml-attdecl-name attdecl)
			(cons (sgml-attdecl-name attdecl) t))
		  other))))
    (setq menu (cons "Attributes"
		     (nconc menu
			    (if other
				(list (cons "Other Attributes" other))))))
    (let ((result (x-popup-menu event menu)))
      (and result
	   (sgml-insert-attribute
	    (car result)
	    (if (eq t (cdr result))
		(sgml-read-attribute-value
		 (sgml-lookup-attdecl (car result) attlist)
		 (sgml-element-attval el (car result)))
	      (cdr result)))))))

;;;; SGML mode: Fill 

(defun sgml-fill-element (element)
  "Fill bigest enclosing element with mixed content.
If current element has pure element content, recursively fill the
subelements."
  (interactive (list (sgml-find-element-of (point))))
  ;;
  (message "Filling...")
  (when (sgml-element-mixed element)
    ;; Find bigest enclosing element with mixed content
    (while (sgml-element-mixed (sgml-element-parent element))
      (setq element (sgml-element-parent element))))
  ;; 
  (sgml-do-fill element)
  (sgml-message "Done"))

(defun sgml-do-fill (element)
  (when sgml-debug
    (goto-char (sgml-element-start element))
    (sit-for 0))
  (save-excursion
    (cond
     ((sgml-element-mixed element)
      (let (last-pos
	    (c (sgml-element-content element))
	    (agenda nil))		; regions to fill later
	(goto-char (sgml-element-stag-end element))
	(when (eolp) (forward-char 1))
	(setq last-pos (point))
	(while c
	  (cond
	   ((sgml-element-mixed c))
	   (t
	    ;; Put region before element on agenda.  Can't fill it now
	    ;; that would mangel the parse tree that is beeing traversed.
	    (push (cons last-pos (sgml-element-start c))
		  agenda)
	    (goto-char (sgml-element-start c))
	    (sgml-do-fill c)
	    ;; Fill may change parse tree, get a fresh
	    (setq c (sgml-find-element-of (point)))
	    (setq last-pos (sgml-element-end c))))
	  (setq c (sgml-element-next c)))
	;; Fill the last region in content of element,
	;; but get a fresh parse tree, if it has change due to other fills.
	(sgml-fill-region last-pos
			  (sgml-element-etag-start
			   (sgml-find-element-of
			    (sgml-element-start element))))
	(while agenda
	  (sgml-fill-region (caar agenda) (cdar agenda))
	  (setq agenda (cdr agenda)))))
     (t
      ;; If element is not mixed, fill subelements recursively
      (let ((c (sgml-element-content element)))
	(while c
	  (goto-char (sgml-element-start c))
	  (sgml-do-fill c)
	  (setq c (sgml-element-next (sgml-find-element-of (point))))))))))

(defun sgml-fill-region (start end)
  (sgml-message "Filling...")
  (save-excursion
    (goto-char end)
    (skip-chars-backward " \t\n")
    (while (progn (beginning-of-line 1)
		  (< start (point)))
      (delete-horizontal-space)
      (delete-char -1)
      (insert " "))
    (end-of-line 1)
    (let (give-up prev-column opoint)
      (while (and (not give-up) (> (current-column) fill-column))
	(setq prev-column (current-column))
	(setq opoint (point))
	(move-to-column (1+ fill-column))
	(skip-chars-backward "^ \t\n")
	(if (bolp)
	    (re-search-forward "[ \t]" opoint t))
	(setq opoint (point))
	(skip-chars-backward " \t")
	(if (bolp)
	    (setq give-up t)
	(delete-region (point) opoint)
	(newline)
	(sgml-indent-line)
	(end-of-line 1)
	(setq give-up (>= (current-column) prev-column)))))))

;;;; SGML mode: Attribute editing

(defvar sgml-start-attributes nil)
(defvar sgml-main-buffer nil)
(defvar sgml-attlist nil)

(defun sgml-edit-attributes ()
  "Edit attributes of current element.
Editing is done in a separate window."
  (interactive)
  (let ((element (sgml-find-attribute-element)))
    (unless (sgml-bpos-p (sgml-element-stag-epos element))
      (error "Element's start-tag is not in the buffer"))
    (push-mark)
    (goto-char (sgml-element-start element))
    (let* ((start (point-marker))
	   (asl (sgml-element-attribute-specification-list element))
	   (cb (current-buffer))
	   (quote sgml-always-quote-attributes))
       (switch-to-buffer-other-window
	(sgml-attribute-buffer element asl))
       (sgml-edit-attrib-mode)
       (make-local-variable 'sgml-attlist)
       (setq sgml-attlist (sgml-element-attlist element))
       (make-local-variable 'sgml-start-attributes)
       (setq sgml-start-attributes start)
       (make-local-variable 'sgml-always-quote-attributes)
       (setq sgml-always-quote-attributes quote)
       (make-local-variable 'sgml-main-buffer)
       (setq sgml-main-buffer cb))))

(defun sgml-attribute-buffer (element asl)
  (let ((bname "*Edit attributes*")
	(buf nil)
	(inhibit-read-only t))
    (save-excursion
      (when (setq buf (get-buffer bname))
	(kill-buffer buf))
      (setq buf (get-buffer-create bname))
      (set-buffer buf)
      (erase-buffer)
      (sgml-insert '(read-only t rear-nonsticky (read-only))
		   "<%s  -- Edit values and finish with C-c C-c --\n"
		   (sgml-element-name element))
      (loop
       for attr in (sgml-element-attlist element) do
       ;; Produce text like
       ;;  name = value
       ;;  -- declaration : default --
       (let* ((aname (sgml-attdecl-name attr))
	      (dcl-value (sgml-attdecl-declared-value attr))
	      (def-value (sgml-attdecl-default-value attr))
	      (cur-value (sgml-lookup-attspec aname asl)))
	 (sgml-insert			; atribute name
	  '(read-only t rear-nonsticky (read-only))
	  " %s = " aname)
	 (cond				; attribute value
	  ((sgml-default-value-type-p 'fixed def-value)
	   (sgml-insert '(read-only t category sgml-fixed
				    rear-nonsticky (category))
			"#FIXED %s"
			(sgml-default-value-attval def-value)))
	  ((and (null cur-value)
		(or (memq def-value '(implied conref current))
		    (sgml-default-value-attval def-value)))
	   (sgml-insert '(category sgml-default rear-nonsticky (category))
			"#DEFAULT"))
	  ((not (null cur-value))
	   (sgml-insert nil "%s" (sgml-attspec-attval cur-value))))
	 (sgml-insert
	  '(read-only 1)
	  "\n\t-- %s: %s --\n"
	  (cond ((sgml-declared-value-token-group dcl-value))
		((sgml-declared-value-notation dcl-value)
		 (format "NOTATION %s"
			 (sgml-declared-value-notation dcl-value)))
		(t
		 dcl-value))
	  (cond ((sgml-default-value-attval def-value))
		(t
		 (concat "#" (upcase (symbol-name def-value))))))))
      (sgml-insert '(read-only t) ">")
      (goto-char (point-min))
      (sgml-edit-attrib-next))
    buf))

(defvar sgml-edit-attrib-mode-map (make-sparse-keymap))
(define-key sgml-edit-attrib-mode-map "\C-c\C-c" 'sgml-edit-attrib-finish)
(define-key sgml-edit-attrib-mode-map "\C-c\C-d" 'sgml-edit-attrib-default)
(define-key sgml-edit-attrib-mode-map "\C-c\C-k" 'sgml-edit-attrib-clear)

(define-key sgml-edit-attrib-mode-map "\C-a"  'sgml-edit-attrib-field-start)
(define-key sgml-edit-attrib-mode-map "\C-e"  'sgml-edit-attrib-field-end)
(define-key sgml-edit-attrib-mode-map "\t"  'sgml-edit-attrib-next)

(defun sgml-edit-attrib-mode ()
  "Major mode to edit attribute specification list.\\<sgml-edit-attrib-mode-map>
Use \\[sgml-edit-attrib-next] to move between input fields.  Use
\\[sgml-edit-attrib-default] to make an attribute have its default
value.  To abort edit kill buffer (\\[kill-buffer]) and remove window
(\\[delete-window]).  To finsh edit use \\[sgml-edit-attrib-finish].

\\{sgml-edit-attrib-mode-map}"
  (kill-all-local-variables)
  (setq mode-name "SGML edit attributes"
	major-mode 'sgml-edit-attrib-mode)
  (use-local-map sgml-edit-attrib-mode-map)
  (run-hooks 'text-mode-hook 'sgml-edit-attrib-mode-hook))

(defun sgml-edit-attrib-finish ()
  "Finish editing and insert attribute values in original buffer."
  (interactive)
  (let ((cb (current-buffer))
	(asl (sgml-edit-attrib-specification-list))
	;; save buffer local variables
	(start sgml-start-attributes))
    (when (markerp start)
      (delete-windows-on cb)
      (switch-to-buffer (marker-buffer start))
      (kill-buffer cb)
      (goto-char start)
      (let ((element (sgml-find-element-of start)))
	;; *** Should the it be verified that this element
	;; is the one edited?
	(sgml-change-start-tag element asl)))))


(defun sgml-edit-attrib-specification-list ()
  (goto-char (point-min))
  (forward-line 1)
  (sgml-with-parser-syntax
   (let ((asl nil)
	 (al sgml-attlist))
     (while (not (eq ?> (following-char)))
       (sgml-parse-s)
       (let ((name (sgml-check-nametoken)))
	 (forward-char 3)
	 (unless (memq (get-text-property (point) 'category)
		       '(sgml-default sgml-fixed))
	   (push
	    (sgml-make-attspec name
			       (sgml-extract-attribute-value
				(sgml-attdecl-declared-value (car al))))
	    asl))
	 (while (progn (beginning-of-line 2)
		       (or (eolp)
			   (not (get-text-property (point) 'read-only))))))
					; was (eq t)
       (forward-line 1)
       (setq al (cdr al)))
     asl)))


(defun sgml-extract-attribute-value (type)
  (save-excursion
    (save-restriction
      (narrow-to-region (point)
			(progn (sgml-edit-attrib-field-end)
			       (point)))
      (unless (eq type 'cdata)
	(subst-char-in-region (point-min) (point-max) ?\n ? )
	(goto-char (point-min))
	(delete-horizontal-space))
      (goto-char (point-min))
      (when (search-forward "\"" nil t)	; don't allow both " and '
	(goto-char (point-min))
	(while (search-forward "'" nil t) ; replace ' with char ref
	  (replace-match "&#39;")))
      (buffer-string))))

(defun sgml-edit-attrib-default ()
  "Set current attribute value to default."
  (interactive)
  (sgml-edit-attrib-clear)
  (save-excursion
    (sgml-insert '(category sgml-default)
		 "#DEFAULT")))

(defun sgml-edit-attrib-clear ()
  "Kill the value of current attribute."
  (interactive)
  (kill-region
   (progn (sgml-edit-attrib-field-start) (point))
   (progn (sgml-edit-attrib-field-end) (point))))

(defun sgml-edit-attrib-field-start ()
  "Go to the start of the attribute value field."
  (interactive)
  (let (start)
        (beginning-of-line 1)
    (while (not (eq t (get-text-property (point) 'read-only)))
      (beginning-of-line 0))
    (setq start (next-single-property-change (point) 'read-only))
    (unless start (error "No attribute value here"))
    (assert (number-or-marker-p start))
    (goto-char start)))

(defun sgml-edit-attrib-field-end ()
  "Go to the end of the attribute value field."
  (interactive)
  (sgml-edit-attrib-field-start)
  (let ((end (if (and (eolp)
		      (get-text-property (1+ (point)) 'read-only))
		 (point)
	       (next-single-property-change (point) 'read-only))))
    (assert (number-or-marker-p end))
    (goto-char end)))

(defun sgml-edit-attrib-next ()
  "Move to next attribute value."
  (interactive)
  (or (search-forward-regexp "^ *[.A-Za-z0-9---]+ *= ?" nil t)
      (goto-char (point-min))))


;;;; SGML mode: Hiding tags/attributes

(defconst sgml-tag-regexp
  "\\(</?>\\|</?[A-Za-z][---A-Za-z0-9.]*\\(\\([^'\"></]\\|'[^']*'\\|\"[^\"]*\"\\)*\\)>?\\)")

(defun sgml-operate-on-tags (action &optional attr-p)
  (let ((buffer-modified-p (buffer-modified-p))
	(inhibit-read-only t)
	(buffer-read-only nil)
	(before-change-function nil)
	(markup-index			; match-data index in tag regexp
	 (if attr-p 2 1))
	(tagcount			; number tags to give them uniq
					; invisible properties
	 1))
    (unwind-protect
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward sgml-tag-regexp nil t)
	    (cond
	     ((eq action 'hide)
	      (let ((tag (downcase
			  (buffer-substring (1+ (match-beginning 0))
					    (match-beginning 1)))))
		(if (or attr-p (not (member tag sgml-exposed-tags)))
		    (add-text-properties
		     (match-beginning markup-index) (match-end markup-index)
		     (list 'invisible tagcount
			   'rear-nonsticky '(invisible face))))))
	     ((eq action 'show)		; ignore markup-index
	      (remove-text-properties (match-beginning 0) (match-end 0)
				      '(invisible nil)))
	     (t (error "Invalid action: %s" action)))
	    (incf tagcount)))
      (set-buffer-modified-p buffer-modified-p))))

(defun sgml-hide-tags ()
  "Hide all tags in buffer."
  (interactive)
  (sgml-operate-on-tags 'hide))

(defun sgml-show-tags ()
  "Show hidden tags in buffer."
  (interactive)
  (sgml-operate-on-tags 'show))

(defun sgml-hide-attributes ()
  "Hide all attribute specifications in the buffer."
  (interactive)
  (sgml-operate-on-tags 'hide 'attributes))

(defun sgml-show-attributes ()
  "Show all attribute specifications in the buffer."
  (interactive)
  (sgml-operate-on-tags 'show 'attributes))


;;;; SGML mode: Normalize (and misc manipulations)

(defun sgml-expand-shortref-to-text (name)
  (let (before-change-function
	(entity (sgml-lookup-entity name (sgml-dtd-entities sgml-dtd-info))))
    (cond
     ((null entity) (sgml-error "Undefined entity %s" name))
     ((sgml-entity-data-p entity)
      (sgml-expand-shortref-to-entity name))
     (t
      (delete-region sgml-markup-start (point))
      (sgml-entity-insert-text entity)
      (setq sgml-goal (point-max))	; May have changed size of buffer
      ;; now parse the entity text
      (goto-char (setq sgml-last-start-pos sgml-markup-start))))))

(defun sgml-expand-shortref-to-entity (name)
  (let ((end (point))
	(re-found nil)
	before-change-function)
    (goto-char sgml-markup-start)
    (setq re-found (search-forward "\n" end t))
    (delete-region sgml-markup-start end)	   
    (insert "&" name (if re-found "\n" ";"))
    (setq sgml-goal (point-max))	; May have changed size of buffer
    (goto-char (setq sgml-last-start-pos sgml-markup-start))))

(defun sgml-expand-all-shortrefs (to-entity)
  "Expand all short references in the buffer.
Short references to text entities are expanded to the replacement text
of the entity other short references are expanded into general entity
references.  If argument, TO-ENTITY, is non-nil, or if called
interactive with numeric prefix argument, all short references are
replaced by generaly entity references."
  (interactive "*P")
  (sgml-reparse-buffer
   (if to-entity
       (function sgml-expand-shortref-to-entity)
     (function sgml-expand-shortref-to-text))))

(defun sgml-normalize (to-entity &optional element)
  "Normalize buffer by filling in omitted tags and expanding empty tags.
Argument TO-ENTITY controls how short references are expanded as with
`sgml-expand-all-shortrefs'.  An optional argument ELEMENT can be the
element to normalize insted of the whole buffer, if used no short
references will be expanded."
  (interactive "*P")
  (unless element
    (sgml-expand-all-shortrefs to-entity))
  (let ((only-one (not (null element))))
    (setq element (or element (sgml-top-element)))
    (goto-char (sgml-element-end element)) 
    (let ((before-change-function nil))
      (sgml-normalize-content element only-one)))
  (sgml-note-change-at (sgml-element-start element))
  (sgml-message "Done"))

(defun sgml-normalize-element ()
  (interactive "*")
  (sgml-normalize nil (sgml-find-element-of (point))))

(defun sgml-normalize-content (element only-first)
  "Normalize all elements in a content where ELEMENT is first element.
If sgml-normalize-trims is non-nil, trim off white space from ends of
elements with omitted end-tags."
  (let ((content nil))
    (while element			; Build list of content elements
      (push element content)
      (setq element (if only-first
			nil
		      (sgml-element-next element))))
    (while content
      (setq element (car content))
      ;; Progress report
      (sgml-lazy-message "Normalizing %d%% left"
			 (/ (point) (/ (point-max) 100)))
      ;; Fix the end-tag
      (sgml-normalize-end-tag element)
      ;; Fix tags of content
      (sgml-normalize-content (sgml-tree-content element) nil)
      ;; Fix the start-tag
      (sgml-normalize-start-tag element)
      ;; Next content element
      (setq content (cdr content)))))

(defun sgml-normalize-start-tag (element)
  (when (sgml-bpos-p (sgml-element-stag-epos element))
    (goto-char (min (point) (sgml-element-start element)))
    (let ((name (sgml-element-gi element))
	  (attlist (sgml-element-attlist element))
	  (asl (sgml-element-attribute-specification-list element)))
      (save-excursion
	(assert (or (zerop (sgml-element-stag-len element))
		    (= (point) (sgml-element-start element))))
	(delete-char (sgml-element-stag-len element))
	(sgml-insert-start-tag name asl attlist nil)))))

(defun sgml-normalize-end-tag (element)
  (unless (sgml-element-empty element)
    (when (sgml-bpos-p (sgml-element-etag-epos element))
      (goto-char (min (point) (sgml-element-etag-start element)))    
      (if (and (zerop (sgml-element-etag-len element))
	       sgml-normalize-trims)
	  (skip-chars-backward " \t\n\r"))
      (delete-char (sgml-tree-etag-len element))
      (save-excursion (insert (sgml-end-tag-of element))))))


(defun sgml-make-character-reference (&optional invert)
  "Convert character after point into a character reference.
If called with a numeric argument, convert a character reference back
to a normal character.  If called from a program, set optional
argument INVERT to non-nil."
  (interactive "*P")
  (cond
   (invert
    (or (looking-at "&#\\([0-9]+\\)[;\n]?")
	(error "No character reference after point"))
    (let ((c (string-to-int (buffer-substring (match-beginning 1)
					      (match-end 1)))))
      (delete-region (match-beginning 0)
		     (match-end 0))
      (insert c)))
   ;; Convert character to &#nn;
   (t
    (let ((c (following-char)))
      (delete-char 1)
      (insert (format "&#%d;" c))))))

(defun sgml-expand-entity-reference ()
  "Insert the text of the entity referenced at point."
  (interactive)
  (sgml-with-parser-syntax
   (setq sgml-markup-start (point))
   (sgml-check-delim "ERO")
   (let* ((ename (sgml-check-name t))
	  (entity (sgml-lookup-entity ename
				      (sgml-dtd-entities
				       (sgml-pstate-dtd
					sgml-buffer-parse-state)))))
     (unless entity
       (error "Undefined entity %s" ename))
     (or (sgml-parse-delim "REFC")
	 (sgml-parse-RE))
     (delete-region sgml-markup-start (point))
     (sgml-entity-insert-text entity))))


;;;; SGML mode: TAB completion

(defun sgml-complete ()
  "Complete the word/tag/entity before point.
If it is a tag (starts with < or </) complete with valid tags.
If it is an entity (starts with &) complete with declared entities.
If it is a markup declaration (starts with <!) complete with markup 
declaration names.
If it is something else complete with ispell-complete-word."
  (interactive "*")
  (let ((tab				; The completion table
	 nil)
	(pattern nil)
	(c nil)
	(here (point)))
    (skip-chars-backward "^ \n\t</!&%")
    (setq pattern (buffer-substring (point) here))
    (setq c (char-after (1- (point))))
    (cond
     ;; entitiy
     ((eq c ?&)
      (sgml-need-dtd)
      (setq tab
	    (sgml-entity-completion-table
	     (sgml-dtd-entities (sgml-pstate-dtd sgml-buffer-parse-state)))))
     ;; start-tag
     ((eq c ?<)
      (save-excursion
	(backward-char 1)
	(sgml-parse-to-here)
	(setq tab (sgml-eltype-completion-table
		   (sgml-current-list-of-valid-eltypes)))))
     ;; end-tag
     ((eq c ?/)
      (save-excursion
	(backward-char 2)
	(sgml-parse-to-here)
	(setq tab (sgml-eltype-completion-table
		   (sgml-current-list-of-endable-eltypes)))))
     ;; markup declaration
     ((eq c ?!)
      (setq tab sgml-markup-declaration-table))
     (t
      (goto-char here)
      (ispell-complete-word)))
    (when tab
      (let ((completion (try-completion pattern tab)))
	(cond ((null completion)
	       (goto-char here)
	       (message "Can't find completion for \"%s\"" pattern)
	       (ding))
	      ((eq completion t)
	       (goto-char here)
	       (message "[Complete]"))
	      ((not (string= pattern completion))
	       (delete-char (length pattern))
	       (insert completion))
	      (t
	       (goto-char here)
	       (message "Making completion list...")
	       (let ((list (all-completions pattern tab)))
		 (with-output-to-temp-buffer " *Completions*"
		   (display-completion-list list)))
	       (message "Making completion list...%s" "done")))))))


;;;; SGML mode: Options menu

(defun sgml-file-options-menu (&optional event)
  (interactive "e")
  (sgml-options-menu event sgml-file-options))

(defun sgml-user-options-menu (&optional event)
  (interactive "e")
  (sgml-options-menu event sgml-user-options))

(defun sgml-options-menu (event vars)
  (let ((var
	 (let ((maxlen 
		(loop for var in vars
		      maximize (length (sgml-variable-description var)))))
	   (sgml-popup-menu
	    event "Options"
	    (loop for var in vars
		  for desc = (sgml-variable-description var)
		  collect
		  (cons
		   (format "%s%s [%s]"
			   desc
			   (make-string (- maxlen (length desc)) ? )
			   (sgml-option-value-indicator var))
		   var))))))
    (when var
      (sgml-do-set-option var event))))

(defun sgml-do-set-option (var &optional event)
  (let ((type (sgml-variable-type var))
	(val (symbol-value var)))
    (cond
     ((eq 'toggle type)
      (message "%s set to %s" var (not val))
      (set var (not val)))
     ((eq 'string type)
      (describe-variable var)
      (setq val (read-string (concat (sgml-variable-description var) ": ")))
      (when (stringp val)
	(set var val)))
     ((consp type)
      (let ((val
	     (sgml-popup-menu event
			      (sgml-variable-description var)
			      (loop for c in type collect
				    (cons
				     (if (consp c) (car c) (format "%s" c))
				     (if (consp c) (cdr c) c))))))
	(set var val)
	(message "%s set to %s" var val)))
     (t
      (describe-variable var)
      (setq val (read-string (concat (sgml-variable-description var)
				     " (sexp): ")))
      (when (stringp val)
	(set var (car (read-from-string val)))))))
  (force-mode-line-update))

(defun sgml-option-value-indicator (var)
  (let ((type (sgml-variable-type var))
	(val (symbol-value var)))
    (cond
     ((eq type 'toggle)
      (if val "Yes" "No"))
     ((eq type 'string)
      (if (stringp val)
	  (substring val 0 4)
	"-"))
     ((and (atom type) val)
      "...")
     ((consp type)
      (or (car (rassq val type))
	  val))
     (t
      "-"))))




;;; psgml-edit.el ends here
