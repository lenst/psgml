;;;; psgml-dtd.el --- DTD parser for SGML-editing mode with parsing support
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

(require 'psgml)
(require 'psgml-parse)


;;;; Constructing basic

(defun sgml-copy-moves (s1 s2)
  "Copy all moves from S1 to S2, keeping their status."
  (let ((l (sgml-state-opts s1)))
    (while l
      (sgml-add-opt-move s2
			 (sgml-move-token (car l))
			 (sgml-move-dest (car l)))
      (setq l (cdr l)))
    (setq l (sgml-state-reqs s1))
    (while l
      (sgml-add-req-move s2
			 (sgml-move-token (car l))
			 (sgml-move-dest (car l)))
      (setq l (cdr l)))))

(defun sgml-copy-moves-to-opt (s1 s2)
  "Copy all moves from S1 to S2 as optional moves."
  (let ((l (sgml-state-opts s1)))
    (while l
      (sgml-add-opt-move s2
			 (sgml-move-token (car l))
			 (sgml-move-dest (car l)))
      (setq l (cdr l)))
    (setq l (sgml-state-reqs s1))
    (while l
      (sgml-add-opt-move s2
			 (sgml-move-token (car l))
			 (sgml-move-dest (car l)))
      (setq l (cdr l)))))


(defun sgml-some-states-of (state)
  ;; List of some states reachable from STATE, includes all final states
  (let* ((states (list state))
	 (l states)
	 s ms m)
    (while l
      (setq s (car l)
	    ms (append (sgml-state-opts s) (sgml-state-reqs s)))
      (while ms
	(setq m (sgml-move-dest (car ms))
	      ms (cdr ms))
	(unless (sgml-normal-state-p m)
	  (setq m (sgml-&node-next m)))
	(unless (memq m states)
	  (nconc states (list m))))
      (setq l (cdr l)))
    states))

(defmacro sgml-for-all-final-states (s dfa &rest forms)
  "For all final states S in DFA do FORMS.
Syntax: var dfa-expr &body forms"
  (` (let ((L-states (sgml-some-states-of (, dfa)))
	   (, s))
       (while L-states
	 (when (sgml-state-final-p (setq (, s) (car L-states)))
	   (,@ forms))
	 (setq L-states (cdr L-states))))))

(put 'sgml-for-all-final-states 'lisp-indent-hook 2)
(put 'sgml-for-all-final-states 'edebug-form-hook '(symbolp &rest form))


;;;; Optimization for the dfa building

(defsubst sgml-empty-state-p (s)
  ;; True if S hase no outgoing moves
  (and (sgml-normal-state-p s)
       (null (sgml-state-reqs s))
       (null (sgml-state-opts s)))  )

(defun sgml-one-final-state (s)
  ;; Collaps all states that have no moves
  ;; This is a safe optimization, useful for (..|..|..)
  (sgml-debug "OPT one final: reqs %d opts %d"
	      (length (sgml-state-reqs s))
	      (length (sgml-state-opts s)))
  (let ((final nil)
	dest)
    (loop for m in (append (sgml-state-reqs s)
			   (sgml-state-opts s))
	  do
	  (setq dest (sgml-move-dest m))
	  (when (sgml-empty-state-p dest)
	    (cond ((null final)
		   (setq final dest))
		  (t
		   (setf (sgml-move-dest m) final)))))))

(defun sgml-states-equal (s1 s2)
  (and (= (length (sgml-state-opts s1))
	  (length (sgml-state-opts s2)))
       (= (length (sgml-state-reqs s1))
	  (length (sgml-state-reqs s2)))
       (loop for m in (sgml-state-opts s1)
	     always
	     (eq (sgml-move-dest m)
		 (sgml-move-dest (sgml-moves-lookup (sgml-move-token m)
						    (sgml-state-opts s2)))))
       (loop for m in (sgml-state-reqs s1)
	     always
	     (eq (sgml-move-dest m)
		 (sgml-move-dest (sgml-moves-lookup (sgml-move-token m)
						    (sgml-state-reqs s2)))))))

(defun sgml-other-start-state (s)
  ;; Look for another state accesible from S that is
  ;; equivalent to S.  Using this state insted may save a state.
  ;; This optimization should be safe, and appropriate for * and +.
  (sgml-debug "OPT other start: reqs %d opts %d"
	      (length (sgml-state-reqs s))
	      (length (sgml-state-opts s)))
  (let ((considered nil)
	(l (append (sgml-state-reqs s)
		   (sgml-state-opts s)))
	(res s)
	dest)
    (while l
      (cond
       ((and (sgml-normal-state-p (setq dest (sgml-move-dest (car l))))
	     (if (memq dest considered)
		 nil
	       (push dest considered))
	     (sgml-states-equal s dest))
	(sgml-debug "OPT other start: sucess")
	(setq res dest
	      l nil))
       (t (setq l (cdr l)))))
    res))


;;;; Constructing

(defun sgml-make-primitive-content-token (token)
  (let ((s1 (sgml-make-state))
	(s2 (sgml-make-state)))
    (sgml-add-req-move s1 token s2)
    s1))

(defun sgml-make-opt (s1)
  (setf (sgml-state-opts s1)
	(append (sgml-state-opts s1)
		(sgml-state-reqs s1)))
  (setf (sgml-state-reqs s1) nil)
  s1)

(defun sgml-make-* (s1)
  (setq s1 (sgml-make-+ s1))
  (cond ((sgml-state-reqs s1)
	 (sgml-other-start-state	; optimize
	  (sgml-make-opt s1)))		; make-opt needed thoug
	(t
	 s1)))

(defun sgml-make-+ (s1)
  (sgml-for-all-final-states s s1
    (sgml-copy-moves-to-opt s1 s))
  (sgml-other-start-state s1))		; optimize

(defun sgml-make-conc (s1 s2)
  (let ((moves (append (sgml-state-reqs s1) (sgml-state-opts s1))))
    (cond
     (;; optimize the case where all moves from s1 goes to empty states
      (loop for m in moves
	    always (sgml-empty-state-p (sgml-move-dest m)))
      (loop for m in moves do (setf (sgml-move-dest m) s2))
      (when (sgml-state-final-p s1)
	(sgml-copy-moves s2 s1)))
     (t					; general case
      (sgml-for-all-final-states s s1
	(sgml-copy-moves s2 s)))))
  s1)

(defun sgml-make-alt (s1 s2)
  (cond ((or (sgml-state-final-p s1)	; is result optional
	     (sgml-state-final-p s2))
	 (sgml-make-opt s1)
	 (sgml-copy-moves-to-opt s2 s1))
	(t
	 (sgml-copy-moves s2 s1)))
  s1)

(defun sgml-make-pcdata ()
  (sgml-make-opt (sgml-make-primitive-content-token sgml-pcdata-token)))

(defun sgml-reduce-, (l)
  (while (cdr l)
    (setcar (cdr l)
	    (sgml-make-conc (car l) (cadr l)))
    (setq l (cdr l)))
  (car l))

(defun sgml-reduce-| (l)
  (while (cdr l)			; apply the binary make-alt
    (setcar (cdr l)			; reducing the list l
	    (sgml-make-alt (car l) (cadr l)))
    (setq l (cdr l)))
  (sgml-one-final-state (car l))	; optimization
  (car l))

(defun sgml-make-& (dfas)
  (let ((&n (sgml-make-&node dfas (sgml-make-state)))
	(s (sgml-make-state))
	(l dfas))
    (while l				; For each si:
      ;; For m in opts(si): add optional move from s to &n on token(m).
      (loop for m in (sgml-state-opts (car l))
	    do (sgml-add-opt-move s (sgml-move-token m) &n))
      ;; For m in reqs(si): add required move from s to &n on token(m).
      (loop for m in (sgml-state-reqs (car l))
	    do (sgml-add-req-move s (sgml-move-token m) &n))
      (setq l (cdr l)))
    ;; Return s.
    s))



;(sgml-make-conc (sgml-make-primitive-content-token 'para) (sgml-make-primitive-content-token 'list))
;(sgml-make-conc (sgml-make-& (list (sgml-make-primitive-content-token 'para) (sgml-make-primitive-content-token 'list))) (sgml-make-primitive-content-token 'foo))

;(setq x  (sgml-some-states-of  (sgml-make-primitive-content-token 'para))) 
;(sgml-state-final-p (car x) ) 
;(sgml-state-final-p (cadr x)) 


;;;; Parse doctype: General

(defun sgml-parse-connector ()
  (sgml-skip-ps)
  (cond ((sgml-parse-char ?,)
	 (function sgml-reduce-,))
	((sgml-parse-char ?|)
	 (function sgml-reduce-|))
	((sgml-parse-char ?&)
	 (function sgml-make-&))))

(defun sgml-parse-name-group ()
  (sgml-skip-ps)
  (let (names)
    (cond
     ((sgml-parse-char ?\()
      (sgml-skip-ps)
      (setq names (list (sgml-check-name)))
      (while (sgml-parse-connector)
	(sgml-skip-ps)
	(push (sgml-check-name) names))
      (sgml-check-char ?\))
      names)
     (t
      (list (sgml-check-name))))))

(defun sgml-parse-nametoken-group ()
  (sgml-skip-ps)
  (let (names)
    (cond
     ((sgml-parse-char ?\()
      (setq names (list (sgml-check-nametoken)))
      (while (sgml-parse-connector)
	(push (sgml-check-nametoken)
	      names))
      (sgml-check-char ?\))
      names)
     (t
      (list (sgml-check-nametoken))))))

(defun sgml-parse-notation-dcl ()
  ;;148  notation declaration = MDO, "NOTATION",
  ;;                        65 ps+, 41 notation name,
  ;;                        65 ps+, 149 notation identifier,
  ;;                        65 ps*, MDC
  ;;41   notation name    = 55 name
  ;;149  notation identifier = 73 external identifier
  (sgml-skip-ps)
  (sgml-check-name)
  (sgml-skip-ps)
  (sgml-parse-external)
  (sgml-skip-ps)
  (sgml-check-mdc))


;;;; Parse doctype: Element

(defun sgml-parse-opt ()
  (sgml-skip-ps)
  (cond ((or (sgml-parse-char ?o)
	     (sgml-parse-char ?O))
	 t)
	((sgml-parse-char ?-)
	 nil)))

(defun sgml-parse-modifier ()
  (cond ((sgml-parse-char ?+)
	 (function sgml-make-+))
	((sgml-parse-char ?*)
	 (function sgml-make-*))
	((sgml-parse-char ??)
	 (function sgml-make-opt))))

(defun sgml-parse-model-group ()
  (sgml-skip-ps)
  (let (el mod)
    (cond
     ((sgml-parse-char ?\()
      (let ((subs (list (sgml-parse-model-group)))
	    (con1 nil)
	    (con2 nil))
	(while (setq con2 (sgml-parse-connector))
	  (cond ((and con1
		      (not (eq con1 con2)))
		 (sgml-error "Mixed connectors")))
	  (setq con1 con2)
	  (setq subs (nconc subs (list (sgml-parse-model-group)))))
	(sgml-check-char ?\))
	(setq el (if con1
		     (funcall con1 subs)
		   (car subs)))))
     ((sgml-parse-rni 'pcdata)
      (setq sgml-used-pcdata t)
      (setq el (sgml-make-pcdata)))
     (t
      (setq el (sgml-make-primitive-content-token (sgml-check-name)))))
    (setq mod (sgml-parse-modifier))
    (if mod
	(funcall mod el)
      el)))

(defun sgml-parse-content-model ()
  (sgml-parse-model-group))

(defun sgml-parse-content ()
  (sgml-skip-ps)
  (cond ((eq (following-char) ?\()
	 (sgml-parse-content-model))
	(t
	 (let ((dc (sgml-check-name)))	;CDATA or RCDATA or EMPTY
	   (intern (upcase (symbol-name dc)))))))

(defun sgml-parse-exeption (type)
  (sgml-skip-ps)
  (if (sgml-parse-char type)
      (sgml-parse-name-group)))

(defun sgml-parse-element-dcl ()
  (sgml-skip-ps)
  (let* ((names (sgml-parse-name-group))
	 (stag-opt (sgml-parse-opt))
	 (etag-opt (sgml-parse-opt))
	 (sgml-used-pcdata nil)
	 (model (sgml-parse-content))
	 (exclusions (sgml-parse-exeption ?-))
	 (inclusions (sgml-parse-exeption ?+)))
    (sgml-skip-ps)
    (sgml-check-mdc)
    (while names
      (sgml-debug "Defining element %s" (car names))
      (sgml-define-element (car names) stag-opt etag-opt model
			 exclusions inclusions
			 sgml-used-pcdata)
      (setq names (cdr names)))))



;;;; Parse doctype: Entity

(defun sgml-parse-entity-dcl ()
  (sgml-skip-ps)
  (cond
   ((sgml-parse-char ?%)		; parameter entity declaration
    (sgml-set-param-entity (sgml-check-ename) (sgml-parse-entity-text)))
   (t					; normal entity declaration
    (cond ((sgml-parse-rni 'default))
	  (t				; general entity
	   (push (symbol-name (sgml-check-ename))
		 sgml-entities)))
    (sgml-parse-entity-text)))
  (sgml-skip-ps)
  (sgml-check-char ?>))


(defun sgml-parse-entity-text ()
  ;;105  entity text  = 66 parameter literal
  ;;                 | 106 data text
  ;;                 | 107 bracketed text
  ;;                 | 108 external entity specification
  (sgml-skip-ps)
  (let ((external-id (sgml-parse-external)))
    (cond
     (external-id			; external entity specification =
					; 73 external identifier,
					; (65 ps+, 109+ entity type)?
      (sgml-skip-ps)
      (sgml-parse-entity-type)
      external-id)
     ((sgml-startnm-char-next)
      (let ((token (sgml-check-name)))
	(sgml-skip-ps)
	(cond
	 ((memq token '(cdata sdata pi)) ; data text ***
	  (sgml-parse-parameter-literal))
	 ((eq token 'starttag)
	  (sgml-start-tag-of (sgml-parse-parameter-literal)))
	 ((eq token 'endtag)
	  (sgml-end-tag-of (sgml-parse-parameter-literal)))	
	 ((eq token 'ms)		; marked section ***
	  (sgml-parse-parameter-literal))
	 ((eq token 'md)		; Markup declaration
	  (concat "<!" (sgml-parse-parameter-literal) ">")))))
     ((sgml-parse-parameter-literal)))))

(defun sgml-parse-entity-type ()
  ;;109+ entity type      = "SUBDOC"
  ;;                      | (("CDATA" | "NDATA" | "SDATA"),
  ;;                             65 ps+,
  ;;                             41 notation name,
  ;;                             149.2+ data attribute specification?)
  (when (sgml-startnm-char-next)
    (let ((type (sgml-check-name)))
      (cond ((eq type 'subdoc))
	    ((memq type '(cdata ndata sdata))
	     (sgml-skip-ps)
	     (sgml-check-name)
	     ;;149.2+ data attribute specification
	     ;;                      = 65 ps+, DSO,
	     ;;                        31 attribute specification list,
	     ;;                        5 s*, DSC
	     (sgml-skip-ps)
	     (when (sgml-parse-char ?\[)
	       (sgml-parse-attribute-specification-list)
	       (sgml-skip-s)
	       (sgml-check-char ?\])))
	    (t (sgml-error "Illegal entity type: %s" type))))))


(defun sgml-parse-parameter-literal ()
  (sgml-skip-ps)
  (let ((qchar (following-char))
	qregexp
	value)
    (cond ((memq qchar '(?\" ?\'))
	   (forward-char 1)
	   (setq qregexp (format "^%c%%" qchar))
	   (setq value "")
	   (while (not (sgml-parse-char qchar))
	     (setq value (concat value
				 (buffer-substring
				  (point)
				  (goto-char
				   (progn (skip-chars-forward qregexp)
					  (point))))))
	     (cond ((sgml-parse-char ?%)	;parameter entity reference
		    (if (sgml-startnm-char-next)
			(sgml-push-to-param (sgml-check-entity-ref))
		      (setq value (concat value "%"))))
		   ((eobp)
		    (or (sgml-pop-param)
			(sgml-error "Parameter literal unterminated")))))
	   value))))


;;;; Parse doctype: Attlist

(defun sgml-parse-attlist ()
  (sgml-skip-ps)
  (let ((assnot (sgml-parse-rni 'notation))
	(assel (sgml-parse-name-group))
	(attlist nil)			; the list
	(attdef nil)
	)	
    (while (setq attdef (sgml-parse-attribute-definition))
      (push attdef attlist))
    (sgml-check-char ?>)
    (setq attlist (nreverse attlist))
    (unless assnot
      (while assel
	(sgml-define-element-attlist (car assel) attlist)
	(setq assel (cdr assel))))))

(defun sgml-parse-attribute-definition ()
  (sgml-skip-ps)
  (if (eq (following-char) ?>)		; End of attlist?
      nil
    (sgml-make-attribute (sgml-check-name)
			 (sgml-parse-declared-value)
			 (sgml-parse-default-value))))

(defun sgml-parse-declared-value ()
  (sgml-skip-ps)
  (cond ((eq (following-char) ?\()	; A name token group
	 (list 'name-token-group
	       (sgml-parse-nametoken-group)))
	(t
	 (let ((key (sgml-check-name)))	; a key word
	   (if (eq key 'notation)
	       (list key (sgml-parse-name-group))
	     key)))))

(defun sgml-parse-default-value ()
  (sgml-skip-ps)
  (let* ((rni (sgml-parse-rni))
	 (key (if rni (sgml-check-name))))
    (if (or (not rni) (eq key 'fixed))
	(list key (sgml-parse-attribute-value-specification))
      key)))

;;;; Parse doctype

(defun sgml-parse-dtd-subset ()
  (let ((elcnt ""))
    (message "Parsing doctype%s" elcnt)
    (while 
	(cond
	 ((sgml-parse-ds))
	 ((sgml-parse-marked-section-start) ; marked section start
	  (sgml-parse-marked-section)
	  t)
	 ((sgml-parse-mdo)
	  (let ((token (sgml-check-name)))
	    (cond
	     ((eq token 'entity)
	      (sgml-parse-entity-dcl)
	      t)
	     ((eq token 'element)
	      (sgml-parse-element-dcl)
	      (setq elcnt (concat elcnt "."))
	      (when (> (length elcnt) 60) (setq elcnt ""))
	      (message "Parsing doctype%s" elcnt)
	      t)
	     ((eq token 'attlist)
	      (sgml-parse-attlist)
	      t)
	     ((eq token 'notation)
	      (sgml-parse-notation-dcl)
	      t)
	     (t
	      (sgml-log-warning "Ignoring markup declaration %s" token)
	      (sgml-skip-markup-declaration)
	      t))))
	 ((sgml-parse-marked-section-end)) ; end of marked section
	 ))
    (when (sgml-any-open-param/file)
      (sgml-error "DTD subset ended"))))

(defun sgml-parse-doctype ()
  (sgml-skip-ps)
  (let ((docname (sgml-check-name))
	(external (sgml-parse-external)))
    (sgml-skip-ps)
    (cond
     ((sgml-parse-char ?\[)
      (sgml-parse-dtd-subset)
      (sgml-check-char ?\])))
    (cond (external
	   (sgml-push-to-file (sgml-external-file external))
	   (sgml-parse-dtd-subset)))
    (sgml-skip-ps)
    (sgml-check-char ?>)
    (sgml-make-primitive-content-token docname)))

;;;; Parse prolog

(defun sgml-parse-prolog ()
  "Parse the document prolog to learn the DTD."
  (interactive)
  (sgml-clear-log)
  (message "Parsing prolog...")
  (setq	sgml-element-map nil		; Remove old element dcl
	sgml-doctype-state nil
	sgml-param-entities nil
	sgml-entities nil)
  (goto-char (point-min))
  (sgml-with-parser-syntax
   (sgml-skip-ds)   
   (sgml-skip-sgml-dcl)
   (sgml-skip-ds)
   (cond ((sgml-parse-mdo)
	  (cond ((eq 'doctype (sgml-parse-nametoken))
		 (sgml-set-doctype (sgml-parse-doctype))
		 (setq sgml-buffer-element-map sgml-element-map
		       sgml-buffer-param-entities sgml-param-entities
		       sgml-buffer-entities (sort sgml-entities
						  (function string-lessp))
		       ))))))
  (sgml-message "Parsing prolog...done"))

;;;; Save DTD: compute translation

(defvar sgml-translate-table nil)

(defun sgml-translate-node (node)
  (assert (not (numberp node)))
  (let ((tp (assq node sgml-translate-table)))
    (unless tp
      (setq tp (cons node (length sgml-translate-table)))
      (nconc sgml-translate-table (list tp)))
    (cdr tp)))

(defun sgml-translate-moves (moves)
  (while moves
    (sgml-translate-node (sgml-move-dest (car moves)))
    (setq moves (cdr moves))))

(defun sgml-translate-model (model)
  (let* ((sgml-translate-table (list (cons model 0)))
	 (p sgml-translate-table))
    (while p
      (cond ((sgml-normal-state-p (caar p))
	     (sgml-translate-moves (sgml-state-opts (caar p)))
	     (sgml-translate-moves (sgml-state-reqs (caar p))))
	    (t
	     (sgml-translate-node (sgml-&node-next (caar p)))))
      (setq p (cdr p)))
    sgml-translate-table))

;;;; Save DTD: binary coding

(defvar sgml-code-token-numbers nil)
(defvar sgml-code-xlate nil)

(defsubst sgml-code-xlate (node)
  ;;(let ((x (cdr (assq node sgml-code-xlate)))) (assert x) x)
  (cdr (assq node sgml-code-xlate)))

(defun sgml-code-number (num)
  (if (> num sgml-max-single-octet-number)
      (insert (+ (lsh (- num sgml-max-single-octet-number) -8)
		 sgml-max-single-octet-number 1)
	      (logand (- num sgml-max-single-octet-number) 255))
    (insert num)))

(defsubst sgml-code-token (token)
  (let ((bp (assq token sgml-code-token-numbers)))
    (unless bp
      (setq sgml-code-token-numbers
	    (nconc sgml-code-token-numbers
		   (list (setq bp (cons token
					(length sgml-code-token-numbers)))))))
    (sgml-code-number (cdr bp))))

(defmacro sgml-code-sequence (loop-c &rest body)
  "Produce the binary coding of a counted sequence from a list.
Syntax: (var seq) &body forms
FORMS should produce the binary coding of element in VAR."
  (let ((var (car loop-c))
	(seq (cadr loop-c)))
    (` (let ((seq (, seq)))
	 (sgml-code-number (length seq))       
	 (loop for (, var) in seq 
	       do (,@ body))))))

(put 'sgml-code-sequence 'lisp-indent-hook 1)
(put 'sgml-code-sequence 'edbug-forms-hook '(sexp &rest form))

(defun sgml-code-sexp (sexp)
  (let ((standard-output (current-buffer)))
    (print sexp)))

(defun sgml-code-tokens (l)
  (sgml-code-sequence (x l)
    (sgml-code-token x)))

(defsubst sgml-code-move (m)
  (sgml-code-token (sgml-move-token m))
  (insert (sgml-code-xlate (sgml-move-dest m))))

(defun sgml-code-model (m)
  (let ((sgml-code-xlate (sgml-translate-model m)))
    (sgml-code-sequence (s sgml-code-xlate)		; s is (node . number)
      (setq s (car s))			; s is node
      (cond
       ((sgml-normal-state-p s)
	(assert (and (< (length (sgml-state-opts s)) 255)
		     (< (length (sgml-state-reqs s)) 256)))
	(sgml-code-sequence (x (sgml-state-opts s))
	  (sgml-code-move x))
	(sgml-code-sequence (x (sgml-state-reqs s))
	  (sgml-code-move x)))
       (t				; s is a &-node
	(insert 255)			; Tag &-node
	(insert (sgml-code-xlate (sgml-&node-next s)))
	(sgml-code-sequence (m (sgml-&node-dfas s))
	  (sgml-code-model m)))))))

(defun sgml-code-element (el)
  (sgml-code-token (element-name el))
  (insert (+ (if (element-stag-optional el) 1 0)
	     (if (element-etag-optional el) 2 0)
	     (if (element-mixed el) 4 0)))
  (let ((c (element-model el))
	(name (element-name el))
	u)
    (cond ((eq c sgml-cdata) (insert 0))
	  ((eq c sgml-rcdata) (insert 1))
	  ((eq c sgml-empty) (insert 2))
	  ((eq c sgml-any) (insert 3))
	  (t
	   (assert (sgml-model-group-p c))
	   (insert 128)
	   (sgml-code-model c))))
  (sgml-code-tokens (element-includes el))
  (sgml-code-tokens (element-excludes el))
  (sgml-code-sexp (element-attlist el)))

(defun sgml-code-dtd (target)
  "Produce the binary coding of the current DTD into the TARGET buffer."
  (let ((elems sgml-buffer-element-map)
	(params sgml-buffer-param-entities)
	(entities sgml-buffer-entities)
	(doctype (element-model (sgml-tree-element sgml-top-tree)))
	(cb (current-buffer)))
    (set-buffer target)
    (erase-buffer)
    (insert
     ";;; This file was created by psgml on " (current-time-string) "\n"
     "(sgml-saved-dtd-version 2)\n")
    (let ((tv-point (point))		; insert token vector here
	  (done 0)			; count written elements
	  (tot (length elems)))
      (setq sgml-code-token-numbers nil)
      (sgml-code-sequence (pair elems)
	(setq done (1+ done))
	(sgml-code-element (cdr pair))
	(message "Coding %d%% done" (/ (* 100 done) tot)))
      (sgml-code-sexp params)
      (sgml-code-sexp entities)
      (sgml-code-sexp doctype)
      (goto-char tv-point)
      (sgml-code-sexp
       (loop for x in sgml-code-token-numbers
	     vconcat (list (car x)))))
    (set-buffer cb)))

;;;; Save DTD

(defun sgml-save-dtd (file)
  "Save the parsed dtd on FILE."
  (interactive
   (let* ((tem (expand-file-name
		(or sgml-default-dtd-file
		    (sgml-default-dtd-file))))
	  (dir (file-name-directory tem))
	  (nam (file-name-nondirectory tem)))
     (list
      (read-file-name "Save DTD in: " dir tem nil nam))))
  (setq file (expand-file-name file))
  (when (equal file (buffer-file-name))
    (error "Would clobber current file"))
  (unless sgml-buffer-element-map
    (sgml-parse-prolog))
  (cond ((equal (expand-file-name default-directory)
		(file-name-directory file))
	 (setq sgml-default-dtd-file (file-name-nondirectory file)))
	(t
	 (setq sgml-default-dtd-file file)))
  (let ((tem (generate-new-buffer " *savedtd*")))
    (unwind-protect
	(progn
	  (sgml-code-dtd tem)
	  (set-buffer tem)
	  (write-region (point-min) (point-max) file))
      (kill-buffer tem))))


;;; psgml-dtd.el ends here

