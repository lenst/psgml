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
  (sgml-make-* (sgml-make-primitive-content-token sgml-pcdata-token)))

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

(defun sgml-skip-ts ()
  ;; Skip over ts*
  ;;70  ts   = 5 s | EE | 60+ parameter entity reference
  ;;For simplicity I use ps*
  ;;65  ps   = 5 s | EE | 60+ parameter entity reference | 92 comment
  ;;*** some comments are accepted that shouldn't
  (sgml-skip-ps))

(defun sgml-parse-character-reference ()
  ;; *** Actually only numerical character references
  ;; I don't know how to handel the function character references.
  (if (sgml-parse-delim "CRO" digit)
      (prog1 (string-to-int (sgml-check-nametoken))
	(or (sgml-parse-delim "REFC")
	    (sgml-parse-char ?\n)))))

(defun sgml-parse-parameter-literal ()
  (let* (lita				; flag if lita
	 (qchar				; LIT or LITA
	  (following-char))
	 (qregexp			; what can be skipped in literal
	  (format "^%c%%&" qchar))
	 (value				; accumulates literals value
	  "")
	 (original-buffer		; Buffer (entity) where lit started
	  (current-buffer))
	 temp
	 )
    (cond
     ((or (sgml-parse-delim "LIT")
	  (setq lita (sgml-parse-delim "LITA")))
      (while (not (and (eq (current-buffer) original-buffer)
		       (sgml-parse-char qchar)))
	(cond ((eobp)
	       (or (sgml-pop-entity)
		   (sgml-error "Parameter literal unterminated")))
	      ((sgml-parse-parameter-entity-ref))
	      ((setq temp (sgml-parse-character-reference))
	       (setq value (concat value (format "%c" temp))))
	      (t
	       (setq value
		     (concat value
			     (buffer-substring
			      (point)
			      (progn (forward-char 1)
				     (skip-chars-forward qregexp)
				     (point)))))))
	)
      value))))

(defun sgml-check-parameter-literal ()
  (or (sgml-parse-parameter-literal)
      (sgml-parse-error "Parameter literal expected")))

(defun sgml-parse-external ()
  "Leaves nil if no external id, or (pubid . sysid)"
  (sgml-skip-ps)
  (let* ((p (point))
	 (token (sgml-parse-nametoken)))
    (cond
     (token
      (setq token (sgml-gname-symbol token))
      (sgml-skip-ps)
      (cond ((memq token '(public system))
	     (cons
	      (if (eq token 'public)
		  (or (sgml-parse-minimum-literal) ; the public id
		      (sgml-parse-error "Public identifier expected")))	;
	      (progn (sgml-skip-ps)
		     (sgml-parse-literal)))) ; the system id
	    (t
	     (goto-char p)
	     nil))))))

(defun sgml-parse-connector ()
  (sgml-skip-ps)
  (cond ((sgml-parse-delim "SEQ")
	 (function sgml-reduce-,))
	((sgml-parse-delim "OR")
	 (function sgml-reduce-|))
	((sgml-parse-delim "AND")
	 (function sgml-make-&))))

(defun sgml-parse-name-group ()
  "Parse a single name or a name group (general name case) .
Returns a list of strings or nil."
  (let (names)
    (cond
     ((sgml-parse-delim "GRPO")
      (sgml-skip-ps)
      (setq names (sgml-parse-name-group)) ; *** Allows more than it should
      (while (sgml-parse-connector)
	(sgml-skip-ps)
	(nconc names (sgml-parse-name-group)))
      (sgml-check-delim "GRPC")
      names)
     ((setq names (sgml-parse-name))
      (list names)))))

(defun sgml-check-name-group ()
  (or (sgml-parse-name-group)
      (sgml-parse-error "Expecting a name or a name group")))

(defun sgml-check-nametoken-group ()
  "Parse a name token group, return a list of strings.
Case transformed for general names."
  (sgml-skip-ps)
  (let ((names nil))
    (cond
     ((sgml-parse-delim GRPO)
      (while (progn
	       (sgml-skip-ps)
	       (push (sgml-general-case (sgml-check-nametoken)) names)
	       (sgml-parse-connector)))
      (sgml-check-delim GRPC)
      names)
     (t
      (list (sgml-general-case (sgml-check-nametoken)))))))

(defun sgml-check-element-type ()
  "Parse and check an element type (name), returns list of strings."
  (cond
   ((sgml-parse-delim GRPO)
    (sgml-skip-ts)
    (let ((names (sgml-check-element-type)))
      (while (progn (sgml-skip-ps)
		    (sgml-parse-connector))
	(sgml-skip-ts)
	(nconc names (sgml-check-element-type)))
      (sgml-check-delim GRPC)
      names))
   (t					; gi/ranked element
    (let ((name (sgml-check-name)))
      (sgml-skip-ps)
      (list (if (and (>= (following-char) ?0)
		     (<= (following-char) ?9))
		(sgml-general-case
		 (concat name (sgml-check-nametoken)))
	      name))))))

(defun sgml-check-external ()
  (or (sgml-parse-external)
      (sgml-parse-error "Expecting a PUBLIC or SYSTEM")))

;;;; Parse doctype: notation
(defun sgml-declare-notation ()
  ;;148  notation declaration = MDO, "NOTATION",
  ;;                        65 ps+, 41 notation name,
  ;;                        65 ps+, 149 notation identifier,
  ;;                        65 ps*, MDC
  ;;41   notation name    = 55 name
  ;;149  notation identifier = 73 external identifier
  (sgml-skip-ps)
  (sgml-check-name)
  (sgml-skip-ps)
  (sgml-check-external))


;;;; Parse doctype: Element

(defun sgml-parse-opt ()
  (sgml-skip-ps)
  (cond ((or (sgml-parse-char ?o)
	     (sgml-parse-char ?O))
	 t)
	((sgml-parse-char ?-)
	 nil)))

(defun sgml-parse-modifier ()
  (cond ((sgml-parse-delim PLUS)
	 (function sgml-make-+))
	((sgml-parse-delim REP)
	 (function sgml-make-*))
	((sgml-parse-delim OPT)
	 (function sgml-make-opt))))

(defun sgml-check-primitive-content-token ()
  (sgml-make-primitive-content-token
   (sgml-eltype-token
    (sgml-lookup-eltype
     (sgml-general-case
      (sgml-check-name))))))

(defun sgml-check-model-group ()
  (sgml-skip-ps)
  (let (el mod)
    (cond
     ((sgml-parse-delim GRPO)
      (let ((subs (list (sgml-check-model-group)))
	    (con1 nil)
	    (con2 nil))
	(while (setq con2 (sgml-parse-connector))
	  (cond ((and con1
		      (not (eq con1 con2)))
		 (sgml-parse-error "Mixed connectors")))
	  (setq con1 con2)
	  (setq subs (nconc subs (list (sgml-check-model-group)))))
	(sgml-check-delim GRPC)
	(setq el (if con1
		     (funcall con1 subs)
		   (car subs)))))
     ((sgml-parse-rni "pcdata")		; #PCDATA
      (setq sgml-used-pcdata t)
      (setq el (sgml-make-pcdata)))
     ((sgml-parse-delim DTGO)			; data tag group
      (sgml-skip-ts)
      (let ((tok (sgml-check-primitive-content-token)))
	(sgml-skip-ts) (sgml-check-delim SEQ)
	(sgml-skip-ts) (sgml-check-data-tag-pattern)
	(sgml-skip-ts) (sgml-check-delim DTGC)
	(setq el (sgml-make-conc tok (sgml-make-pcdata)))
	(setq sgml-used-pcdata t)))
     (t
      (setq el (sgml-check-primitive-content-token))))
    (setq mod (sgml-parse-modifier))
    (if mod
	(funcall mod el)
      el)))

(defun sgml-check-data-tag-pattern ()
  ;; 134  data tag pattern 
  ;; template | template group
  (cond ((sgml-parse-delim GRPO)
	 (sgml-skip-ts)
	 (sgml-check-parameter-literal)	; data tag template,
	 (while (progn (sgml-skip-ts)
		       (sgml-parse-delim OR))
	   (sgml-skip-ts)
	   (sgml-check-parameter-literal)) ; data tag template
	 (sgml-skip-ts)
	 (sgml-check-delim GRPC))
	(t
	 (sgml-check-parameter-literal))) ; data tag template
  (sgml-skip-ts)
  (when (sgml-parse-delim SEQ)
    (sgml-check-parameter-literal)))	; data tag padding template

(defun sgml-check-content-model ()
  (sgml-check-model-group))

(defun sgml-check-content ()
  (sgml-skip-ps)
  (cond ((sgml-is-delim GRPO)
	 (sgml-check-content-model))
	(t
	 (let ((dc (sgml-check-name)))	;CDATA or RCDATA or EMPTY
	   (intern (upcase dc))))))

(defun sgml-parse-exeption (type)
  (sgml-skip-ps)
  (if (sgml-parse-char type)
      (mapcar (function sgml-lookup-eltype)
	      (sgml-check-name-group))))

(defun sgml-declare-element ()
  (let* ((names (sgml-check-element-type))
	 (stag-opt (sgml-parse-opt))
	 (etag-opt (sgml-parse-opt))
	 (sgml-used-pcdata nil)
	 (model (sgml-check-content))
	 (exclusions (sgml-parse-exeption ?-))
	 (inclusions (sgml-parse-exeption ?+)))
    (while names
      (sgml-debug "Defining element %s" (car names))
      (sgml-define-eltype (sgml-lookup-eltype (car names))
			  stag-opt etag-opt model
			  exclusions inclusions
			  sgml-used-pcdata)
      (setq names (cdr names)))))


;;;; Parse doctype: Entity

(defun sgml-declare-entity ()
  (let (name				; Name of entity
	dest				; Entity table
	(type 'text)			; Type of entity
	text				; Text of entity
	extid				; External id 
	)
    (cond
     ((sgml-parse-delim PERO)		; parameter entity declaration
      (sgml-skip-ps)
      (setq dest (sgml-dtd-parameters sgml-dtd-info)))
     ((sgml-parse-rni "default"))	; *** where to store default?
     (t					; normal entity declaration
      (setq dest (sgml-dtd-entities sgml-dtd-info))))
    (when dest
      (setq name (sgml-check-name t)))
    (sgml-skip-ps)
    ;;105  entity text  = 66 parameter literal
    ;;                 | 106 data text
    ;;                 | 107 bracketed text
    ;;                 | 108 external entity specification
    (setq extid (sgml-parse-external))
    (setq text
	  (cond
	   (extid			; external entity specification =
					; 73 external identifier,
					; (65 ps+, 109+ entity type)?
	    (sgml-skip-ps)
	    (setq type (or (sgml-parse-entity-type) 'text))
	    extid)
	   ((sgml-startnm-char-next)
	    (let ((token (intern (sgml-check-name))))
	      (sgml-skip-ps)
	      (cond
	       ((memq token '(cdata sdata)) ; data text ***
		(setq type token)
		(sgml-check-parameter-literal))
	       ((eq token 'pi)
		(concat "<?" (sgml-check-parameter-literal) ">"))
	       ((eq token 'starttag)
		(sgml-start-tag-of (sgml-check-parameter-literal)))
	       ((eq token 'endtag)
		(sgml-end-tag-of (sgml-check-parameter-literal)))	
	       ((eq token 'ms)		; marked section
		(concat "<![" (sgml-check-parameter-literal) "]]>"))
	       ((eq token 'md)		; Markup declaration
		(concat "<!" (sgml-check-parameter-literal) ">")))))
	   ((sgml-check-parameter-literal))))
    (when dest
      (sgml-entity-declare name dest type text))))


(defun sgml-parse-entity-type ()
  ;;109+ entity type      = "SUBDOC"
  ;;                      | (("CDATA" | "NDATA" | "SDATA"),
  ;;                             65 ps+,
  ;;                             41 notation name,
  ;;                             149.2+ data attribute specification?)
  (let ((type (sgml-parse-name)))
    (when type
      (setq type (intern (downcase type)))
      (cond ((eq type 'subdoc))
	    ((memq type '(cdata ndata sdata))
	     (sgml-skip-ps)
	     (sgml-check-name)
	     ;;149.2+ data attribute specification
	     ;;                      = 65 ps+, DSO,
	     ;;                        31 attribute specification list,
	     ;;                        5 s*, DSC
	     (sgml-skip-ps)
	     (when (sgml-parse-delim DSO)
	       (sgml-parse-attribute-specification-list)
	       (sgml-parse-s)
	       (sgml-check-delim DSC)))
	    (t (sgml-error "Illegal entity type: %s" type))))
    type))


;;;; Parse doctype: Attlist

(defun sgml-declare-attlist ()
  (let* ((assnot (cond ((sgml-parse-rni "notation")
			(sgml-skip-ps)
			t)))
	 (assel (sgml-check-name-group))
	 (attlist nil)			; the list
	 (attdef nil))
    (while (setq attdef (sgml-parse-attribute-definition))
      (push attdef attlist))
    (setq attlist (nreverse attlist))
    (unless assnot
      (loop for elname in assel do
	    (sgml-define-eltype-attlist (sgml-lookup-eltype elname)
					attlist)))))

(defun sgml-parse-attribute-definition ()
  (sgml-skip-ps)
  (if (sgml-is-delim MDC) ; End of attlist?
      nil
    (sgml-make-attdecl (sgml-general-case (sgml-check-name))
		       (sgml-check-declared-value)
		       (sgml-check-default-value))))

(defun sgml-check-declared-value ()
  (sgml-skip-ps)
  (let ((type 'name-token-group)
	(names nil))
    (unless (eq (following-char) ?\()
      (setq type (sgml-gname-symbol (sgml-check-name)))
      (sgml-skip-ps))
    (when (memq type '(name-token-group notation))
      (setq names (sgml-check-nametoken-group)))
    (sgml-make-declared-value type names)))

(defun sgml-check-default-value ()
  (sgml-skip-ps)
  (let* ((rni (sgml-parse-rni))
	 (key (if rni (sgml-gname-symbol (sgml-check-name)))))
    (sgml-skip-ps)
    (sgml-make-default-value
     key
     (if (or (not rni) (eq key 'fixed))
	 (sgml-check-attribute-value-specification)))))


;;;; Parse doctype: shortref

;;;150  short reference mapping declaration = MDO, "SHORTREF",
;;;                        [[65 ps]]+, [[151 map name]],
;;;                        ([[65 ps]]+, [[66 parameter literal]],
;;;                        [[65 ps]]+, [[55 name]])+,
;;;                        [[65 ps]]*, MDC

(defun sgml-declare-shortref ()
  (sgml-check-name)			; map name
  (while (progn
	   (sgml-skip-ps)
	   (sgml-parse-parameter-literal))
    (sgml-skip-ps)
    (sgml-check-name)))

;;;152  short reference use declaration = MDO, "USEMAP",
;;;                        [[65 ps]]+, [[153 map specification]],
;;;                        ([[65 ps]]+, [[72 associated element type]])?,
;;;                        [[65 ps]]*, MDC

(defun sgml-declare-usemap ()
  (or (sgml-parse-rni "empty")
      (sgml-check-name))		; map specification
  (sgml-skip-ps)
  (sgml-parse-name-group)) 		; associated element type


;;;; Parse doctype

(defun sgml-check-dtd-subset ()
  (while 
      (cond
       ((sgml-parse-ds))
       ((sgml-parse-markup-declaration 'dtd))
       ;;((sgml-parse-marked-section-end)) ; end of marked section
       ((sgml-parse-delim "MS-END"))
       ))
  (when (sgml-any-open-param/file)
    (sgml-parse-error "DTD subset ended")))

(defun sgml-check-doctype-body ()
  ;; Parse the part of a DOCTYPE declaration after the <!DOCTYPE and 
  ;; before the >
  (message "Parsing doctype...")
  (let ((docname (sgml-check-name))
	(external (sgml-parse-external)))
    (setq sgml-dtd-info (sgml-make-dtd docname)) ; Create a empty DTD struct
    (sgml-skip-ps)
    (cond
     ((sgml-parse-delim "DSO")
      (sgml-check-dtd-subset)
      (sgml-check-delim "DSC")))
    (cond (external
	   (sgml-push-to-entity (sgml-make-entity docname 'dtd external))
	   (sgml-check-dtd-subset)))
    (sgml-skip-ps)
    (sgml-set-initial-state sgml-dtd-info))
  (message "Parsing doctype...done")
  (run-hooks 'sgml-doctype-parsed-hook))


;;;; Parse prolog

(defun sgml-parse-prolog ()
  "Parse the document prolog to learn the DTD."
  (interactive)
  (sgml-clear-log)
  (message "Parsing prolog...")
  (setq	sgml-dtd-info nil)
  (goto-char (point-min))
  (sgml-with-parser-syntax
   (while (progn (sgml-skip-ds)
		 (and (sgml-parse-markup-declaration 'prolog)
		      (null sgml-dtd-info)))))
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

(defun sgml-code-token-number (token)
  (let ((bp (assq token sgml-code-token-numbers)))
    (unless bp
      (setq sgml-code-token-numbers
	    (nconc sgml-code-token-numbers
		   (list (setq bp (cons token
					(length sgml-code-token-numbers)))))))
    (cdr bp)))

(defun sgml-code-token (token)
  (sgml-code-number (sgml-code-token-number token)))

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
    (prin1 sexp)
    (terpri)))

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
  (insert (+ (if (sgml-eltype-stag-optional el) 1 0)
	     (if (sgml-eltype-etag-optional el) 2 0)
	     (if (sgml-eltype-mixed el) 4 0)))
  (let ((c (sgml-eltype-model el))
	(name (sgml-eltype-name el))
	u)
    (cond ((eq c sgml-cdata) (insert 0))
	  ((eq c sgml-rcdata) (insert 1))
	  ((eq c sgml-empty) (insert 2))
	  ((eq c sgml-any) (insert 3))
	  ((null c) (insert 4))
	  (t
	   (assert (sgml-model-group-p c))
	   (insert 128)
	   (sgml-code-model c))))
  (sgml-code-tokens (sgml-eltype-includes el))
  (sgml-code-tokens (sgml-eltype-excludes el))
  (sgml-code-sexp (sgml-eltype-attlist el))
  (sgml-code-sexp (sgml-eltype-all-appdata el)))

(defun sgml-code-dtd (target)
  "Produce the binary coding of the current DTD into the TARGET buffer."
  (let ((dtd (sgml-pstate-dtd sgml-buffer-parse-state))
	(cb (current-buffer)))
    (set-buffer target)
    (erase-buffer)
    (insert
     ";;; This file was created by psgml on " (current-time-string) "\n"
     "(sgml-saved-dtd-version 3)\n")
    (sgml-code-sexp (sgml-dtd-doctype dtd))
    (let ((done 0)			; count written elements
	  tot)
      (setq sgml-code-token-numbers nil)
      (sgml-code-token-number sgml-pcdata-token) ; Make #PCDATA token 0
      (sgml-map-eltypes			; Assign numbers to all tokens
       (function (lambda (et)
		   (sgml-code-token-number (sgml-eltype-token et))))
       dtd)
      (setq tot (length sgml-code-token-numbers))
      ;; Produce the counted sequence of element type names
      (sgml-code-sequence (pair (cdr sgml-code-token-numbers))
	(sgml-code-sexp (sgml-eltype-name (car pair))))
      ;; Produce the counted sequence of element types
      (sgml-code-sequence (pair (cdr sgml-code-token-numbers))
	(setq done (1+ done))
	(sgml-code-element (car pair))
	(message "Coding %d%% done" (/ (* 100 done) tot)))
      (sgml-code-sexp (sgml-dtd-parameters dtd))
      (sgml-code-sexp (sgml-dtd-entities dtd))
      (set-buffer cb))))

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
  (sgml-need-dtd)
  (cond ((equal (expand-file-name default-directory)
		(file-name-directory file))
	 (setq sgml-default-dtd-file (file-name-nondirectory file)))
	(t
	 (setq sgml-default-dtd-file file)))
  (let ((tem (generate-new-buffer " *savedtd*"))
	(cb (current-buffer)))
    (unwind-protect
	(progn
	  (sgml-code-dtd tem)
	  (set-buffer tem)
	  (write-region (point-min) (point-max) file)
	  (set-buffer cb)
	  (setq sgml-loaded-dtd file))
      (kill-buffer tem))))


;;; psgml-dtd.el ends here
