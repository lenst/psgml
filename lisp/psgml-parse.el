;;;; psgml-parse.el --- Parser for SGML-editing mode with parsing support
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

;;;; Variables

(defvar sgml-parent-document nil
  "*File name (a string) of a file containing a DOCTYPE declaration to use,
or a list (FILENAME DOCTYPENAME), where FILENAME is a file name of a file '
containing a DOCTYPE declaration to use with the modification that the
document type name is DOCTYPENAME.")

;;; Internal variables
;;; See also parser state

(defvar sgml-parser-syntax nil
  "Syntax table used during parsing.")

(defvar sgml-throw-on-warning nil
  "Set to a symbol other than nil to make sgml-log-warning throw to that symbol.")

(defvar sgml-suppress-warning nil
  "Set to t to suppress warnings.")

(defvar sgml-goal 0
  "Point in buffer to parse up to.")

(defvar sgml-close-element-trap nil
  "Can be nil for no trap, an element or t for any element.
Tested by sgml-close-element to see if the parse should be ended.")

(defvar sgml-markup-type nil)
(defvar sgml-markup-tree)
(defvar sgml-markup-start)

(defconst sgml-pcdata-token (intern "#PCDATA"))

(defvar sgml-error-context nil)		; Vars used in *param* buffers
(defvar sgml-previous-buffer nil)	; "
(defvar sgml-parameter-name nil)	; "


;; For loading DTD
(eval-and-compile
  (defconst sgml-max-single-octet-number 250))

(defvar sgml-single-octet-threshold 255
  "Octets greater than this is the first of a two octet coding.")

(defvar sgml-read-token-vector nil)	; Vector of symbols used to decode
					; token numbers.
(defvar sgml-read-nodes nil)		; Vector of nodes used when reading
					; a finite automaton.

;; Variables used during doctype parsing and loading
(defvar sgml-element-map nil)		; assoc list of element types
(defvar sgml-param-entities nil)	; assoc list of parameter entities
(defvar sgml-used-pcdata nil)		; True if model group built is mixed
(defvar sgml-entities nil)		; List of general entity names
(defvar sgml-doctype nil)		; Top level state machine

;; Buffer local variables 

(defvar sgml-buffer-param-entities nil)
(make-variable-buffer-local 'sgml-buffer-element-map)

(defvar sgml-buffer-entities nil)
(make-variable-buffer-local 'sgml-buffer-entities)

(defvar sgml-buffer-element-map nil)
(make-variable-buffer-local 'sgml-buffer-element-map)

(defvar sgml-buffer-doctype nil)
(make-variable-buffer-local 'sgml-buffer-doctype)

(defvar sgml-top-tree nil)
(make-variable-buffer-local 'sgml-top-tree)

(defvar sgml-document-element nil)
(make-variable-buffer-local 'sgml-document-element)

(defvar sgml-loaded-dtd nil
  "File name corresponding to current DTD.")
(make-variable-buffer-local 'sgml-loaded-dtd)


;;;; Variable manipulation

(defun sgml-set-global ()
  "Copy the buffer local DTD data structures to global variables."
  (setq sgml-element-map sgml-buffer-element-map
	sgml-entities sgml-buffer-entities
	sgml-param-entities sgml-buffer-param-entities
	sgml-doctype sgml-buffer-doctype))

(defun sgml-set-local ()
  "Copy the global DTD data structures to buffer local variables."
  (setq sgml-buffer-element-map sgml-element-map
	sgml-buffer-entities sgml-entities
	sgml-buffer-param-entities sgml-param-entities)
  (sgml-set-doctype sgml-doctype))


;;;; Build parser syntax table

(setq sgml-parser-syntax (make-syntax-table))

(let ((i 0))
  (while (< i 256)
    (modify-syntax-entry i "-" sgml-parser-syntax)
    (setq i (1+ i))))

(mapconcat (lambda (c)
	     (modify-syntax-entry c "w" sgml-parser-syntax))
	   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrtsuvwxyz" "")
(mapconcat (lambda (c)
	     (modify-syntax-entry c "_" sgml-parser-syntax))
	   "-.0123456789" "")
(mapconcat (lambda (c)
	     (modify-syntax-entry c "." sgml-parser-syntax))
	   "</>&%#[]" ".")

;;(progn (set-syntax-table sgml-parser-syntax) (describe-syntax))


(defmacro sgml-with-parser-syntax (&rest body)
  (` (let ((normal-syntax-table (syntax-table)))
       (set-syntax-table sgml-parser-syntax)
       (unwind-protect
	   (progn (,@ body))
	 (set-syntax-table normal-syntax-table)))))
(put 'sgml-with-parser-syntax 'edebug-form-hook '(&rest form))

;;;; State machine

;; From the parsers POV a state is a mapping from tokens (in sgml it
;; is primitive state tokens) to states.  The pairs of the mapping is 
;; called moves.

;; DFAs are allways represented by the start state, which is a 
;; normal state.  Normal states contain moves of two types:
;; 1. moves for required tokens, 2. moves for optional tokens.
;; By design these are keept in two different sets.
;; [Alt: they could perhaps have been keept in one set but
;; marked in different ways.]

;; The &-model groups creates too big state machines, therefor
;; there is a datastruture called &-node.

;; A &-node is a specification for a dfa that has not been computed.
;; It contains a set of dfas that all have to be traversed befor going
;; to the next state.  The &-nodes are only stored in moves and are
;; not seen by the parser.  When a move is taken the &-node is converted
;; to a &-state.

;; A &-state keeps track of which dfas still need to be
;; traversed and the state of the current dfa.

;; move = <token, node>

;; node = normal-state | &-node

;; &-node = <dfas, next>  
;; where: dfas is a set of normal-state
;;        next is a normal-state

;; State = normal-state | &-state
;; The parser only knows about the state type.

;; normal-state = <opts, reqs>
;; where: opts is a set of moves for optional tokens
;; 	  reqs is a set of moves for required tokens

;; &-state = <substate, dfas, next>
;; where: substate is a normal-state
;;        dfas is a set of states
;;        next is the next state

;; The &-state is only used during the parsing.
;; Primitiv functions to get data from parse state need
;; to know both normal-state and &-state.


;;; Representations:

;;move: (token . node)

(defmacro sgml-make-move (token node)
  (` (cons (, token) (, node))))

(defmacro sgml-move-token (x)
  (` (car (, x))))

(defmacro sgml-move-dest (x)
  (` (cdr (, x))))

;; set of moves: list of moves

(defsubst sgml-add-move-to-set (token node set)
  (cons (cons token node) set))

(defmacro sgml-moves-lookup (token set)
  (` (assq (, token) (, set))))

;; normal-state: ('normal-state opts . reqs)

(defsubst sgml-make-state ()
  (cons 'normal-state (cons nil nil)))

(defmacro sgml-normal-state-p (s)
  (` (eq (car (, s)) 'normal-state)))

(defmacro sgml-state-opts (s)
  (` (cadr (, s))))

(defmacro sgml-state-reqs (s)
  (` (cddr (, s))))

(defsubst sgml-state-final-p (s)
  (null (sgml-state-reqs s)))

;; adding moves
;; *** Should these functions check for ambiguity?
;; What if adding a optional move for a token that has a 
;;  required move?
;; What about the other way?

(defsubst sgml-add-opt-move (s token dest)
  (or (sgml-moves-lookup token (sgml-state-opts s))
      (setf (sgml-state-opts s)
	    (sgml-add-move-to-set token dest (sgml-state-opts s)))))

(defsubst sgml-add-req-move (s token dest)
  (or (sgml-moves-lookup token (sgml-state-reqs s))
      (setf (sgml-state-reqs s)
	    (sgml-add-move-to-set token dest (sgml-state-reqs s)))))


;;&-state: (state next . dfas)

(defsubst sgml-make-&state (state dfas next)
  (cons state (cons next dfas)))

(defsubst sgml-step-&state (state &state)
  (cons state (cdr &state)))

(defsubst sgml-&state-substate (s)
  (car s))

(defsubst sgml-&state-dfas (s)
  (cddr s))

(defsubst sgml-&state-next (s)
  (cadr s))


;;&-node:  (next . dfas)

(defsubst sgml-make-&node (dfas next)
  (cons next dfas))

(defmacro sgml-&node-next (n)
  (` (car (, n))))

(defmacro sgml-&node-dfas (n)
  (` (cdr (, n))))


;;; Using states

;; get-move: State x Token --> State|nil

(defsubst sgml-get-move (state token)
  "Return a new state or nil, after traversing TOKEN from STATE."
  (cond
   ((sgml-normal-state-p state)
    (let ((c (or (sgml-moves-lookup token (sgml-state-opts state))
		 (sgml-moves-lookup token (sgml-state-reqs state)))))
      (if c
	  (let ((dest (sgml-move-dest c)))
	    (if (sgml-normal-state-p dest)
		dest
	      ;; dest is a &-node
	      (sgml-next-sub& (sgml-&node-dfas dest)
			      token
			      (sgml-&node-next dest)))))))
   (t					;state is a &-state
    (sgml-get-&move state token))))

(defun sgml-get-&move (state token)
  ;; state is a &-state
  (let ((m (sgml-get-move (sgml-&state-substate state) token)))
    (cond (m (cons m (cdr state)))
	  ((sgml-state-final-p (sgml-&state-substate state))
	   (sgml-next-sub& (sgml-&state-dfas state)
			   token
			   (sgml-&state-next state))))))

(defun sgml-next-sub& (dfas token next)
  "Compute the next state, choosing from DFAS and moving by TOKEN.
If this is not possible, but all DFAS are final, move by TOKEN in NEXT."
  (let ((allfinal t)
	(l dfas)
	(res nil)
	s1 s2)
    (while (and l (not res))
      (setq s1 (car l)
	    allfinal (and allfinal (sgml-state-final-p s1))
	    s2 (sgml-get-move s1 token)
	    res (and s2 (sgml-make-&state s2 (remq s1 dfas) next))
	    l (cdr l)))
    (cond (res)
	  (allfinal (sgml-get-move next token)))))

(defsubst sgml-tokens-of-moves (moves)
  (mapcar (lambda (m)
	    (sgml-move-token m))
	  moves))

(defun sgml-required-tokens (state)
  (if (sgml-normal-state-p state)
      (sgml-tokens-of-moves (sgml-state-reqs state))
    (or (sgml-required-tokens (sgml-&state-substate state))
        (loop for s in (sgml-&state-dfas state)
              append (sgml-tokens-of-moves (sgml-state-reqs s)))
        (sgml-tokens-of-moves (sgml-state-reqs (sgml-&state-next state))))))


(defun sgml-optional-tokens (state)
  (if (sgml-normal-state-p state)
      (sgml-tokens-of-moves (sgml-state-opts state))
    (append
     (sgml-optional-tokens (sgml-&state-substate state))
     (if (sgml-final (sgml-&state-substate state))
	 (loop for s in (sgml-&state-dfas state)
	       append (sgml-tokens-of-moves (sgml-state-opts s))))
     (if (loop for s in (sgml-&state-dfas state)
               always (sgml-state-final-p s))
	 (sgml-tokens-of-moves (sgml-state-opts (sgml-&state-next state)))))))


(defun sgml-final (state)
  (if (sgml-normal-state-p state)
      (sgml-state-final-p state)
    (and (sgml-final (sgml-&state-substate state))
         (loop for s in (sgml-&state-dfas state)
               always (sgml-state-final-p s))
         (sgml-state-final-p (sgml-&state-next state)))))


;;;; List of attributes
;;; An attribute is <name, declared-value, default-value>

(defun sgml-make-attribute (name dcl-value default-value)
  (list name dcl-value default-value))

(defun sgml-attribute-name (attrib)
  (car attrib))

(defun sgml-attribute-declared-value (attrib)
  "The declared value of ATTRIB.
It may be a symbol or (name-token-group (NAME1 ... NAMEn))
or (notation  (NOT1 ... NOTn))"
  (cadr attrib))

(defun sgml-attribute-default-value (attrib)
  "The default value of ATTRIB.
The default value is either a symbol (required | implied | current |
conref) or a list with first element nil or symbol 'fixed' and second
element the value."
  (caddr attrib))


;;;; Element content types

;; The content of an element is defined as
;;	 (125 declared content | 126 content model),
;; 125  declared content = "CDATA" | "RCDATA" | "EMPTY"
;; 126  content model    = (127 model group | "ANY"),
;;			 (65 ps+, 138 exceptions)?

;; I represent a model group with the first state of a corresponding finite 
;; automaton (this is a cons).  Exceptions are handled separately.
;; The other content types are represented by symbols.

(defsubst sgml-model-group-p (model)
  (consp model))

(defconst sgml-cdata 'CDATA)
(defconst sgml-rcdata 'RCDATA)
(defconst sgml-empty 'EMPTY)
(defconst sgml-any 'ANY)


;;;; Element objects

(defstruct element
  name					; Name of element (symbol)
  stag-optional				; Flag
  etag-optional				; Flag
  model					; Content type
  attlist				; List of defined attributes
  includes				; List of included elements
  excludes				; List of excluded elements
  mixed					; Flag if mixed content
  conref-regexp				; Regexp used to check for CONREF attr
  )

(defun sgml-define-element (name stag-opt etag-opt
				 content excludes includes mixed)
  (let ((el (make-element
	     :name name
	     :stag-optional stag-opt
	     :etag-optional etag-opt
	     :model content
	     :excludes excludes
	     :includes includes
	     :mixed mixed))
	(bp (assq name sgml-element-map)))
    ;; If the element is allready in the map, it is because its
    ;; attlist has been defined.
    (cond ((null bp)
	   (setq bp (cons name nil))
	   (setq sgml-element-map (cons bp sgml-element-map)))
	  (t
	   (setf (element-attlist el) (element-attlist (cdr bp))
		 (element-conref-regexp el)
		 (element-conref-regexp (cdr bp)))))
    (setcdr bp el)))

(defun sgml-define-element-attlist (name attlist)
  "Define the ATTLIST for NAME. Returns the element."
  (let ((bp (assq name sgml-element-map)))
    (cond
     (bp
      (setf (element-attlist (cdr bp)) attlist))
     (t
      (setq bp (cons name
		     (make-element :name name
				   :model sgml-any
				   :attlist attlist)))
      (push bp sgml-element-map)))
    (while attlist			; Find any conref attribute
      (cond				; and set conref regexp
       ((eq (sgml-attribute-default-value (car attlist))
	    'conref)
	(setf (element-conref-regexp (cdr bp))
	      (format "\\b%s[ \t\r\n]*=" (sgml-attribute-name (car attlist))))
	(setq attlist nil))
       (t
	(setq attlist (cdr attlist)))))
    (cdr bp)))

(defun sgml-lookup-element (name)
  "Lookup the element defintion for NAME (symbol or string)."
  (or (cdr-safe (assq (if (symbolp name) name (intern (downcase name)))
		       sgml-buffer-element-map))
      (progn
	(when (and sgml-warn-about-undefined-elements
		   sgml-buffer-element-map)
	  (sgml-log-warning "Undefined element %s" name))
	(make-element :name name :model sgml-any))))

(defun sgml-start-tag-of (element)
  "Return the start tag for ELEMENT (token or element)."
  (format "<%s>"
	  (if (vectorp element)
	      (if (element-p element)
		  (element-name element)
		(sgml-element-name element))
	    element)))

(defun sgml-end-tag-of (element)
  "Return the end tag for ELEMENT (token or element)."
  (format "</%s>"
	  (if (vectorp element)
	      (if (element-p element)
		  (element-name element)
		(sgml-element-name element))
	    element)))

;;;; Load a saved dtd

(defsubst sgml-read-octet ()
  (prog1 (following-char)
    (forward-char 1)))

(defsubst sgml-read-number ()
  "Read a number.
A number is 1: an octet [0--sgml-singel-octet-threshold]
or 2: two octets (n,m) interpreted as  (n-t-1)*256+m+t."
  (if (> (following-char) sgml-single-octet-threshold)
      (+ (* (- (following-char) (eval-when-compile
				 (1+ sgml-max-single-octet-number)))
	    256)
	 (prog1 (char-after (1+ (point)))
	   (forward-char 2))
	 sgml-max-single-octet-number)
    (sgml-read-octet)))

(defsubst sgml-read-peek ()
  (following-char))

(defun sgml-read-sexp ()
  (prog1
      (let ((standard-input (current-buffer)))
	(read))
    (skip-chars-forward " \t")
    (forward-char 1)))

(defsubst sgml-read-token ()
  (aref sgml-read-token-vector (sgml-read-number)))

(defsubst sgml-read-node-ref ()
  (aref sgml-read-nodes (sgml-read-octet)))

(defun sgml-read-model-seq ()
  (loop repeat (sgml-read-number) collect (sgml-read-model)))

(defun sgml-read-token-seq ()
  (loop repeat (sgml-read-number) collect (sgml-read-token)))

(defun sgml-read-moves ()
  (loop repeat (sgml-read-number)
	collect (sgml-make-move (sgml-read-token) (sgml-read-node-ref))))

(defun sgml-read-model ()
  (let* ((n (sgml-read-number))
	 (sgml-read-nodes (make-vector n nil)))
    (loop for i below n do (aset sgml-read-nodes i (sgml-make-state)))
    (loop for e across sgml-read-nodes do
	  (cond ((eq 255 (sgml-read-peek))	; a &node
		 (sgml-read-octet)		; skip
		 (setf (sgml-&node-next e) (sgml-read-node-ref))
		 (setf (sgml-&node-dfas e) (sgml-read-model-seq)))
		(t			; a normal-state
		 (setf (sgml-state-opts e) (sgml-read-moves))
		 (setf (sgml-state-reqs e) (sgml-read-moves)))))
    (aref sgml-read-nodes 0))) 

(defun sgml-read-content ()
  (let ((c (sgml-read-octet)))
    (cond ((eq c 0) sgml-cdata)
	  ((eq c 1) sgml-rcdata)
	  ((eq c 2) sgml-empty)
	  ((eq c 3) sgml-any)
	  ((eq c 128)
	   (sgml-read-model)))))

(defun sgml-read-decode-flag (flag mask)
  (not (zerop (logand flag mask))))

(defun sgml-read-element ()
  (let* ((name (sgml-read-token))
	 (flags (sgml-read-octet))
	 (content (sgml-read-content))
	 (incl (sgml-read-token-seq))
	 (excl (sgml-read-token-seq))
	 (attlist (sgml-read-sexp)))
    (sgml-define-element
     name
     (sgml-read-decode-flag flags 1)		; stag optional
     (sgml-read-decode-flag flags 2)		; etag optional
     content
     excl
     incl
     (sgml-read-decode-flag flags 4)		; mixed
     )
    (sgml-define-element-attlist name attlist)))

(defun sgml-read-dtd (buffer)
  "Decode the saved DTD in BUFFER, set global variabels."
  (let ((gc-cons-threshold (max gc-cons-threshold 500000))
	(cb (current-buffer))
	temp)
    (setq sgml-buffer-element-map nil
	  sgml-buffer-entities nil
	  sgml-buffer-param-entities nil)
    (setq sgml-element-map nil
	  sgml-entities nil
	  sgml-param-entities nil)
    (set-buffer buffer)
    (goto-char (point-min))
    (setq temp (sgml-read-sexp))		; file-version
    (cond ((equal temp '(sgml-saved-dtd-version 1))
	   (setq sgml-single-octet-threshold 255))
	  ((equal temp '(sgml-saved-dtd-version 2))
	   (setq sgml-single-octet-threshold sgml-max-single-octet-number))
	  (t
	   (error "Unknown file format for saved DTD: %s" temp)))
    ;; elements
    (setq sgml-read-token-vector (sgml-read-sexp))
    (loop repeat (sgml-read-number) do (sgml-read-element))
    (setq sgml-param-entities (sgml-read-sexp))
    (setq sgml-entities (sgml-read-sexp))
    (setq sgml-doctype (sgml-read-sexp))
    (set-buffer cb)))

(defun sgml-load-dtd (file)
  "Load a saved DTD from FILE."
  (interactive
   (let ((tem (expand-file-name
	       (or sgml-default-dtd-file
		   (sgml-default-dtd-file)))))
     (list (read-file-name "Load DTD from: "
			   (file-name-directory tem)
			   tem
			   t
			   (file-name-nondirectory tem)))))
  (setq file (expand-file-name file))
  (let ((cb (current-buffer))
	(tem nil)
	(doctype nil)
	(l (buffer-list)))
    ;; Search loaded buffer for a already loaded DTD
    (while (and l (null tem))
      (set-buffer (car l))
      (if (equal sgml-loaded-dtd file)
	  (setq tem (current-buffer)))
      (setq l (cdr l)))
    (cond
     (tem				; loaded DTD found
      (sgml-set-global))
     (t					; load DTD from file
      (set-buffer cb)
      (setq tem (generate-new-buffer " *saveddtd*"))
      (unwind-protect
	  (progn
	    (message "Loading DTD from %s..." file)
	    (set-buffer tem)
	    (insert-file-contents file)
	    (set-buffer cb)
	    (sgml-read-dtd tem)
	    (message "Loading DTD from %s...done" file))
	(kill-buffer tem))))
    (set-buffer cb)
    (sgml-set-local)
    (setq sgml-default-dtd-file file)
    (setq sgml-loaded-dtd file)))

;;;; Parse tree

(defstruct (sgml-tree
	    (:type vector)
	    (:constructor sgml-make-tree
			  (element start stag-len  parent level
				   excludes includes pstate mixed)))
  element				; element object
  start					; start point in buffer
  end					; end point in buffer
  stag-len				; length of start tag
  etag-len				; length of end tag
  parent				; parent tree
  level					; depth of this node
  excludes				; current excluded elements
  includes				; current included elements
  pstate				; state in parent
  next					; next sibling tree
  content				; child trees
  mixed					; cache for mixed flag in element
  net-enabled				; if NET enabled
)

;;;; (text) Element view of parse tree

(defmacro sgml-alias-fields (orig dest &rest fields)
  (let ((macs nil))
    (while fields
      (push
       (` (defmacro (, (intern (format "%s-%s" dest (car fields)))) (element)
	    (list
	     '(, (intern (format "%s-%s" orig (car fields))))
	     element)))
       macs)
      (setq fields (cdr fields)))
    (cons 'progn macs)))

(sgml-alias-fields sgml-tree sgml-element
  eltype				; element object
  start					; start point in buffer
  stag-len				; length of start tag
  etag-len				; length of end tag
  parent				; parent tree
  level					; depth of this node
  excludes				; current excluded elements
  includes				; current included elements
  pstate				; state in parent
  mixed					; cache for mixed flag in element
  net-enabled				; if NET enabled
  )

(defun sgml-element-model (element)
  "Declared content or content model of ELEMENT."
  (element-model (sgml-tree-element element)))

(defun sgml-element-name (element)
  (element-name (sgml-tree-element element)))

(defun sgml-element-stag-optional (element)
  (element-stag-optional (sgml-tree-element element)))

(defun sgml-element-etag-optional (element)
  (element-etag-optional (sgml-tree-element element)))

(defun sgml-element-attlist (element)
  (element-attlist (sgml-tree-element element)))

(defun sgml-element-stag-end (element)
  "Position after start tag of ELEMENT."
  (+ (sgml-tree-start element)
     (sgml-tree-stag-len element)))

(defun sgml-element-empty (element)
  "True if ELEMENT is empty."
  (let ((regexp (element-conref-regexp (sgml-tree-element element))))
      (or (eq sgml-empty (sgml-element-model element))
      (and regexp
	   (save-excursion
	     (goto-char (sgml-tree-start element))
	     (search-forward-regexp regexp
				    (+ (point)
				       (sgml-tree-stag-len element))
				    t))))))

(defun sgml-element-data-p (element)
  (or (sgml-element-mixed element)
      (eq sgml-cdata (sgml-element-model element))
      (eq sgml-rcdata (sgml-element-model element))))

(defun sgml-element-context-string (element)
  (if (eq element sgml-top-tree)
      ""
    (format "in %s %s"
	    (sgml-element-name element)
	    (sgml-element-context-string (sgml-tree-parent element)))))

;;;; Parser state

(defvar sgml-current-state nil
  "Current state in content model or model type if CDATA, RCDATA or ANY.")

(defvar sgml-current-tree nil
  "Current parse tree node, identifies open element.")

(defsubst sgml-current-element ()
  (sgml-tree-element sgml-current-tree))

(defsubst sgml-excludes ()
  (sgml-tree-excludes sgml-current-tree))

(defsubst sgml-includes ()
  (sgml-tree-includes sgml-current-tree))

(defsubst sgml-current-mixed-p ()
  (sgml-tree-mixed sgml-current-tree))

(defun sgml-set-doctype (model)
  (setq sgml-buffer-doctype model)
  (make-local-variable 'before-change-function)
  (setq before-change-function 'sgml-note-change-at)
  (setq sgml-document-element (and (sgml-model-group-p model)
				   (sgml-move-token
				    (car (sgml-state-reqs model)))))
  (setq sgml-top-tree
	(sgml-make-tree
	 (make-element :name "Document (no element)"
		       :model model)
	 0 0 nil 0 nil nil nil nil)))


(autoload 'sgml-make-primitive-content-token "psgml-dtd")

(defun sgml-set-doctype-element (element-name)
  (sgml-set-doctype
   (sgml-make-primitive-content-token element-name)))

(defun sgml-reset-parse-state ()
  (setf (sgml-tree-content sgml-top-tree) nil) ; do I need this?
  (sgml-set-parse-state sgml-top-tree 'start))

(defun sgml-set-parse-state (tree where)
  "Set parse state from TREE, either from start of TREE if WHERE is start
or from after TREE if WHERE is after."
  (setq sgml-current-tree tree
	sgml-markup-tree tree)
  (cond ((and (eq where 'start)
	      (not (sgml-element-empty tree)))
	 (setq sgml-current-state
	       (sgml-element-model sgml-current-tree))
	 (setq sgml-markup-type 'start-tag
	       sgml-markup-start (sgml-tree-start sgml-current-tree))
	 (goto-char (+ sgml-markup-start
		       (sgml-tree-stag-len sgml-current-tree))))
	(t
	 (setq sgml-current-state (sgml-tree-pstate sgml-current-tree))
	 (goto-char (sgml-tree-end sgml-current-tree))
	 (setq sgml-markup-type 'end-tag
	       sgml-markup-start (- (point)
				    (sgml-tree-etag-len sgml-current-tree)))
	 (setq sgml-current-tree (sgml-tree-parent sgml-current-tree)))))
	 

(defsubst sgml-final-p (state)
  ;; Test if a state/model can be ended
  (or (not (sgml-model-group-p state))
      (sgml-final state)))

(defun sgml-current-element-contains-data ()
  "Retrun true if the current open element is either mixed or is (r)cdata."
  (or (eq sgml-cdata sgml-current-state)
      (eq sgml-rcdata sgml-current-state)
      (sgml-current-mixed-p)))

(defun sgml-current-element-content-class ()
  "Return a string describing the type of content in the current element.
The type can be CDATA, RCDATA, ANY, #PCDATA or none."
  (cond ((eq sgml-cdata sgml-current-state)
	 "CDATA")
	((eq sgml-rcdata sgml-current-state)
	 "RCDATA")
	((eq sgml-any sgml-current-state)
	 "ANY")
	((sgml-current-mixed-p)
	 "#PCDATA")
	(t "")))

(defun sgml-open-element (name before-tag after-tag)
  (let* ((el (sgml-lookup-element name))
	 (nt (sgml-make-tree
	      el before-tag (- after-tag before-tag)
	      sgml-current-tree
	      (1+ (sgml-tree-level sgml-current-tree))
	      (append (element-excludes el) (sgml-excludes))
	      (append (element-includes el) (sgml-includes))
	      sgml-current-state (element-mixed el)
	      )))
    (cond ((null (sgml-tree-content sgml-current-tree))
	   (setf (sgml-tree-content sgml-current-tree) nt))
	  (t
	   (let ((u (sgml-tree-content sgml-current-tree)))
	     (while (and (sgml-tree-next u)
			 (> before-tag (sgml-tree-start (sgml-tree-next u))))
	       (setq u (sgml-tree-next u)))
	     (setf (sgml-tree-next u) nt))))
    (setf (sgml-tree-next sgml-current-tree) nil)
    (setq sgml-current-state (element-model el)
	  sgml-current-tree nt)
    (setq sgml-markup-tree sgml-current-tree)
    (when (sgml-element-empty sgml-current-tree)
      (sgml-close-element after-tag after-tag))))


(defun sgml-fake-open-element (tree el)
  (sgml-make-tree
   el 0 0 
   tree
   0
   (append (element-excludes el) (sgml-tree-excludes tree))
   (append (element-includes el) (sgml-tree-includes tree))
   nil nil))

(defun sgml-close-element (before-tag after-tag)
  (when (or (eq sgml-close-element-trap t)
	    (eq sgml-close-element-trap sgml-current-tree))
    (setq sgml-goal (point)))
  (setf (sgml-tree-end sgml-current-tree) after-tag)
  (setf (sgml-tree-etag-len sgml-current-tree) (- after-tag before-tag))
  (setq sgml-markup-tree sgml-current-tree)
  (cond ((eq sgml-current-tree sgml-top-tree)
	 (unless (eobp)
	   (sgml-error "Parse ended")))
	(t
	 (setq sgml-current-state (sgml-tree-pstate sgml-current-tree)
	       sgml-current-tree (sgml-tree-parent sgml-current-tree)))))


(defun sgml-context-as-string ()
  (format "%s %s"
	  (if (sgml-model-group-p sgml-current-state)
	      (sgml-current-element-content-class)
	    sgml-current-state)
	  (if (eq sgml-current-tree sgml-top-tree)
	      "in empty context"
	    (sgml-element-context-string sgml-current-tree))))

(defun sgml-list-of-elements (tree state)
  (let* ((req (if (sgml-model-group-p state)
		  (sgml-required-tokens state)))
	 (elems
	  (if (sgml-model-group-p state)
	      (append req
		      (delq sgml-pcdata-token (sgml-optional-tokens state)))))
	 (in (sgml-tree-includes tree))
	 (ex (sgml-tree-excludes tree)))
    ;; Modify for exceptions
    (while in
      (unless (memq (car in) elems)
	      (setq elems (nconc elems (list (car in)))))
      (setq in (cdr in)))
    (while ex
      (setq elems (delq (car ex) elems))
      (setq ex (cdr ex)))
    ;; Check for omitable start tags
    (when (and sgml-omittag-transparent
	       (not (sgml-final-p state))
	       req
	       (null (cdr req)))
	  (let ((el (sgml-lookup-element (car req))))
	    (when (element-stag-optional el)
		  (setq elems
			(nconc elems	; *** possibility of duplicates
			       (sgml-list-of-elements
				(sgml-fake-open-element tree el)
				(element-model el)))))))
    elems))

(defun sgml-current-list-of-valid-elements ()
  "Returns a list of contextually valid elements."
  (let ((elems (sgml-list-of-elements sgml-current-tree sgml-current-state))
	(tree sgml-current-tree)
	(state sgml-current-state))
    (when sgml-omittag-transparent
      (while (and tree
		  (sgml-final-p state)
		  (sgml-element-etag-optional tree))
	(setq state (sgml-tree-pstate tree)
	      tree (sgml-tree-parent tree))
	(loop for e in (sgml-list-of-elements tree state)
	      do
	      (when (not (memq e elems))
		(setq elems (nconc elems (list e)))))))
    (sort elems (function string-lessp))))

(defun sgml-current-list-of-required-elements ()
  (let ((req (sgml-required-tokens sgml-current-state)))
    (if req
	req
      (if (and sgml-omittag-transparent
	       (sgml-model-group-p sgml-current-state)
	       (sgml-final-p sgml-current-state)
	       (sgml-element-etag-optional sgml-current-tree))
	  (sgml-required-tokens
	   (sgml-tree-pstate sgml-current-tree))))))

(defun sgml-stags-of-names (names)
  (mapcar (function sgml-start-tag-of) names))

(defun sgml-current-list-of-valid-tags ()
  (let* ((elems (sgml-current-list-of-valid-elements))
	 (tags (sgml-stags-of-names elems))
	 (etags nil)
	 (tree sgml-current-tree)
	 (state sgml-current-state))
    (if sgml-balanced-tag-edit
	tags
      (while
	  (and (sgml-final-p state)
	       (not (eq tree sgml-top-tree))
	       (progn
		 (setq etags
		       (nconc etags
			      (list (sgml-end-tag-of
				     (sgml-tree-element tree)))))
		 sgml-omittag-transparent)
	       (sgml-element-etag-optional tree))
	(setq state (sgml-tree-pstate tree)
	      tree (sgml-tree-parent tree)))
      (nconc etags tags))))

;;;; Logging of warnings

(defconst sgml-log-buffer-name "*SGML LOG*")

(defvar sgml-log-last-size 0)

(defun sgml-display-log ()
  (let ((buf (get-buffer sgml-log-buffer-name)))
    (when buf
      (display-buffer buf)
      (setq sgml-log-last-size (save-excursion (set-buffer buf)
					       (point-max))))))

(defun sgml-log-warning (format &rest things)
  (unless sgml-suppress-warning
    (when sgml-throw-on-warning
      (apply 'message format things)
      (throw sgml-throw-on-warning t))
    (apply 'sgml-message format things)
    (apply 'sgml-log-message format things)))

(defun sgml-log-message (format &rest things)
  (let ((mess (apply 'format format things))
	(buf (get-buffer-create sgml-log-buffer-name))
	(cb (current-buffer)))
    (set-buffer buf)
    (goto-char (point-max))
    (insert mess "\n")
    (when (get-buffer-window buf)
      (setq sgml-log-last-size  (point-max)))
    (set-buffer cb)))

(defun sgml-error (format &rest things)
  (while (and (boundp 'sgml-previous-buffer) sgml-previous-buffer)
    (when sgml-parameter-name
      (sgml-log-message "in %%%s " sgml-parameter-name))
    (when sgml-error-context
      (sgml-log-message "In file %s line %s"
		      sgml-error-context (count-lines (point-min) (point))))
    (sgml-pop-param))
  (apply 'sgml-log-warning format things)
  (apply 'error format things))

(defun sgml-parse-error (format &rest things)
  (apply 'sgml-error
	 (concat format "; at: %s")
	 (append things (list (buffer-substring
			       (point)
			       (min (point-max) (+ (point) 12)))))))

(defun sgml-message (format &rest things)
  (let ((buf (get-buffer sgml-log-buffer-name)))
    (when (and buf
	       (> (save-excursion (set-buffer buf)
				  (point-max))
		  sgml-log-last-size))
      (sgml-display-log)))
  (apply 'message format things))

(defun sgml-reset-log ()
  (let ((buf (get-buffer sgml-log-buffer-name)))
    (when buf
      (setq sgml-log-last-size
	    (save-excursion (set-buffer buf)
			    (point-max))))))

(defun sgml-clear-log ()
  (let ((b (get-buffer sgml-log-buffer-name)))
    (when b
      (delete-windows-on b)
      (kill-buffer b)
      (setq sgml-log-last-size 0))))

(defun sgml-show-or-clear-log ()
  "Show the *SGML LOG* buffer if it is not showing, or clear and
remove it if it is showing."
  (interactive)
  (cond ((and (get-buffer sgml-log-buffer-name)
	      (null (get-buffer-window sgml-log-buffer-name)))
	 (sgml-display-log))
	(t
	 (sgml-clear-log))))

;;;; External identifyer resolve

(defun sgml-external-file (extid)
  ;; extid is (pubid . sysid)
  (let* ((pubid (car extid))
	 (sysid (cdr extid))
	 file)
    (cond (sysid			;use sysid if given
	   (let ((l sgml-system-path))
	     (while (and l (not file))
	       (unless (file-exists-p
			(setq file (concat (car l) "/" sysid)))
		 (setq file nil))
	       (setq l (cdr l)))
	     (unless file
	       (sgml-error "System id %s not found" sysid))
	     file))
	  (pubid
	   (setq file (sgml-map-public pubid))
	   (unless file
	     (sgml-error "Public id %s; can't find file"
		       pubid))
	   file)
	  (t
	   (sgml-error
	    "No external file to be found: need a system identifier")))))

(defun sgml-map-public (pubid)
  (let ((pubid-parts (sgml-pubid-parts pubid))
	(l sgml-public-map)
	res)
    (while (and l (not res))
      (cond
       ((and (consp (car l))
	     (string-match (caar l) pubid))
	(setq res (sgml-pub-expand (cdar l) pubid-parts)))
       ((stringp (car l))
	(setq res (sgml-pub-expand (car l) pubid-parts))))
      (when res
	(unless (file-exists-p
		 (setq res (substitute-in-file-name res)))
	  (setq res nil)))
      (setq l (cdr l)))
    res))

(defun sgml-pubid-parts (pubid)
  (string-match
   "^\\(+//\\|-//\\)?\\(\\([^/]\\|/[^/]\\)+\\)//\\([^ \t\n]*\\)[ \t\n]+\\(\\([^/]\\|/[^/]\\)*\\).*"
   pubid)
  (list
   (cons ?o (sgml-transliterate-file (sgml-matched-string pubid 2)))
   (cons ?c (downcase (sgml-matched-string pubid 4)))
   (cons ?d (sgml-transliterate-file (sgml-matched-string pubid 5)))))

(defun sgml-pub-expand-char (c parts)
  (cond ((memq (downcase c) '(?c ?o ?d))
	 (cdr-safe (assq (downcase c) parts)))
	(t
	 (char-to-string c))))

(defun sgml-transliterate-file (string)
  (mapconcat (lambda (c)
	       (char-to-string
		(or (cdr-safe (assq c sgml-public-transliterations))
		    c)))
	   string ""))

(defun sgml-pub-expand (s parts)
  (loop for i from 0 to (1- (length s))
	as c = (aref s i)
	concat (if (eq c ?%)
		   (or (sgml-pub-expand-char (aref s (incf i)) parts)
		       (return nil)) 
		 (char-to-string (aref s i)))))


(defun sgml-matched-string (string n &optional regexp noerror)
  (let ((res (if regexp
		 (or (string-match regexp string)
		     noerror
		     (error "String match fail")))))
    (if (or (null regexp)
	    (numberp res))
	(substring string (match-beginning n)
		   (match-end n)))))


;;;; Parameter entities and files

(defun sgml-set-param-entity (param value)
  (let ((bp (assq param sgml-param-entities)))
    (cond (bp				; Already defined
	   ;;(setcdr bp value) ; ignore
	   (sgml-debug "Ignoring redefinition of %%%s" param)
	   )
	  (t
	   (sgml-debug "Defining entity %s as %s" param value)
	   (setq sgml-param-entities
		 (cons (cons param value)
		       sgml-param-entities))))))

(defun sgml-push-to-param (param)
  (let ((cb (current-buffer))
	(val (cdr-safe (assq param sgml-param-entities)))
	(buf (generate-new-buffer " *param*")))
    (sgml-debug "Enter param %s" param)
    (set-buffer buf)
    (set-syntax-table sgml-parser-syntax)
    (make-local-variable 'sgml-previous-buffer)
    (setq sgml-previous-buffer cb)
    (make-local-variable 'sgml-parameter-name)
    (setq sgml-parameter-name param)
    (cond ((consp val)			; external entity
	   (let ((file (sgml-external-file val)))
	     (make-local-variable 'sgml-error-context)
	     (setq sgml-error-context file)
	     (insert-file-contents file)))
	  ((stringp val)
	   (insert val))
	  (t				; sgml-warn-undefined-entity ***
	   (sgml-log-warning "Entity %s undefined" param)))
    (goto-char (point-min))))

(defun sgml-push-to-file (file)
  (let ((cb (current-buffer))
	(buf (generate-new-buffer " *param*")))
    (set-buffer buf)
    (set-syntax-table sgml-parser-syntax)
    (make-local-variable 'sgml-previous-buffer)
    (setq sgml-previous-buffer cb)
    (make-local-variable 'sgml-error-context)
    (setq sgml-error-context file)
    (sgml-debug "Push to file %s" file)
    (insert-file-contents file)
    (goto-char (point-min))))

(defun sgml-pop-param ()
  (cond ((and (boundp 'sgml-previous-buffer)
	      (bufferp sgml-previous-buffer))
	 (sgml-debug "Exit param")
	 (kill-buffer (prog1 (current-buffer)
			(set-buffer sgml-previous-buffer)))
	 t)))

(defun sgml-any-open-param/file ()
  "Return true if there currently is a parameter or file open."
  (and (boundp 'sgml-previous-buffer)
       sgml-previous-buffer))

;;;; General lexical functions
;;; Naming conventions
;;; sgml-parse-xx  try to parse xx, return nil if can't else return
;;;		   some propriate non-nil value.
;;;                Except: for name/nametoken parsing, return 0 if can't.
;;; sgml-check-xx  require xx, report error if can't parse.  Return 
;;;                aproporiate value.

(defmacro sgml-parse-char (char)
  (` (cond ((eq (, char) (following-char))
	    (forward-char 1)
	    t))))

(defmacro sgml-parse-chars (char1 char2 &optional char3)
  "Parse two or three chars; return nil if can't"
  (if (null char3)
      (` (cond ((and (eq (, char1) (following-char))
		 (eq (, char2) (char-after (1+ (point)))))
	    (forward-char 2)
	    t)))
    (` (cond ((and (eq (, char1) (following-char))
		 (eq (, char2) (char-after (1+ (point))))
		 (eq (, char3) (char-after (1+ (1+ (point))))))
	    (forward-char 3)
	    t)))))

(defsubst sgml-check-char (char)
  (cond ((not (sgml-parse-char char))
	 (sgml-parse-error "Expecting %c" char))))

(defun sgml-parse-vi ()
  (sgml-parse-char ?=))

(defmacro sgml-parse-mdo (&optional name)
  "Parse a MDO optionally follwed by NAME, if can't return nil."
  (if name
      (`
       (if (sgml-parse-chars ?< ?!)
	   (let ((p (point)))
	     (if (eq (sgml-parse-nametoken) (, name))
		 t
	       (goto-char (- p 2))
	       nil))))
    '(sgml-parse-chars ?< ?!)))

(defsubst sgml-check-mdc ()
  (sgml-check-char ?>))

(defsubst sgml-parse-marked-section-start ()
  (sgml-parse-chars ?< ?! ?\[))

(defun sgml-parse-marked-section-end ()
  (sgml-parse-chars ?\] ?\] ?>))

(defmacro sgml-startnm-char (c)
  (` (eq ?w (char-syntax (, c)))))

(defsubst sgml-startnm-char-next ()
  (sgml-startnm-char (following-char)))

(defsubst sgml-name-char (c)
  (or (sgml-startnm-char c)
      (eq ?_ (char-syntax c))))

(defsubst sgml-is-end-tag ()
  (and (eq ?< (following-char))
       (eq ?/ (char-after (1+ (point))))
       (sgml-startnm-char (char-after (+ 2 (point))))))

(defsubst sgml-is-start-tag ()
  (and (eq ?< (following-char))
       (or (sgml-startnm-char (char-after (1+ (point))))
	   (eq ?> (char-after (1+ (point)))))))

(defsubst sgml-skip-s ()
  (/= 0 (skip-chars-forward " \t\n\r")))

(defsubst sgml-skip-processing-instruction ()
  (and (sgml-parse-chars ?< ??)
       (progn (skip-chars-forward "^>")
	      (sgml-check-char ?>)
	      (setq sgml-markup-type 'pi)
	      t)))

(defun sgml-check-name ()
  (or (sgml-startnm-char-next)
      (sgml-parse-error "Expecting a name"))
  (let ((start (point)))
    (skip-syntax-forward "w_")
    (intern (downcase (buffer-substring start (point))))))

(defun sgml-check-ename ()
  "Parse an entity name"
  (sgml-skip-ps)
  (or (sgml-startnm-char-next)
      (sgml-error "Expecting a name"))
  (let ((start (point)))
    (skip-syntax-forward "w_")
    (intern (buffer-substring start (point)))))

(defun sgml-parse-nametoken ()
  "Parses a name token and returns it.  Returns 0 if unabel to parse."
  (if (sgml-name-char (following-char))
      (let ((start (point)))
	(skip-syntax-forward "w_")
	(intern (downcase (buffer-substring start (point)))))
    0))

(defun sgml-parse-nametoken-string ()
  "Parses a name token and returns a string or nil if no nametoken."
  (and (sgml-name-char (following-char))
       (let ((start (point)))
	 (skip-syntax-forward "w_")
	 (buffer-substring start (point)))))

(defun sgml-gname-symbol (string)
  "Convert a string to a general name/nametoken/numbertoken."
  (intern (downcase string)))

(defun sgml-ename-symbol (string)
  "Convert a string to an entity name."
  (intern string))

(defun sgml-check-nametoken ()
  "Require and return a nametoken as a symbol."
  (sgml-gname-symbol (or (sgml-parse-nametoken-string)
			 (sgml-parse-error "Expecting a name token"))))

(defun sgml-check-entity-ref ()
  (prog1 (sgml-check-ename)
    (sgml-parse-char ?\;)))

(defun sgml-skip-ps ()
  (while
      (or (sgml-skip-s)
	  (and (eobp) (sgml-pop-param))
	  (cond ((sgml-parse-char ?%)	;parameter entity reference
		 (cond ((sgml-startnm-char-next)
			(sgml-push-to-param (sgml-check-entity-ref))
			t)
		       (t
			(forward-char -1)
			nil)))
		((and (eq ?- (following-char))
		      (eq ?- (char-after (1+ (point)))))
		 (forward-char 2)
		 (search-forward "--"))))))

(defsubst sgml-skip-comment-declaration ()
  "If point is at a comment declaration, skip it and return true.
Else return false."
  (and (eq ?< (following-char))
       (eq ?! (char-after (1+ (point))))
       (cond ((eq ?> (char-after (+ 2 (point))))
	      (forward-char 3)
	      t)
	     ((and (eq ?- (char-after (+ 2 (point))))
		   (eq ?- (char-after (+ 3 (point)))))
	      (forward-char 2)
	      (sgml-skip-ps)
	      (sgml-check-mdc)
	      t))
       (setq sgml-markup-type 'comment)))

(defun sgml-parse-ds ()
;71  ds   = 5 s | EE | 60+ parameter entity reference
;         | 91 comment declaration
;         | 44 processing instruction
;         | 93 marked section declaration ***
  (or (and (eobp) (sgml-pop-param))	;EE
      (sgml-skip-s)			;5 s
      (sgml-skip-comment-declaration)	;91 comment declaration
      (if (sgml-parse-char ?%)	;parameter entity reference
	  (progn (sgml-push-to-param (sgml-check-entity-ref))
		 t))
      (sgml-skip-processing-instruction)))

(defun sgml-skip-ds ()
  (while (sgml-parse-ds)))

(defmacro sgml-parse-rni (&optional name)
  "Parse a RNI (#) return nil if none; with optional NAME, 
a RNI must be followed by NAME."
  (cond
   (name
    (` (if (sgml-parse-char ?#)
	   (if (eq (sgml-check-name) (, name))
	       t
	     (sgml-parse-error "Reserved name not expected")))))
   (t '(sgml-parse-char ?#))))

(defun sgml-parse-string ()
  "Parse a quoted SGML string and return it, if no string return nil."
  (sgml-skip-ps)
  (let ((qchar (following-char))
	start
	value)
    (cond ((memq qchar '(?\" ?\'))
	   (forward-char 1)
	   (setq start (point))
	   (skip-chars-forward (format "^%c" qchar))
	   (setq value (buffer-substring start (point)))
	   (sgml-check-char qchar)
	   value))))

(defun sgml-skip-cdata ()
  (while (progn (skip-chars-forward "^<")
		(not (sgml-is-end-tag)))
    (forward-char 1)))

(defun sgml-skip-doctype ()
  (cond ((sgml-parse-mdo 'doctype)
	 (skip-chars-forward "^>[")
	 (cond ((eq ?\[ (following-char))
		(sgml-skip-braces)))
	 (sgml-skip-ps)
	 (sgml-check-mdc)
	 (setq sgml-markup-type 'doctype)
	 t)))

(defun sgml-skip-braces ()
  (sgml-check-char ?\[)
  (while
      (progn (skip-chars-forward "^[]'\"\\-")
	     (not (eq ?\] (following-char))))
    (cond ((sgml-parse-chars ?- ?-)
	   (search-forward "--"))
	  ((sgml-parse-char ?-))
	  ((sgml-parse-string))
	  ((sgml-skip-braces))))
  (sgml-check-char ?\]))

(defun sgml-skip-markup-declaration ()
  (while (progn (skip-chars-forward "^'\"\\->")
		(not (eq ?> (following-char))))
    (cond ((sgml-parse-chars ?- ?-)
	   (search-forward "--"))
	  ((sgml-parse-char ?-))
	  ((sgml-parse-string))))
  (sgml-check-mdc))

(defun sgml-skip-sgml-dcl ()
  (and (sgml-parse-mdo 'sgml)
       (progn (sgml-skip-markup-declaration)
	      (setq sgml-markup-type 'sgml)
	      t)))

(defun sgml-parse-external ()
  "Leaves nil if no external id, or (pubid . sysid)"
  (sgml-skip-ps)
  (let* ((p (point))
	 (token (sgml-parse-nametoken)))
    (cond ((eq token 0) nil)		; no nametoken
	  ((eq token 'public)
	   (cons
	    (or (sgml-parse-string)	;the public id
		(sgml-error "Public identifier expected"))
	    (progn (sgml-skip-ps)
		   (sgml-parse-string)))) ;the system id
	  ((eq token 'system)
	   (sgml-skip-ps)
	   (cons nil (sgml-parse-string)))
	  (t
	   (goto-char p)
	   nil))))

;;;; Parse marked section

(defun sgml-parse-marked-section ()
  (let ((status nil))
    (while (progn (sgml-skip-ps)
		  (not (sgml-parse-char ?\[)))
      (push (sgml-check-name)
	    status))
    (cond
     ((memq 'ignore status)
      (sgml-skip-marked-section)
      (setq sgml-markup-type 'ignored))
     ((or (memq 'cdata status)
	  (memq 'rcdata status))
      (or (search-forward "]]>" nil t)
	  (sgml-error "CDATA marked section not terminated"))
      (setq sgml-markup-type sgml-cdata))
     (t
      (setq sgml-markup-type 'ms-start)))))
  
(defun sgml-skip-marked-section ()
  (while (not (sgml-parse-marked-section-end))
    (or (re-search-forward "]]>\\|<!\\[" nil t)
	(sgml-error "Marked section unterminated"))
    (goto-char (match-beginning 0))
    (when (sgml-parse-marked-section-start)
      (search-forward "[")
      (sgml-skip-marked-section))))

;;;; Parsing attribute values

(defun sgml-parse-attribute-specification-list (&optional element)
  "Parse an attribute specification list.
Optional argument ELEMENT, is used to resolve omitted name=.
Returns a list ((name val) ...).
Where name is a symbol and val is a string."
  (let (name val asl)
    (while (setq name (progn (sgml-skip-s)
			     (sgml-parse-nametoken-string)))
      (sgml-skip-s)
      (cond ((sgml-parse-vi)
	     (setq val (sgml-parse-attribute-value-specification)))
	    ((null element)
	     (sgml-parse-error "Expecting a ="))
	    ((setq name (sgml-find-name-for-value (setq val name)
						  element)))
	    (t
	     (sgml-log-warning
	      "%s is not in any name group for element %s."
	      val
	      (sgml-element-name element))))
      (push (list (sgml-gname-symbol name) val) asl))
    asl))

(defun sgml-parse-attribute-value-specification ()
  (or (sgml-parse-string)
      (sgml-parse-nametoken-string)
      (sgml-parse-error "Expecting an attribute value: literal or token")))

(defun sgml-find-name-for-value (value element)
  "Find the attribute of ELEMENT that has VALUE in its name group.
VALUE is a string.  Returns nil or a string."
  (if (stringp value)
      (setq value (sgml-gname-symbol value)))
  (let ((al (sgml-element-attlist element))
	dv)
    (while (and al
		(or (atom (setq dv (sgml-attribute-declared-value (car al))))
		    (not (eq (car dv) 'name-token-group))
		    (not (memq value (cadr dv)))))
      (setq al (cdr al)))
    (if al
	(symbol-name (sgml-attribute-name (car al))))))

;;;; Parser driver

;; The parser maintains a partial parse tree during the parse.  This tree
;; can be inspected to find information, and also be used to restart the
;; parse.  The parser also has a postition in the current content model.
;; (Called a state.)  The parser is used for several things:
;; 1) To find the state the parser would be in at a point in the buffer.
;;    (Point in emacs sense, I.e. between chararacters).
;; 2) Identify the element containing a character.
;; 3) Find end of an element.
;; 4) Find the next element.
;; 5) To find the previous element.

;; These tasks are done by a combination of parsing and traversing
;; the partial parse tree.  The primite parse operation is to parse
;; until a goal point in the buffer has been passed.  In addition to
;; this it is possible to "trap" closing of elements.  Either for a
;; specific element or for any element.  When the trap is sprung the
;; parse is ended.  This is used to extend the parse tree.  When the
;; trap is used the parser is usually called with the end of the
;; buffer as the goal point.

(defun sgml-need-dtd ()
  "Make sure that an eventual DTD is parsed or loaded."
  (save-excursion (sgml-parse-to (point-min))))

(defun sgml-parse-until-end-of (sgml-close-element-trap)
  "Parse until the SGML-CLOSE-ELEMENT-TRAP has ended,
or if it is t, any additional element has ended,
or if nil, until end of buffer."
  (sgml-parse-to (point-max))
  (when (eobp)				; End of buffer, can imply
					; end of any open element.
    (while (prog1 (not
		   (or (eq sgml-close-element-trap t)
		       (eq sgml-close-element-trap sgml-current-tree)
		       (eq sgml-current-tree sgml-top-tree)))
	     (sgml-implied-end-tag "buffer end" (point) (point))))))

(defsubst sgml-do-pcdata ()
  ;; Parse pcdata
  (while				; Until token accepted
      (cond
       ((eq sgml-current-state sgml-any) nil)
       ((sgml-get-move sgml-current-state sgml-pcdata-token)
	nil)
       ((sgml-do-implied "data character"))))
  (forward-char 1)
  (skip-chars-forward "^<]/"))

(defun sgml-setup-dtd ()
  (sgml-set-doctype sgml-any)		; fall back DTD
  (cond ((and sgml-default-dtd-file
	      (file-exists-p sgml-default-dtd-file))
	 (sgml-load-dtd sgml-default-dtd-file))
	((null sgml-parent-document)
	 (sgml-parse-prolog))
	(t				; get DTD from parent document
	 (save-excursion
	   (set-buffer (find-file-noselect
			(if (listp sgml-parent-document)
			    (car sgml-parent-document)
			  sgml-parent-document)))
	   (sgml-need-dtd)
	   (sgml-set-global))
	 (sgml-set-local)
	 (if (listp sgml-parent-document)
	     (let ((doctypename (second sgml-parent-document)))
	       (if (symbolp doctypename)
		   (setq doctypename (symbol-name doctypename)))
	       (sgml-set-doctype-element
		(sgml-gname-symbol doctypename)))))))

(defun sgml-parse-to (sgml-goal)
  (when (null sgml-top-tree)		; first parse in this buffer
    (sgml-setup-dtd))
  (unless (and (boundp 'pre-command-hook)
	       (not (null pre-command-hook)))
    (make-local-variable 'pre-command-hook)
    (setq pre-command-hook '(sgml-reset-log)))
  (sgml-find-start-point (min sgml-goal (point-max)))
  (assert sgml-current-tree)
  (let ((bigparse (> (- sgml-goal (point)) 10000))
	(sgml-param-entities sgml-buffer-param-entities))
    (when bigparse
      (sgml-message "Parsing..."))
    (sgml-with-parser-syntax
     (sgml-parser-loop))
    (when bigparse
      (sgml-message ""))))

(defun sgml-parser-loop ()
  (while (< (point) sgml-goal)
    (assert sgml-current-tree)
    (setq sgml-markup-start (point)
	  sgml-markup-type nil)
    (cond
     ((and (not (sgml-current-mixed-p)) 	(sgml-skip-s)))
     ((and (or (eq sgml-current-state sgml-cdata)
	       (eq sgml-current-state sgml-rcdata))
	   (not (sgml-is-end-tag)))
      (sgml-skip-cdata))
     ((sgml-parse-char ?<)		; Markup?
      (cond
       ((and (sgml-parse-char ?/)
	     (or (sgml-startnm-char-next)
		 (eq ?> (following-char)))) ; empty end tag
	(sgml-parse-end-tag))
       ((or (sgml-startnm-char-next)
	    (eq ?> (following-char)))	; empty start tag
	(sgml-parse-start-tag))
       ((memq (following-char) '(?! ??))
	(forward-char -1)
	(or (and (eq sgml-current-tree sgml-top-tree)
		 (eq sgml-current-state (sgml-element-model sgml-top-tree))
		 (or (sgml-skip-sgml-dcl)
		     (sgml-skip-doctype)))
	    (sgml-skip-other-content)))
       (t
	(sgml-do-pcdata))))
     ((and (eq ?/ (following-char))
	   (sgml-tree-net-enabled sgml-current-tree))
      (sgml-parse-end-tag))
     ((sgml-parse-marked-section-end)	; end of marked section
      (setq sgml-markup-type 'ms-end))
     (t
      (sgml-do-pcdata)))))

(defun sgml-find-start-point (goal)
  (let ((u sgml-top-tree))
    (while
	(cond
	 ((and (sgml-tree-next u)
	       (> goal (sgml-tree-start (sgml-tree-next u))))
	  (setq u (sgml-tree-next u)))
	 ((and (sgml-tree-end u)
	       (if (> (sgml-tree-etag-len u) 0) ; if threre is an end tag
		   (>= goal (sgml-tree-end u))  ; precisely after is after
		 (> goal (sgml-tree-end u))))   ; else it could possibly
					      ; becom part of the element
	  (sgml-set-parse-state u 'after)
	  nil)
	 ((and (sgml-tree-content u)
	       (> goal (sgml-tree-start (sgml-tree-content u))))
	  (setq u (sgml-tree-content u)))
	 (t
	  (sgml-set-parse-state u 'start)
	  nil)))))

(defun sgml-parse-start-tag ()    
  ;; Assume point after <
  (setq sgml-markup-type 'start-tag)
  (let* ((gi
	  (if (eq ?> (following-char))	; empty start tag
	      (if t ;sgml-omittag ; if omittag use current open element
		  ;; *** if not omittag how does it work??
		  (if (eq sgml-current-tree sgml-top-tree)
		      sgml-document-element ; or document element if
					; no element is open
		    (sgml-element-name sgml-current-tree))
		(sgml-last-closed-element))
	    (sgml-check-name)))
	 temp net-enabled)
    (unless (sgml-parse-char ?>)	; optimize common case
      (unless (search-forward-regexp
	       "\\([^\"'<>/]\\|\"[^\"]*\"\\|'[^']*'\\)*"
	       nil t)
	(sgml-error "Invalid start tag"))
      (or (sgml-parse-char ?>)
	  (eq ?< (following-char))
	  (if (sgml-parse-char ?/)
	      (setq net-enabled t))
	  (sgml-parse-error "Invalid character in markup %c"
			    (following-char))))
    (while				; Until token accepted
	(cond
	 ((memq gi (sgml-excludes))
	  (sgml-log-warning "Excluded element %s" gi)
	  nil)
	 ((eq sgml-current-state sgml-any) nil)
	 ((memq gi (sgml-includes)) nil)
	 ((setq temp (sgml-get-move sgml-current-state gi))
	  (setq sgml-current-state temp)
	  nil)
	 ((sgml-do-implied (format "%s start tag" gi)))))
    (sgml-open-element gi sgml-markup-start (point))
    (when net-enabled
      (setf (sgml-tree-net-enabled sgml-current-tree) t))))

;(defun sgml-last-closed-element ()
;  "Return the GI of the last closed element."
;  (let ((u sgml-current-tree))))

(defun sgml-parse-end-tag ()
  "Assume point after </ or at / in a NET"
  (setq sgml-markup-type 'end-tag)
  (let ((gi (if (or (sgml-parse-char ?>) ; empty end tag
		    (sgml-parse-char ?/)) ; net
		(sgml-element-name sgml-current-tree)
	      (prog1 (sgml-check-name)
		(or (eq ?< (following-char))	;unclosed end tag
		    (sgml-check-char ?>))))))
    (while
	(progn
	  (unless (sgml-final-p sgml-current-state)
	    (sgml-log-warning
	     "%s element can't end here, need one of %s; %s end tag out of context"
	     (sgml-element-name sgml-current-tree)
	     (sgml-required-tokens sgml-current-state)
	     gi))
	  (when (eq sgml-current-tree sgml-top-tree)
	    (sgml-error
	     "%s end tag ended document and parse" gi))
	  (not (eq gi (sgml-element-name sgml-current-tree))))
      (sgml-implied-end-tag (format "%s end tag" gi)
			    sgml-markup-start sgml-markup-start))
    (sgml-close-element sgml-markup-start (point))))

(defun sgml-do-implied (type &optional temp)
  (cond
   ((sgml-final-p sgml-current-state)
    (sgml-implied-end-tag type sgml-markup-start sgml-markup-start)
    t)
   ((and (setq temp (sgml-required-tokens sgml-current-state))
	 (null (cdr temp)))
    (setq sgml-current-state
	  (sgml-get-move sgml-current-state (car temp)))
    (sgml-open-element (car temp) sgml-markup-start sgml-markup-start)
    (unless (sgml-element-stag-optional sgml-current-tree)
      (sgml-log-warning
       "%s start tag implied by %s; not minimizable"
       (car temp) type))
    t)
   (t (sgml-log-warning "%s out of context" type)
      nil)))

(defun sgml-implied-end-tag (type start end)
  (cond ((eq sgml-current-tree sgml-top-tree)
	 (unless (eobp)
	   (sgml-error
	    "document ended by %s" type)))
	((not (sgml-element-etag-optional sgml-current-tree))
	 (sgml-log-warning
	  "%s end tag implied by %s; not minimizable"
	  (sgml-element-name sgml-current-tree)
	  type)))
  (sgml-close-element start end))

(defun sgml-skip-other-content ()
  (cond ((sgml-skip-comment-declaration))
	((sgml-skip-processing-instruction))
;;;                      | 152 short reference use declaration
;;;                      | 169+ link set use declaration
;;;                      | SHORTREF
;;;                      | 62 character reference  ***
;;;                      | 59+ general entity reference
	((sgml-parse-marked-section-start) ; | 93 marked section declaration
	 (sgml-parse-marked-section)
	 t)
;;;                      | EE
	(t (sgml-parse-error "Unrecognized markup"))))


(defun sgml-note-change-at (at &optional end)
  ;; Inform the cache that there have been some changes after AT
  (let ((u sgml-top-tree))
    (when u
      ;;(message "%d" at)
      (while
	  (cond
	   ((and (sgml-tree-next u)	; Change clearly in next element
		 (> at (sgml-element-stag-end (sgml-tree-next u))))
	    (setq u (sgml-tree-next u)))
	   (t				; 
	    (setf (sgml-tree-next u) nil) ; Forget next element
	    (cond 
	     ;; If change after this element and it is ended by an end
	     ;; tag pruning is done.  If the end of the element is
	     ;; implied changing the tag that implied it may change
	     ;; the extent of the element.
	     ((and (sgml-tree-end u)	
		   (> at (sgml-tree-end u))
		   (or (> (sgml-tree-etag-len u) 0)
		       (sgml-element-empty u)))
	      nil) 
	     (t
	      (setf (sgml-tree-end u) nil)
	      (cond ((and (sgml-tree-content u)
			  (> at (sgml-element-stag-end (sgml-tree-content u))))
		     (setq u (sgml-tree-content u)))
		    (t
		     (setf (sgml-tree-content u) nil)))))))))))


;;;; Parsing tasks and extending the element view of parse tree

(defun sgml-find-context-of (pos)
  "Find the parser context for POS, returns the parse tree.
Also sets sgml-current-tree, sgml-current-state and point."
  (sgml-parse-to pos)
  (cond ((and (> (point) pos)
	      sgml-markup-type)
	 (setq sgml-current-state sgml-markup-type)
	 (cond ((memq sgml-markup-type '(start-tag end-tag))
		(setq sgml-current-tree sgml-markup-tree)))))
  sgml-current-tree)

(defun sgml-parse-to-here ()
  (save-excursion
   (sgml-find-context-of (point))))

(defun sgml-find-element-of (pos)
  "Find the element containing character at POS."
  (when (eq pos (point-max))
    (error "End of buffer"))
  (save-excursion
    (sgml-parse-to (1+ pos))		; Ensures that the element is
					; in the tree.
    ;;  Find p in u:
    ;;  assert p >= start(u)
    ;;  if next(u) and p >= start(next(u)): find p in next(u)
    ;;  else if end(u) and p >= end(u): in parent(u) unless u is top
    ;;  else if content:
    ;;    if p < start(content(u)): in u
    ;;    else find p in content(u)
    ;;  else: in u
    (let ((u sgml-top-tree))
      (while				; pos >= start(u)
	  (cond ((and (sgml-tree-next u)
		      (>= pos (sgml-tree-start (sgml-tree-next u))))
		 (setq u (sgml-tree-next u))) ; continue searching next node
		((and (sgml-tree-end u)
		      (>= pos (sgml-tree-end u)))
		 (setq u (sgml-tree-parent u)) ; must be parent node
		 nil)
		((and (sgml-tree-content u)
		      (>= pos (sgml-tree-start (sgml-tree-content u))))
		 (setq u (sgml-tree-content u))))) ; search content
      u)))

(defun sgml-find-previous-element (pos &optional noerror)
  (save-excursion
    ;;   Parse to point; now the previous element is in the parse tree.
    (sgml-parse-to pos)
    (let* ((u sgml-current-tree)	; Let u be the containing element.
	   (c (sgml-tree-content u)))	; Let c be content(u).
      (while
	  (cond
	   ((null c)			; If c = Nil: no previous element.
	    ;; But maybe the containing element ends at pos too.
	    (cond ((= pos (sgml-element-end u))
		   (setq c u)))		; Previos is parent!
	    nil)
	   ((<= pos (sgml-tree-start c))	; Pos before first content el
	    (setq c nil))		; No, previous element.
	   ((null (sgml-tree-next c)) nil) ; No next, c must be the prev el
	   ((>= (sgml-tree-start (sgml-tree-next c)) pos)
	    nil)
	   (t
	    (setq c (sgml-tree-next c)))))
      (when (and (null c) (not noerror))
	(error "No previous element in %s element."
	       (sgml-element-name u)))
      c)))

(defun sgml-find-element-after (pos)
  "Find the first element starting after POS.
Returns parse tree; error if no element after POS."
  (save-excursion
    (let ((next (sgml-find-element-of pos)))
      (while (and next			; while next is actually parent
		  (or (< (sgml-tree-start next) pos)
		      (zerop (sgml-tree-stag-len next))))
	(when (and (null (sgml-tree-content next)) ; make sure that if this element
		   (null (sgml-tree-end next))) ; has some content the first element
	  (sgml-parse-until-end-of t))	; of the content has been parsed
	(setq next (sgml-tree-content next))
	(while (and next (< (sgml-tree-start next) pos)) ; search content
	  (setq next (sgml-element-next next))))
      (unless next
	(sgml-message "")		; force display of log buffer
	(error "No more elements."))
      next)))

(defun sgml-element-content (element)
  "First element in content of ELEMENT, or nil."
  (when (null (or (sgml-tree-content element)
		  (sgml-tree-end element)))
    (save-excursion (sgml-parse-until-end-of t)))
  (sgml-tree-content element))

(defun sgml-element-next (element)
  (unless (sgml-tree-end element)
    (save-excursion (sgml-parse-until-end-of element)))
  (unless (or (sgml-tree-next element)
	      (sgml-tree-end (sgml-tree-parent element)))
    (save-excursion (sgml-parse-until-end-of t)))
  (sgml-tree-next element))

(defun sgml-element-end (element)
  (unless (sgml-tree-end element)
    (save-excursion
      (sgml-parse-until-end-of element)))
  (assert (sgml-tree-end element))
  (sgml-tree-end element))

(defun sgml-element-etag-start (element)
  (- (sgml-element-end element)
     (sgml-tree-etag-len element)))

(defun sgml-read-element-name (prompt)
  (sgml-parse-to-here)
  (cond ((and sgml-buffer-element-map
	      (not (eq sgml-current-state sgml-any)))
	 (let ((tab
		(mapcar (lambda (x) (cons (symbol-name x) nil))
			(sgml-current-list-of-valid-elements))))
	   (cond ((null tab)
		  (error "No element valid at this point"))
		 (t
		  (completing-read prompt tab nil t
				   (and (null (cdr tab)) (caar tab)))))))
	(t
	 (read-from-minibuffer prompt))))

(defun sgml-map-content (element element-fun &optional data-fun pi-fun)
  "Map content of ELEMENT, calling ELEMENT-FUN for every element.
Also calling DATA-FUN, if non-nil, with data in content."
  (let ((c (sgml-element-content element))
	(last-pos (sgml-element-stag-end element)))
    (save-excursion
      (goto-char last-pos)
      (when (eolp)
	(setq last-pos (1+ last-pos))))
    (while c
      ;; Any data before first element in content
      (when (and data-fun
		 (sgml-element-mixed element))
	(sgml-map-data (buffer-substring last-pos (sgml-element-start c))
		       data-fun pi-fun))
      ;; Do the element
      (when element-fun
	(funcall element-fun c))
      (setq last-pos (sgml-element-end c))
      ;; Next element
      (setq c (sgml-element-next c)))
    ;; Any data after last element
    (when (and data-fun
	       (sgml-element-data-p element))
      (sgml-map-data (buffer-substring last-pos
				       (sgml-element-etag-start element))
		     data-fun
		     pi-fun))))

(defun sgml-map-data (data data-fun pi-fun)
  (when (not (equal data ""))
    (funcall data-fun data)))

;;;; SGML mode: structure editing

(defun sgml-beginning-of-element ()
  "Move to after the start tag of the current element.
If the start tag is implied, move to the start of the element."
  (interactive)
  (goto-char (sgml-element-stag-end (sgml-find-context-of (point)))))

(defun sgml-end-of-element ()
  "Move to before the end tag of the current element."
  (interactive)
  (goto-char (sgml-element-etag-start (sgml-find-context-of (point)))))

(defun sgml-backward-up-element ()
  "Move backward out of this element level.
That is move to before the start tag or where a start tag is implied."
  (interactive)
  (goto-char (sgml-element-start (sgml-find-context-of (point)))))

(defun sgml-up-element ()
  "Move forward out of this element level.
That is move to after the end tag or where an end tag is implied."
  (interactive)
  (goto-char (sgml-element-end (sgml-find-context-of (point)))))

(defun sgml-forward-element ()
  "Move forward over next element."
  (interactive)
  (goto-char (sgml-element-end (sgml-find-element-after (point)))))

(defun sgml-backward-element ()
  "Move backward over previous element at this level.
With implied tags this is ambigous."
  (interactive)
  (goto-char (sgml-element-start (sgml-find-previous-element (point)))))

(defun sgml-down-element ()
  "Move forward and down one level in the element structure."
  (interactive)
  (goto-char (sgml-element-stag-end (sgml-find-element-after (point)))))

(defun sgml-kill-element ()
  "Kill the element following the cursor."
  (interactive)
  (let ((element (sgml-find-element-after (point))))
    (goto-char (sgml-element-start element))
    (kill-region (sgml-element-start element)
		 (sgml-element-end element))))

(defun sgml-transpose-element ()
  "Interchange element before point with element after point, leave point after."
  (interactive)
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
  "Set mark at end of current element, and leave point before current element."
  (interactive)
  (let ((el (sgml-find-element-of (point))))
    (set-mark (sgml-element-end el))
    (goto-char (sgml-element-start el)))
  (sgml-message ""))

(defun sgml-change-element-name (gi)
  "Replace the name (generic identifyer) of the current element with a new name."
  (interactive
   (list
    (let ((el (sgml-find-element-of (point))))
      (goto-char (sgml-element-start el))
      (sgml-read-element-name
       (format "Change %s to: " (sgml-element-name el))))))
  (when (or (null gi)
	    (equal gi ""))
    (error "Illegal name"))
  (let ((element (sgml-find-element-of (point))))
    (goto-char (sgml-element-end element))
    (delete-char (- (sgml-element-etag-len element)))
    (insert (sgml-end-tag-of gi))
    (goto-char (sgml-element-start element))
    (delete-char (sgml-element-stag-len element))
    (insert (sgml-start-tag-of gi))))


;;;; SGML mode: folding

(defun sgml-fold-region (beg end &optional unhide)
  "Hide (or if prefixarg unhide) region.
If called from a program first two arguments are start and end of
region. And optional third argument true unhides."
  (interactive "r\nP")
  (let ((mp (buffer-modified-p))
	(buffer-read-only nil)
	(before-change-function nil))
    (setq selective-display t)
    (unwind-protect
	(subst-char-in-region beg end
			      (if unhide ?\r ?\n)
			      (if unhide ?\n ?\r)
			      'noundo)
      (set-buffer-modified-p mp))))

(defun sgml-fold-element ()
  "Fold the lines comprising the current element, leaving the first line visible.
This uses the selective display feature."
  (interactive)
  (sgml-parse-to-here)
  (cond ((or (looking-at "<!sgml")
	     (eq sgml-markup-type 'sgml))
	 (sgml-fold-region (point)
			   (save-excursion
			     (sgml-skip-markup-declaration)
			     (point))))
	(t
	 (let ((el (sgml-find-element-of (point))))
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
	(setq element (sgml-find-element-of (point))))
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
  (let ((sgml-throw-on-warning 'next-data))
    (catch sgml-throw-on-warning
      (while (and (progn (skip-chars-forward "^<")
			 (not (eobp)))
		  (progn (sgml-parse-to (1+ (point)))
			 (not (sgml-current-element-contains-data)))))
      (sgml-message "%s" (sgml-current-element-content-class)))))

(defun sgml-next-trouble-spot ()
  "Move forward to next point where something is amiss with the structure."
  (interactive)
  (sgml-parse-to-here)
  (push-mark)
  (sgml-note-change-at (point))		; Prune the parse tree
  (let ((sgml-throw-on-warning 'trouble))
    (or (catch sgml-throw-on-warning
	  (sgml-parse-until-end-of nil))
	(message "Ok"))))



;;;; SGML mode: information display

(defun sgml-list-valid-tags ()
  "Display a list of the contextually valid tags."
  (interactive)
  (sgml-parse-to-here)
  (with-output-to-temp-buffer "*Tags*"
    (when (eq sgml-current-state sgml-any)
      (princ (sgml-current-element-content-class))
      (terpri))
    (let ((l (sgml-current-list-of-valid-tags)))
      (while l
	(princ (car l))
	(terpri)
	(setq l (cdr l))))))

(defun sgml-show-context ()
  "Display where the cursor is in the element hierarchy."
  (interactive)
  (sgml-parse-to-here)
  (sgml-message "%s" (sgml-context-as-string)))

(defun sgml-what-element ()
  "Display what element is under the cursor."
  (interactive)
  (let ((el (sgml-find-element-of (point))))
    (assert (not (null el)))
    (message "%s %s"
	     (cond ((eq el sgml-top-tree)
		    "outside document element")
		   ((< (point) (sgml-element-stag-end el))
		    "start tag")
		   ((>= (point) (sgml-element-etag-start el))
		    "end tag")
		   (t
		    "content"))
	     (sgml-element-context-string el))))

;;;; SGML mode: inserting

(defun sgml-completion-table (&optional avoid-tags-in-cdata)
  (sgml-parse-to-here)
  (cond ((or (sgml-model-group-p sgml-current-state)
	     (eq sgml-current-state sgml-any))
	 (mapcar (lambda (x) (cons x x)) (sgml-current-list-of-valid-tags)))
	(t
	 (sgml-message "%s" sgml-current-state)
	 nil)))

(defun sgml-name-of (tag)
  "Name of tag"
  (substring tag
	     (if (eq (aref tag 1) ?/) 2 1)
	     -1))

(defun sgml-insert-end-tag ()
  "Insert end tag for the current open element."
  (interactive)
  (sgml-parse-to-here)
  (cond
   ((eq sgml-current-tree sgml-top-tree)
    (sgml-error "No open element"))
   ((not (sgml-final-p sgml-current-state))
    (sgml-error "Can`t end element here"))
   (t
    (when (and sgml-indent-step
	       (not (sgml-current-element-contains-data)))
      (delete-horizontal-space)
      (unless (bolp)
	(insert "\n")))
    (when (prog1 (bolp)
	    (insert (if (sgml-element-net-enabled sgml-current-tree)
			"/"
		      (sgml-end-tag-of sgml-current-tree))))
      (sgml-indent-line)))))

(defun sgml-insert-tag ()
  "Insert a tag, reading tag name in minibuffer with completion.
If the variable sgml-balanced-tag-edit is t, also inserts the
corresponding end tag. If sgml-leave-point-after-insert is t, the point
is left after the inserted tag(s), unless the element has som required
content.  If sgml-leave-point-after-insert is nil the point is left
after the first tag inserted."
  (interactive)
  (sgml-tags-menu nil t))

(defun sgml-tags-menu (event &optional nomenu)
  "Pop up a menu with valid tags and insert the choosen tag.
If the variable sgml-balanced-tag-edit is t, also inserts the
corresponding end tag. If sgml-leave-point-after-insert is t, the point
is left after the inserted tag(s), unless the element has som required
content.  If sgml-leave-point-after-insert is nil the point is left
after the first tag inserted."
  (interactive "e")
  (or event (setq event (list '(0 0) (selected-frame))))
  (let (start end)
    (cond				;*** can I use sgml-mouse-region ?
     ((and (or sgml-tag-region-if-active
	       transient-mark-mode)
	   mark-active)
      (setq start (region-beginning)
	    end (region-end)))
     ((and mouse-secondary-overlay
	   (eq (current-buffer)
	       (overlay-buffer mouse-secondary-overlay)))
      (setq start (overlay-start mouse-secondary-overlay)
	    end (overlay-end mouse-secondary-overlay))
      (delete-overlay mouse-secondary-overlay)))
    (when start
      (goto-char start))
    (sgml-ask-and-insert-tags event nomenu start end)))

(defun sgml-ask-and-insert-tags (event &optional nomenu start end)
  (let* ((tab (sgml-completion-table))
	 (data (sgml-current-element-contains-data))
	 
	 tag				; tag to insert
	 element			; the element of the inserted tag
	 )
    (cond ((null tab)			;Nothing to insert?
	   (error "No tags available"))
	  ;;((null (cdr tab))		;Only one
	  ;; (setq tag (caar tab)))
	  (nomenu
	   (setq tag (completing-read "Tag: " tab nil t "<" )))
	  (t
	   (setq tag (x-popup-menu event
				   (sgml-split-menu "Tags" tab)))))
    (unless tag
      (error "Menu abort"))
    (cond
     ((and start end)
      (sgml-tag-region (sgml-name-of tag) start end))
     (sgml-balanced-tag-edit
      (sgml-insert-element (sgml-name-of tag)))
     (t
      (setq element (sgml-lookup-element (sgml-name-of tag)))
      (unless (or data (bolp)) (insert "\n"))
      (unless (or data (looking-at "\\s-*$")) (open-line 1))
      (when (and (prog1 (bolp)
		   (insert tag))
		 (if data sgml-indent-data t))
	(sgml-indent-line))))))

(defun sgml-insert-element (name &optional after silent)
  "Reads element name from minibuffer and inserts start and end tags."
  (interactive
   (list
    (sgml-read-element-name "Element: ")
    sgml-leave-point-after-insert))
  (let ((inel (sgml-parse-to-here))	; element inserting in
	(data (sgml-current-element-contains-data))
	stag-end element
	col)
    (when (and name (not (equal name "")))
      (cond ((and sgml-indent-data data
		  (save-excursion
		    (skip-chars-backward " \t")
		    (and (bolp)
			 (looking-at "\\s-*$"))))
	     (delete-horizontal-space))
	    ((not data)
	     (skip-chars-backward " \t")
	     (unless (looking-at "\\s-*$") (open-line 1))
	     (delete-horizontal-space)
	     (unless (bolp) (insert "\n"))
	     ))
      (when (prog1 (bolp)
	      (insert (format "<%s>" name)))
	(setq col (sgml-indent-line)))
      (setq stag-end (point))
      (setq element (sgml-find-element-of (1- (point))))
      (when (not (sgml-element-empty element))
	(when (and sgml-auto-insert-required-elements
		   (sgml-model-group-p sgml-current-state))
	  (let (tem
		newpos
		(sgml-suppress-warning t))
	    (while (and (setq tem (sgml-required-tokens sgml-current-state))
			(null (cdr tem)))
	      (setq tem (sgml-insert-element (car tem) t t))
	      (setq newpos (or newpos tem))
	      (sgml-parse-to-here))
	    (when tem			; more than one req elem
	      (insert (format "\n<!-- one of %s -->" tem))
	      (sgml-indent-line nil element))
	    (if newpos (setq stag-end newpos))))
	(when (and col (not (sgml-element-data-p element)))
	  (insert "\n")
	  (sgml-indent-line col))
	(insert (format "</%s>" name))
	(unless after (goto-char stag-end))
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
      (insert (sgml-start-tag-of element)))))

(defun sgml-entities-menu (event)
  (interactive "e")
  (sgml-need-dtd)
  (let ((menu (mapcar (lambda (x)
			(cons x x))
		      sgml-buffer-entities))
	choice)
    (unless menu
      (error "No entities defined"))
    (setq choice (x-popup-menu event
			       (sgml-split-menu "Entities" menu)))
    (when choice
      (insert "&" choice ";"))))

(defun sgml-split-menu (title menu)
  (let ((menus (list (cons title menu))))
    (cond ((> (length menu)
	    sgml-max-menu-size)
	   (setq menus
		 (loop for i from 1 while menu
		 collect
		 (prog1 (cons
			 (format "%s %d" title i)
			 (subseq menu 0 (min (length menu)
					    sgml-max-menu-size)))
		   (setq menu (nthcdr sgml-max-menu-size menu)))))))
    (cons title menus)))

;;;; SGML mode: Fill 

(defun sgml-fill-element (element)
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
  (cond
   ((sgml-element-mixed element)
    (let ((last-pos (sgml-element-stag-end element))
	  (c (sgml-element-content element))
	  (agenda nil))			; regions to fill later
      (while c
	(cond
	 ((sgml-element-mixed c))
	 (t
	  ;; Put region before element on agenda.  Can't fill it now
	  ;; that would mangel the parse tree that is beeing traversed.
	  (push (cons last-pos (sgml-element-start c))
		agenda)
	  (setq last-pos (sgml-element-end c))
	  (sgml-fill-element c)))
	(setq c (sgml-element-next c)))
      (sgml-fill-region last-pos (sgml-element-etag-start element))
      (while agenda
	(sgml-fill-region (caar agenda) (cdar agenda))
	(setq agenda (cdr agenda)))))
   (t
    ;; If element is not mixed, fill subelements recursively
    (sgml-map-content element
		      (function sgml-fill-element)))))

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
	(delete-region (point) opoint)
	(newline)
	(sgml-indent-line)
	(setq give-up (>= (current-column) prev-column))
	(end-of-line 1)))))


;;;; SGML mode: attributes

(defvar sgml-start-attributes nil)
(defvar sgml-end-attributes nil)
(defvar sgml-main-buffer nil)
(defvar sgml-attlist nil)

(defun sgml-edit-attributes ()
  "Edit attributes of current element.
Editing is done in a separate window."
  (interactive)
  (let* ((element (sgml-find-element-of (point)))
	 (tag-beg (sgml-element-start element)))
    (push-mark)
    (goto-char (1+ tag-beg))
    (sgml-with-parser-syntax
     (sgml-check-name)
     (let* ((start (point-marker))
	    (asl (sgml-parse-attribute-specification-list element))
	    (end (point-marker))
	    (cb (current-buffer))
	    (quote sgml-always-quote-attributes))
       (switch-to-buffer-other-window
	(sgml-attribute-buffer element asl))
       (sgml-edit-attrib-mode)
       (make-local-variable 'sgml-attlist)
       (setq sgml-attlist (sgml-element-attlist element))
       (make-local-variable 'sgml-start-attributes)
       (setq sgml-start-attributes start)
       (make-local-variable 'sgml-end-attributes)
       (setq sgml-end-attributes end)
       (make-local-variable 'sgml-always-quote-attributes)
       (setq sgml-always-quote-attributes quote)
       (make-local-variable 'sgml-main-buffer)
       (setq sgml-main-buffer cb)))))

(defun sgml-insert (props format &rest args)
  (let ((start (point)))
    (insert (apply (function format)
		   format
		   args))
    (add-text-properties start (point) props)))

(defun sgml-attribute-buffer (element asl)
  (let ((buf (get-buffer-create "*Edit attributes*"))
	(inhibit-read-only t))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (sgml-insert '(read-only t)
		   "<%s  -- Edit values and finish with C-c C-c --\n>"
		   (sgml-element-name element))
      (forward-char -1)
      (loop
       for attr in (sgml-element-attlist element) do
       ;; Produce text like
       ;;  name = value
       ;;  -- declaration : default --
       (let* ((aname (sgml-attribute-name attr))
	      (dcl-value (sgml-attribute-declared-value attr))
	      (def-value (sgml-attribute-default-value attr))
	      (cur-value (assq aname asl)))
	 (sgml-insert			; atribute name
	  '(read-only t rear-nonsticky (read-only))
	  " %s = " aname)
	 (cond				; attribute value
	  ((and (consp def-value)
		(eq (car def-value) 'fixed))
	   (sgml-insert '(read-only t category sgml-fixed)
			"#FIXED %s" (cadr def-value)))
	  ((and (null cur-value)
		(or (memq def-value '(implied conref current))
		    (consp def-value)))
	   (sgml-insert '(category sgml-default) "#DEFAULT"))
	  ((not (null cur-value))
	   (sgml-insert nil "%s" (cadr cur-value))))
	 (sgml-insert
	  '(read-only 1)
	  "\n\t-- %s: %s --\n"
	  (if (consp dcl-value)		; Declared value
	      (if (eq (car dcl-value) 'notation)
		  (format "NOTATION %s" (cadr dcl-value))
		(cadr dcl-value))
	    dcl-value)
	  (if (consp def-value)		; Default value
	      (cadr def-value)
	    (concat "#" (upcase (symbol-name def-value)))))))
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
	(avl (sgml-attribute-value-list))
	;; save buffer local variables
	(start sgml-start-attributes)
	(end sgml-end-attributes)
	(attlist sgml-attlist))
    (when (markerp start)
      (delete-windows-on cb)
      (switch-to-buffer (marker-buffer start))
      (kill-buffer cb)
      (goto-char start)
      (unless (= start end)
	(kill-region start end))
      (let (name val def tem)
	(while avl
	  (setq name (caar avl)
		val (cdar avl)
		def (assq name attlist)
		avl (cdr avl))
	  (cond ((and def
		      (eq sgml-minimize-attributes 'max)
		      (if (listp (setq tem (sgml-attribute-default-value def)))
			  (if (symbolp (setq tem (cadr tem)))
			      (eq tem (intern val))
			    (equal val tem)))))
		((and def
		      sgml-minimize-attributes
		      (listp (setq tem (sgml-attribute-declared-value def)))
		      (eq 'name-token-group (car tem))
		      (if (memq (sgml-gname-symbol val) (cadr tem))
			  (progn (insert " " val) t)
			(sgml-log-warning
			 "%s is not an allowed value for attribute %s"
			 val name)
			nil)))
		(t
		 (insert " " (symbol-name name) "="
			 (sgml-quote-attribute-value val)))))))))

(defun sgml-attribute-value-list ()
  (goto-char (point-min))
  (forward-line 1)
  (sgml-with-parser-syntax
   (let ((avl nil)
	 (al sgml-attlist))
     (while (not (eq ?> (following-char)))
       (sgml-skip-s)
       (let ((name (sgml-check-nametoken)))
	 (forward-char 3)
	 (unless (memq (get-text-property (point) 'category)
		       '(sgml-default sgml-fixed))
	   (setq avl
		 (cons (cons name
			     (sgml-extract-attribute-value
			      (sgml-attribute-declared-value (car al))))
			   avl)))
	 (while (progn (beginning-of-line 2)
		       (not (eq t (get-text-property (point) 'read-only))))))
       (setq al (cdr al)))
     (reverse avl))))

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

(defun sgml-quote-attribute-value (value)
  (let ((quote ""))
	(cond ((and (not sgml-always-quote-attributes)
		    (string-match "^[.A-Za-z0-9---]+$" value))
	       ) ; no need to quote
	      ((not (string-match "\"" value)) ; can use "" quotes
	       (setq quote "\""))
	      (t			; use '' quotes
	       (setq quote "'")))
	(concat quote value quote)))

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
  (let ((end (if (eq 1 (get-text-property (point) 'read-only))
		 (point)
	       (next-single-property-change (point) 'read-only))))
    (assert (number-or-marker-p end))
    (goto-char end)))

(defun sgml-edit-attrib-next ()
  "Move to next attribute value."
  (interactive)
  (or (search-forward-regexp "^ *[.A-Za-z0-9---]+ *= ?" nil t)
      (goto-char (point-min))))


;;; Hiding attributes

(defun sgml-identify-attributes (prop val)
  (remove-text-properties (point-min) (point-max) '(category nil))
  (let ((mp (buffer-modified-p))
	(buffer-read-only nil)
	(before-change-function nil))
    (unwind-protect
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward
		  "<[A-Za-z][---A-Za-z0-9\.]*\\([^>]*\\)>"
		  nil t)
	    (put-text-property (match-beginning 1) (match-end 1)
			       prop val)))
      (set-buffer-modified-p mp))))

(defun sgml-hide-attributes ()
  (interactive)
  (sgml-identify-attributes 'invisible t))

(defun sgml-show-attributes ()
  (interactive)
  (sgml-identify-attributes 'invisible nil))

;;;; SGML mode: Normalize

(defun sgml-normalize (&optional element)
  "Normalize buffer by filling in omitted tags and expanding empty tags.
A  optional argument ELEMENT can be the element to normalize
insted of the whole buffer."
  (interactive)
  (let ((only-one (not (null element))))
    (setq element (or element (sgml-top-element)))
    (goto-char (sgml-element-end element)) 
    (let ((before-change-function nil))
      (sgml-normalize-content element only-one)))
  (sgml-note-change-at (sgml-element-start element))
  (sgml-message "Done"))

(defun sgml-top-element ()
  (sgml-element-content (sgml-find-context-of (point-min))))

(defun sgml-normalize-element ()
  (interactive)
  (sgml-normalize (sgml-find-element-of (point))))

(defun sgml-normalize-content (element only-first)
  "Normalize all elements in a content where ELEMENT is first element.
If sgml-normalize-trims is non-nil, trim off white space from ends of
elements with omitted end tags."
  (let ((content nil))
    (while element
      (push element content)
      (setq element (if only-first
			nil
		      (sgml-element-next element))))
    (while content
      (setq element (car content))
      ;; Progress report
      (message "Normalizing %d%% left"
	       (/ (* 100.0 (point)) (point-max)))
      ;; Fix the end tag
      (unless (sgml-element-empty element)
	(when (or (sgml-tree-net-enabled element)
		  (zerop (sgml-tree-etag-len element))
		  (= 3 (sgml-tree-etag-len element)))
	  (goto-char (min (point) (sgml-element-etag-start element)))
	  (if (zerop (sgml-element-etag-len element))
	      (if sgml-normalize-trims
		  (skip-chars-backward " \t\n\r"))
	    (delete-char (sgml-tree-etag-len element)))
	  (when (not (sgml-element-data-p element))
	    (unless (bolp) (insert "\n"))
	    (unless (eolp)
	      (open-line 1)))
	  (save-excursion
	    (insert (sgml-end-tag-of (sgml-element-name element))))
	  (when (not (sgml-element-data-p element))
	    (sgml-indent-line nil element)
	    (beginning-of-line 1))))
      ;; Fix tags of content
      (sgml-normalize-content (sgml-tree-content element) nil)
      ;; Fix the start tag
      (goto-char (min (point) (sgml-tree-start element)))
      (cond ((or (zerop (sgml-tree-stag-len element))
		 (= 2 (sgml-tree-stag-len element)))
	     (save-excursion
	       (delete-char (sgml-tree-stag-len element))
	       (insert (sgml-start-tag-of (sgml-tree-element element)))))
	    (t
	     (forward-char (1- (sgml-tree-stag-len element)))
	     (cond ((looking-at ">"))
		   (t
		    (when (looking-at "/")
		      (delete-char 1))
		    (save-excursion (insert ">"))))))      
      ;; Next content element
      (setq content (cdr content)))))


;;;; SGML mode: TAB completion

(defun sgml-complete ()
  (interactive)
  (let ((tab nil)
	(pattern nil)
	(c nil)
	(here (point)))
    (skip-chars-backward "^ \n\t</!&%")
    (setq pattern (buffer-substring (point) here))
    (setq c (char-after (1- (point))))
    (cond ((eq c ?&)			; entitiy
	   (setq tab sgml-buffer-entities))
	  ((eq c ?<)			; start tag
	   (save-excursion
	     (backward-char 1)
	     (sgml-parse-to-here)
	     (setq tab (sgml-current-list-of-valid-elements))))
	  ((eq c ?/)			; end tag
	   (save-excursion
	     (backward-char 2)
	     (sgml-parse-to-here)
	     (setq tab (sgml-current-list-of-endable-elements))))
	  ((eq c ?!)
	   (setq tab '("entity" "element" "attlist" "doctype"
		       "notation" "sgml")))
	  (t
	   (goto-char here)
	   (ispell-complete-word)))
    (message "Tab=%s" tab)
    (when tab
      (setq tab (mapcar
		 (lambda (x)
		   (cons (if (symbolp x) (symbol-name x) x)
			 nil))
		 tab))
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

(defun sgml-current-list-of-endable-elements ()
  (let* ((elems nil)
	 (tree sgml-current-tree)
	 (state sgml-current-state))
    (while
	(and (sgml-final-p state)
	     (not (eq tree sgml-top-tree))
	     (progn
	       (setq elems
		     (nconc elems
			    (list (element-name (sgml-tree-element tree))))) 
	       sgml-omittag-transparent)
	     (element-etag-optional (sgml-tree-element tree)))
      (setq state (sgml-tree-pstate tree)
	    tree (sgml-tree-parent tree)))
    elems))


;;;; SGML mode: menus

(defun sgml-options-menu (&optional event)
  (interactive "e")
  (let
      ((options '(("Balanced edit" . sgml-balanced-tag-edit)
		  ("Auto insert required elements"
		   . sgml-auto-insert-required-elements)
		  ("Omittag transparent" . sgml-omittag-transparent)
		  ("Leave point after insert" . sgml-leave-point-after-insert)
		  ("Indent data" . sgml-indent-data)
		  ("Always quote attribute values"
		   . sgml-always-quote-attributes)
		  ("Warn about undefined elements"
		   . sgml-warn-about-undefined-elements)
		  ("Normalize trims trailing blanks"
		   . sgml-normalize-trims)
		  ("Minimize attributes"
		   . sgml-minimize-attributes)
		  ;;("Omittag" . sgml-omittag)
		  ;;("Debug" . sgml-debug)
		  ))
       (indents '(("None" . nil) ("0" . 0) ("1" . 1) ("2" . 2) ("3" . 3)
		  ("4" . 4) ("5" . 5) ("6" . 6) ("7" . 7) ("8" . 8)))
       (maxlen 1)
       menu1 menu2
       chooice)
    (mapcar (lambda (entry)
	      (setq maxlen (max maxlen (length (car entry)))))
	    options)
    (setq menu1
	  (cons
	   "-- Options --"
	   (mapcar
	    (lambda (entry)
	      (cons (concat (car entry)
			    (make-string (- maxlen (length (car entry))) ? )
			    "  "
			    (if (eval (cdr entry)) "v/" ""))
		    (cdr entry)))
	    options)))
    (setq menu2
	  (cons
	   "-- Indent step --"
	   (mapcar
	    (lambda (entry)
	      (cons (format "%-6s%s"
			    (car entry)
			    (if (eq sgml-indent-step (cdr entry))
				"v/" ""))
		    (cons 'sgml-indent-step (cdr entry))))
	    indents)))
    (setq chooice (x-popup-menu event (list "hepp" menu1 menu2)))
    (let (var val)
    (cond
     ((consp chooice)
      (setq var (car chooice)
	    val (cdr chooice)))
     (chooice
      (setq var chooice
	    val (not (symbol-value var)))))
    (when var
      (make-local-variable var)
      (set var val)
      (message "%s set to %s" var val)))))

;;;; Debugging

;;(define-key sgml-mode-map "\C-cg" 'sgml-goto-cache)
(define-key sgml-mode-map "\C-c\C-b" 'sgml-setup-buffer)
(define-key sgml-mode-map "\C-c\C-x" 'sgml-dump)

(defun sgml-this-element ()
  (interactive)
  (let ((tree (sgml-find-element-of (point))))
    (sgml-dump-rec tree)))

(defun sgml-goto-cache ()
  (interactive)
  (sgml-find-start-point (point))
  (message "%s" (sgml-context-as-string)))

(defun sgml-setup-buffer ()
  (interactive)
  (save-excursion
    (sgml-reset-parse-state)))

(defun sgml-dump (arg)
  (interactive "P")
  (when arg
    (sgml-parse-to-here))
  (with-output-to-temp-buffer "*Dump*"
    (sgml-dump-rec sgml-top-tree)))

(defun sgml-dump-rec (u)
  (when u
    (princ
     (format
      "%s%s start:%s(%s) end:%s(%s)\n"
      (make-string (sgml-tree-level u) ?. )
      (sgml-element-name u)
      (sgml-tree-start u) (sgml-tree-stag-len u)
      (sgml-tree-end u) (sgml-tree-etag-len u)))
    (sgml-dump-rec (sgml-tree-content u))
    (sgml-dump-rec (sgml-tree-next u))))

;;;; For edebug

(put 'when 'edebug-form-hook t)
(put 'unless 'edebug-form-hook t)
(put 'push 'edebug-form-hook '(form sexp))
(put 'setf 'edebug-form-hook '(sexp form))


(provide 'psgml-parse)
;;; psgml-parse.el end here
