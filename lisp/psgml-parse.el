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

;;; Interface to psgml-dtd
(eval-and-compile
  (autoload 'sgml-make-primitive-content-token "psgml-dtd")
  (autoload 'sgml-check-doctype-body "psgml-dtd")
  (autoload 'sgml-do-usemap-element  "psgml-dtd")
  )

;;;; Variables

;;; Hooks

(defvar sgml-close-element-hook nil
  "The hook run by `sgml-close-element'.
These functions are invoked with `sgml-current-tree' bound to the
element just parsed.")

(defvar sgml-doctype-parsed-hook nil
  "This hook is caled after the doctype has been parsed.
It can be used to load any additional information into the DTD structure.")

(defvar sgml-sysid-resolve-functions nil
  "This variable should contain a list of functions.
Each function should take one argument, the system identifier of an entity.
If the function can handle that identifier, it should insert the text
of the entity into the current buffer at point and return t.  If the
system identifier is not handled the function should return nil.")

;;; Internal variables

(defconst sgml-pcdata-token (intern "#PCDATA"))

(defvar sgml-computed-map nil
  "Internal representation of entity search map.")

(defvar sgml-used-entity-map nil
  "The value of `sgml-current-entity-map' used to compute the map in
`sgml-compute-map'.")

(defvar sgml-last-element nil
  "Used to keep information about position in element structure between
commands.")

(defconst sgml-users-of-last-element
  '(sgml-beginning-of-element
    sgml-end-of-element
    sgml-up-element
    sgml-backward-up-element
    sgml-backward-element
    sgml-forward-element
    sgml-down-element
    sgml-show-context
    sgml-next-data-field
    )
  "List of commands that set the sgml-last-element variable.")

(defvar sgml-parser-syntax nil
  "Syntax table used during parsing.")


;;; Variables dynamically bound to affect parsing

(defvar sgml-throw-on-warning nil
  "Set to a symbol other than nil to make sgml-log-warning throw to that symbol.")

(defvar sgml-suppress-warning nil
  "Set to t to suppress warnings.")

(defvar sgml-close-element-trap nil
  "Can be nil for no trap, an element or t for any element.
Tested by sgml-close-element to see if the parse should be ended.")

(defvar sgml-goal 0
  "Point in buffer to parse up to.")

(defvar sgml-shortref-handler (function sgml-handle-shortref)
  "Function called by parser to handle a short reference.
Called with the entity as argument.  The start and end of the 
short reference is `sgml-markup-start' and point.")


;;; Global variables active during parsing

(defvar sgml-dtd-info nil
  "Holds the `sgml-dtd' structure describing the current DTD.")

(defvar sgml-current-entity-map (make-vector 4 nil)
  "The current values of `sgml-local-catalogs', `sgml-catalog-files',
`sgml-system-path', and `sgml-public-map' as a vector.")

(defvar sgml-current-omittag nil
  "Value of `sgml-omittag' in main buffer. Valid during parsing.")

(defvar sgml-current-shorttag nil
  "Value of `sgml-shorttag' in main buffer. Valid during parsing.")

(defvar sgml-current-state nil
  "Current state in content model or model type if CDATA, RCDATA or ANY.")

(defvar sgml-current-shortmap nil
  "The current active short reference map.")

(defvar sgml-current-tree nil
  "Current parse tree node, identifies open element.")

(defvar sgml-previous-tree nil
  "Previous tree node in current tree.
This is nil if no previous node.")

(defvar sgml-markup-type nil
"Contains the type of markup parsed last.
The value is a symbol:
nil	- pcdata or space
CDATA	- CDATA or RCDATA
comment	- comment declaration
doctype	- doctype declaration
end-tag 
ignored	- ignored marked section
ms-end	- marked section start, if not ignored 
ms-start - marked section end, if not ignored
pi	- processing instruction
sgml	- SGML declaration
start-tag
entity  - general entity reference
param   - parameter reference
shortref- short reference
mdecl   - markup declaration
")

(defvar sgml-top-tree nil
  "Root node of parse tree during parsing.")

(defvar sgml-markup-tree nil
  "Tree node of markup parsed.
In case markup closed element this is different from sgml-current-tree.
Only valid after `sgml-parse-to'.")

(defvar sgml-markup-start nil
  "Start point of markup beeing parsed.")


;; Vars used in *param* buffers

(defvar sgml-previous-buffer nil)

(defvar sgml-parameter-name nil)

(defvar sgml-current-eref nil
  "This is the entity reference used to enter current entity.
If this is nil, then current entity is main buffer.")


;;; For loading DTD

(eval-and-compile
  (defconst sgml-max-single-octet-number 250))

(defvar sgml-single-octet-threshold 255
  "Octets greater than this is the first of a two octet coding.")

(defvar sgml-read-token-vector nil)	; Vector of symbols used to decode
					; token numbers.
(defvar sgml-read-nodes nil)		; Vector of nodes used when reading
					; a finite automaton.

;; Buffer local variables 

(defvar sgml-document-element nil)
(make-variable-buffer-local 'sgml-document-element)

(defvar sgml-loaded-dtd nil
  "File name corresponding to current DTD.")
(make-variable-buffer-local 'sgml-loaded-dtd)

(defvar sgml-current-element-name nil
  "Name of current element for mode line display.")


;;;; Build parser syntax table

(setq sgml-parser-syntax (make-syntax-table))

(let ((i 0))
  (while (< i 256)
    (modify-syntax-entry i " " sgml-parser-syntax)
    (setq i (1+ i))))

(mapconcat (function (lambda (c)
	     (modify-syntax-entry c "w" sgml-parser-syntax)))
	   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrtsuvwxyz" "")
(mapconcat (function (lambda (c)
		       (modify-syntax-entry c "_" sgml-parser-syntax)))
	   "-.0123456789" "")
(mapconcat (function (lambda (c)
		       (modify-syntax-entry c "." sgml-parser-syntax)))
	   "</>&%#[]" ".")

;;(progn (set-syntax-table sgml-parser-syntax) (describe-syntax))


(defmacro sgml-with-parser-syntax (&rest body)
  (` (let ((normal-syntax-table (syntax-table)))
       (set-syntax-table sgml-parser-syntax)
       (unwind-protect
	   (progn (,@ body))
	 (set-syntax-table normal-syntax-table)))))


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
  (mapcar (function (lambda (m) (sgml-move-token m)))
	  moves))

(defun sgml-required-tokens (state)
  (if (sgml-normal-state-p state)
      (sgml-tokens-of-moves (sgml-state-reqs state))
    (or (sgml-required-tokens (sgml-&state-substate state))
        (loop for s in (sgml-&state-dfas state)
              nconc (sgml-tokens-of-moves (sgml-state-reqs s)))
        (sgml-tokens-of-moves (sgml-state-reqs (sgml-&state-next state))))))


(defun sgml-optional-tokens (state)
  (if (sgml-normal-state-p state)
      (sgml-tokens-of-moves (sgml-state-opts state))
    (nconc
     (sgml-optional-tokens (sgml-&state-substate state))
     (if (sgml-final (sgml-&state-substate state))
	 (loop for s in (sgml-&state-dfas state)
	       nconc (sgml-tokens-of-moves (sgml-state-opts s))))
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


;;;; Attribute Types

;;; Basic Types
;; name = string	attribute names are lisp symbols
;; attval = string	attribute values are lisp strings

;;; Attribute Declaration Type 
;; attdecl = <name, declared-value, default-value>

;; This is the result of the ATTLIST declarations in the DTD.
;; All attribute declarations for an element is the elements
;; attlist.

;;; Attribute Declaration Operations
;; sgml-make-attdecl: name declared-value default-value -> attdecl
;; sgml-attdecl-name: attdecl -> name
;; sgml-attdecl-declared-value: attdecl -> declared-value
;; sgml-attdecl-default-value: attdecl -> default-value

;;; Attribute Declaration List Type
;; attlist = attdecl*

;;; Attribute Declaration List Operations
;; sgml-lookup-attdecl: name x attlist -> attdecl

;;; Declared Value Type
;; declared-value = (token-group | notation | simpel)
;; token-group = nametoken+
;; notation = nametoken+
;; simple = symbol		lisp symbol corresponding to SGML type

;;; Declared Value Operations
;; sgml-declared-value-token-group: declared-value -> list of symbols
;; sgml-declared-value-notation: declared-value -> list of symbols
;; (empty list if not token-group/notation)

;;; Default Value Type
;; default-value = (required | implied | conref | specified )
;; implied, conref = constant symbol
;; specified = (fixed | normal)
;; fixed, normal = attval

;;; Default Value Operations
;; sgml-default-value-attval: default-value -> (attval | nil)
;; sgml-default-value-type-p: type x default-value -> cond

;;; Attribute Specification Type
;; attspec = <name, attval>

;; This is the result of parsing an attribute specification.

;; sgml-make-attspec: name x attval -> attspec
;; sgml-attspec-name: attspec -> name
;; sgml-attspec-attval: attspec -> attval


;;; Attribute Specification List Type
;; asl = attspec*

;; aka. attribute value list


;;; Code

;;; attdecl representation = (name declared-value default-value)

(defun sgml-make-attdecl (name dcl-value default-value)
  (list name dcl-value default-value))

(defun sgml-attdecl-name (attdecl)
  (car attdecl))

(defun sgml-attdecl-declared-value (attdecl)
  "The declared value of ATTDECL.
It may be a symbol or (name-token-group (NAME1 ... NAMEn))
or (notation  (NOT1 ... NOTn))"
  (cadr attdecl))

(defun sgml-attdecl-default-value (attdecl)
  "The default value of ATTDECL.
The default value is either a symbol (required | implied | current |
conref) or a list with first element nil or symbol 'fixed' and second
element the value."
  (caddr attdecl))


;;; attlist representation = (attspec*)

(defun sgml-lookup-attdecl (name attlist)
  "Return the attribute declaration for NAME in ATTLIST."
  (assoc name attlist))

(defun sgml-attribute-with-declared-value (attlist declared-value)
  "Find the first attribute in ATTLIST that has DECLARED-VALUE."
  (let ((found nil))
    (while (and attlist (not found))
      (when (equal declared-value
		   (sgml-attdecl-declared-value (car attlist)))
	(setq found (car attlist)))
      (setq attlist (cdr attlist)))
    found))


;;; declared-value representation
;; token-group = (name-token (symbol+))
;; notation = (notation (symbol+))
;; simple = symbol		lisp symbol correspoinding to SGML type

(defun sgml-make-declared-value (type &optional names)
  "Make a declared-value of TYPE.
TYPE should be a symbol.  If TYPE is name-token-group or notation
NAMES should be a list of symbols."
  (if (consp names)
      (list type names)
    type))

(defun sgml-declared-value-token-group (declared-value)
  "Return the name token group for the DECLARED-VALUE.
This applies to name token groups.  For other declared values nil is
returned."
  (and (consp declared-value)
       (eq 'name-token-group (car declared-value))
       (cadr declared-value)))

(defun sgml-declared-value-notation (declared-value)
  "Return the list of notation names for the DECLARED-VALUE.
This applies to notation declared value.  For other declared values
nil is returned."
  (and (consp declared-value)
       (eq 'notation (car declared-value))
       (cadr declared-value)))

;;; default-value representation = symbol | ((nil | 'fixed) attval)

(defun sgml-make-default-value (type &optional attval)
  (if attval
      (list type attval)
    type))

(defun sgml-default-value-attval (default-value)
  "Return the actual default value of the declared DEFAULT-VALUE.
The actual value is a string. Return nil if no actual value."
  (and (consp default-value)
       (cadr default-value)))

(defun sgml-default-value-type-p (type default-value)
  "Return true if DEFAULT-VALUE is of TYPE.
Where TYPE is a symbol, one of required, implied, conref, or fixed."
  (or (eq type default-value)
      (and (consp default-value)
	   (eq type (car default-value)))))


;;; attspec representation = (symbol . string)

(defun sgml-make-attspec (name attval)
  "Create an attspec from NAME and ATTVAL.
Special case, if ATTVAL is nil this is an implied attribute."
  (cons name attval))

;; sgml-attspec-name: attspec -> name
(defun sgml-attspec-name (attspec)
  (car attspec))

;; sgml-attspec-attval: attspec -> attval
(defun sgml-attspec-attval (attspec)
  "Return the value of attribute specification ATTSPEC.
If ATTSPEC is nil, nil is returned."
  (cdr attspec))

;;; asl representaion = (attspec*)

(defun sgml-lookup-attspec (name asl)
  (assoc name asl))


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


;;;; DTD 

;;DTD = (doctype, eltypes, parameters, entities, shortmaps)
;;doctype = name
;;eltypes = oblist
;;parameters = entity*
;;entities = entity*
;;shortmaps = (name, shortmap)*

(defstruct (sgml-dtd
	    (:constructor
	     sgml-make-dtd  (doctype)))
  doctype				; STRING, name of doctype
  (eltypes				; OBLIST, element types defined
   (sgml-make-eltypes-table))
  (parameters				; ALIST
   (sgml-make-entity-table))
  (entities				; ALIST
   (sgml-make-entity-table))
  (shortmaps				; ALIST
   (sgml-make-shortref-table)))



;;;; Element type objects

;;; Oblist for eltype

(defun sgml-make-eltypes-table ()
  (make-vector 17 0))

(defun sgml-eltype-name (et)
  (symbol-name et))

(defun sgml-eltype-token (et)
  "Return a token for the element type"
  et)

(defun sgml-token-eltype (token)
  "Return the element type corresponding to TOKEN."
  token)

(defmacro sgml-prop-fields (&rest names)
  (cons
   'progn
   (loop for n in names collect
	 (`(defmacro (, (intern (format "sgml-eltype-%s" n))) (et)
	     (list 'get et ''(, n)))))))

(sgml-prop-fields 
 ;;flags			; optional tags and mixed
					; (perhaps in value field)
 ;;model					; Content type
					; (perhaps in function field)
 attlist				; List of defined attributes
 includes				; List of included elements
 excludes				; List of excluded elements
 shortmap				; Associated shortref map
					; nil if none and 'empty if #empty
 )


;;(macroexpand '(sgml-eltype-model x))
;;(symbol-function 'sgml-eltype-model)

(defmacro sgml-eltype-flags (et)
  (` (symbol-value (, et))))

(defmacro sgml-eltype-model (et)
  (` (symbol-function (, et))))

(defun sgml-eltype-stag-optional (et)
  (oddp (sgml-eltype-flags et)))

(defun sgml-eltype-etag-optional (et)
  (/= 0 (logand 2 (sgml-eltype-flags et))))

(defun sgml-eltype-mixed (et)
  (< 3 (sgml-eltype-flags et)))

(defsetf sgml-eltype-stag-optional (et) (f)
  (list 'sgml-set-eltype-flag et 1 f)) 
(defsetf sgml-eltype-etag-optional (et) (f)
  (list 'sgml-set-eltype-flag et 2 f)) 
(defsetf sgml-eltype-mixed (et) (f)
  (list 'sgml-set-eltype-flag et 4 f)) 

(defun sgml-set-eltype-flag (et mask f)
  (setf (sgml-eltype-flags et)
	(logior (logand (sgml-eltype-flags et) (lognot mask))
	       (if f mask 0))))

(defmacro sgml-eltype-appdata (et prop)
  "Get application data from element type ET with name PROP.
PROP should be a symbol, reserved names are: flags, model, attlist,
includes, excludes, conref-regexp, mixed, stag-optional, etag-optional."
  (` (get (, et) (, prop))))

(defun sgml-eltype-all-appdata (et)
  (loop for p on (symbol-plist et) by (function cddr)
	unless (memq (car p) '(model flags attlist includes excludes))
	nconc (list (car p) (cadr p))))

(defun sgml-define-eltype (et stag-opt etag-opt
			      content excludes includes mixed
			      &optional attlist appdata)
  "Define the element type ET.
ET is the result of a sgml-lookup-eltype."
  (setf (sgml-eltype-stag-optional et) stag-opt
	(sgml-eltype-etag-optional et) etag-opt
	(sgml-eltype-model et) 	content
	(sgml-eltype-mixed et) 	mixed)
  (when excludes
    (setf (sgml-eltype-excludes et) excludes))
  (when includes
    (setf (sgml-eltype-includes et) includes))
  (when attlist
    (setf (sgml-eltype-attlist et) attlist))
  (setf (symbol-plist et)
	(nconc (symbol-plist et) appdata)))

(defun sgml-define-eltype-attlist (et attlist)
  "Define the ATTLIST for eltype ET."
  (setf (sgml-eltype-attlist et) attlist))

(defun sgml-lookup-eltype (name &optional dtd)
  "Lookup the element defintion for NAME (string)."
  (assert (stringp name))
  (let ((et (intern name
		    (sgml-dtd-eltypes (or dtd sgml-dtd-info)))))
    (unless (boundp et)			; first reference
      (setf (sgml-eltype-flags et) 0)
      (setf (sgml-eltype-model et) nil))
    et))

(defun sgml-make-eltype (name)
  (let ((et (make-symbol name)))
    (setf (sgml-eltype-flags et) 0)
    et))

(defun sgml-eltype-completion-table (eltypes)
  "Make a completion table from a list, ELTYPES, of element types."
  (loop for et in eltypes as name = (sgml-eltype-name et)
	collect (cons name name)))

(defun sgml-map-eltypes (fn dtd &optional collect)
  (mapatoms (if collect
		(progn
		  (setq collect nil)
		  (function (lambda (a)
			    (push (funcall fn a) collect))))
		fn)
	    (sgml-dtd-eltypes dtd))
  (if collect (nreverse collect)))


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
	  ((eq c 4) nil)
	  ((eq c 128)
	   (sgml-read-model)))))

(defun sgml-read-decode-flag (flag mask)
  (not (zerop (logand flag mask))))

(defun sgml-read-element (eltype)
  (let* ((flags (sgml-read-octet))
	 (content (sgml-read-content))
	 (incl (sgml-read-token-seq))
	 (excl (sgml-read-token-seq))
	 (attlist (sgml-read-sexp))
	 (appdata (sgml-read-sexp)))
    (sgml-define-eltype
     eltype
     (sgml-read-decode-flag flags 1)		; stag optional
     (sgml-read-decode-flag flags 2)		; etag optional
     content
     excl
     incl
     (sgml-read-decode-flag flags 4)		; mixed
     attlist
     appdata)))

(defun sgml-read-dtd (buffer)
  "Decode the saved DTD in BUFFER, set global variabels."
  (let ((gc-cons-threshold (max gc-cons-threshold 500000))
	(cb (current-buffer))
	temp dtd)
    (set-buffer buffer)
    (goto-char (point-min))
    (setq temp (sgml-read-sexp))	; file-version
    (cond ((equal temp '(sgml-saved-dtd-version 4))
	   (setq sgml-single-octet-threshold sgml-max-single-octet-number))
	  (t
	   (error "Unknown file format for saved DTD: %s" temp)))
    ;; Doctype -- create dtd structure
    (setq dtd (sgml-make-dtd (sgml-read-sexp)))
    ;; Element type names -- read and create token vector
    (setq temp (sgml-read-number))	; # eltypes
    (setq sgml-read-token-vector (make-vector (1+ temp) nil))
    (aset sgml-read-token-vector 0 sgml-pcdata-token)
    (loop for i from 1 to temp do
	  (aset sgml-read-token-vector i
		(sgml-lookup-eltype (sgml-read-sexp) dtd)))
    ;; Element type descriptions
    (loop for i from 1 to (sgml-read-number) do
	  (sgml-read-element (aref sgml-read-token-vector i)))
    (setf (sgml-dtd-parameters dtd) (sgml-read-sexp))
    (setf (sgml-dtd-entities dtd) (sgml-read-sexp))
    (setf (sgml-dtd-shortmaps dtd) (sgml-read-sexp))
    (set-buffer cb)
    dtd))

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
  (setq sgml-loaded-dtd nil)		; Allow reloading of DTD
  ;; Search for 'file' on the sgml-system-path [ndw]
  (let (real-file)
    (let ((l (cons "." sgml-system-path)))
      (while (and l (not real-file))
	(unless (file-exists-p
		 (setq real-file
		       (expand-file-name file (car l))))
	  (setq real-file nil))
	(setq l (cdr l)))
      real-file)
    (or real-file
	(error "Saved DTD file %s not found" file))
    (let ((cb (current-buffer))
	  (tem nil)
	  (dtd nil)
	  (l (buffer-list)))
      ;; Search loaded buffer for a already loaded DTD
      (while (and l (null tem))
	(set-buffer (car l))
	(if (equal sgml-loaded-dtd real-file)
	    (setq tem (current-buffer)))
	(setq l (cdr l)))
      (cond
       (tem				; loaded DTD found
	(setq dtd (sgml-pstate-dtd sgml-buffer-parse-state)))
       (t				; load DTD from file
	(set-buffer cb)
	(setq tem (generate-new-buffer " *saveddtd*"))
	(unwind-protect
	    (progn
	      (message "Loading DTD from %s..." file)
	      (set-buffer tem)
	      (insert-file-contents real-file)
	      (set-buffer cb)
	      (setq dtd (sgml-read-dtd tem))
	      (message "Loading DTD from %s...done" file))
	  (kill-buffer tem))))
      (set-buffer cb)
      (sgml-set-initial-state dtd)
      (setq sgml-default-dtd-file file)
      (setq sgml-loaded-dtd real-file))))

;;;; Entity Manager

(defstruct (sgml-entity
	    (:type list)
	    (:constructor sgml-make-entity (name type text)))
  name					; Name of entity (string)
  type					; Type of entity CDATA NDATA PI SDATA
  text					; string or external
  )

(defun sgml-entity-data-p (entity)
  "True if ENTITY is a data entity, that is not a text entity."
  (not (eq (sgml-entity-type entity) 'text)))

(defun sgml-entity-insert-text (entity)
  "Insert the text of ENTITY."
  (let ((text (sgml-entity-text entity)))
    (cond
     ((stringp text)
      (insert text))
     (t
      (sgml-insert-external-entity text
				   (sgml-entity-type entity)
				   (sgml-entity-name entity))))))

;;; Entity tables

(defun sgml-make-entity-table ()
  (list nil))

(defun sgml-lookup-entity (name entity-table)
  (assoc name (cdr entity-table)))

(defun sgml-entity-declare (name entity-table type text)
  (unless (sgml-lookup-entity name entity-table)
    (sgml-debug "Declare entity %s %s as %S" name type text)
    (nconc entity-table
	   (list (sgml-make-entity name type text)))))

(defun sgml-entity-completion-table (entity-table)
  "Make a completion table from the ENTITY-TABLE."
  (cdr entity-table))

(defun sgml-map-entities (fn entity-table &optional collect)
  (if collect
      (mapcar fn (cdr entity-table))
    (loop for e in (cdr entity-table) do (funcall fn e))))


;;; External identifyer resolve

(defun sgml-compute-map ()
  (unless (and sgml-computed-map
	       (equal sgml-used-entity-map sgml-current-entity-map))
    (setq sgml-computed-map
	  (nconc (loop for file in (append (elt sgml-current-entity-map 0)
					   (elt sgml-current-entity-map 1))
		       nconc (sgml-load-catalog file))
		 (mapcar (function (lambda (s) (concat s "/%s")))
			 (elt sgml-current-entity-map 2))
		 (elt sgml-current-entity-map 3))
	  sgml-used-entity-map (copy-sequence sgml-current-entity-map))))

(defun sgml-update-catalog ()
  "Reload catalog files."
  (interactive)
  (setq sgml-computed-map nil)
  (sgml-compute-map))

(defun sgml-insert-external-entity (extid &optional type name)
  ;; extid is (pubid . sysid)
  (sgml-compute-map)
  (let* ((pubid (car extid))
	 (sysid (cdr extid))
	 (subst (list '(?% ?%)))
	 res)
    (when pubid
      (nconc subst (list (cons ?p (sgml-transliterate-file pubid)))
	     (sgml-pubid-parts pubid))
      (setq pubid (sgml-canonize-pubid pubid)))
    (when sysid (nconc subst (list (cons ?s sysid))))
    (when name  (nconc subst (list (cons ?n name))))
    (when type  (nconc subst (list (cons ?y (cond ((eq type 'dtd) "dtd")
						  ((eq type 'text) "text")
						  ((eq type 'param) "parm")
						  (t "sgml"))))))
    (sgml-debug "Ext. file subst. = %S" subst)
    (when sysid
      (loop for fn in sgml-sysid-resolve-functions
	    until res do (setq res (funcall fn sysid))))
    (unless res
      (loop
       for entry in sgml-computed-map until res do
       (cond
	((stringp entry)
	 (setq res (sgml-pub-expand entry subst)))
	;; Catalog entry
	((or (and (eq 'entity (first entry))
		  name (not (eq type 'dtd))
		  (string= name (second entry)))
	     (and (eq 'public (first entry))
		  pubid
		  (string= pubid (second entry)))
	     (and (eq 'doctype (first entry))
		  (eq type 'dtd)
		  (string= name (second entry))))
	 (setq res (third entry)))
	;; Predicated entry
	((and (stringp (first entry))
	      (string-match (first entry) pubid))
	 (setq res (sgml-pub-expand (second entry) subst))))
       (when res
	 (sgml-debug "file-readable-p? %S" res)
	 (unless (file-readable-p
		  (setq res (substitute-in-file-name res)))
	   (setq res nil))))
      (cond
       (res (insert-file-contents res))
       (t
	(sgml-log-warning "External entity %s not found" name)
	(when pubid
	  (sgml-log-warning "  Public identifier %s" pubid))
	(when sysid
	  (sgml-log-warning "  System identfier %s" sysid)))))))


(defun sgml-load-catalog (file)
  "Load a catalog file."
  (let ((old-buffer (current-buffer))
	(buf (generate-new-buffer " *SGML catalog*"))
	map)
    (setq file (expand-file-name file))
    (unwind-protect
	(progn
	  (message "Parsing SGML catalog file %s ..." file)
	  (sgml-debug "Parsing SGML catalog file %s ..." file)
	  (set-buffer buf)
	  (insert-file-contents file)
	  (setq default-directory (file-name-directory file))
	  (setq map (sgml-parse-catalog-buffer))
	  (message "Parsing SGML catalog file %s ... done." file))
      (kill-buffer buf))
    (set-buffer old-buffer)
    map))

; Parse a buffer full of catalogue entries.
(defun sgml-parse-catalog-buffer ()
  "Parse all entries in a catalogue."
  (goto-char (point-min))
  (sgml-with-parser-syntax
   (let (type name sysid map)
     (while (progn (sgml-skip-cs)
		   (setq type (sgml-parse-nametoken)))
       (setq type (intern (downcase type)))
       (sgml-skip-cs)
       (cond
					; Public identifier.
	((eq type 'public)
	 (setq name (sgml-canonize-pubid (sgml-parse-minimum-literal))))
					; Entity reference.
	((eq type 'entity)
	 (if (sgml-parse-char ?%)
	     (setq type 'pentity))
	 (setq name (sgml-parse-name t)))
					; Document type.
	((eq type 'doctype)
	 (setq name (sgml-parse-nametoken t)))
					; Oops!
	(t
	 (error "Error in catalog file: \"%s\" should be \"PUBLIC\", \"ENTITY\", or \"DOCTYPE\"." type)))
       
       (sgml-skip-cs)
       (setq sysid (or (sgml-parse-literal)
		       (buffer-substring (point)
					 (progn (skip-chars-forward "^ \r\n\t")
						(point)))))
       (setq sysid (expand-file-name sysid))
       (setq map (nconc map (list (list type name sysid)))))
     map)))

(defun sgml-skip-cs ()
  "Skip over the separator used in the catalog."
  (while (or (sgml-parse-s)
	     (sgml-parse-comment))))



(defconst sgml-formal-pubid-regexp
  (concat
   "^\\(+//\\|-//\\|\\)"		; Registered indicator  [1]
   "\\(\\([^/]\\|/[^/]\\)+\\)"		; Owner                 [2]
   "//"
   "\\([^ ]+\\)"			; Text class            [4]
   " "
   "\\(\\([^/]\\|/[^/]\\)*\\)"		; Text description      [5]
   "//"
   "\\(\\([^/]\\|/[^/]\\)*\\)"		; Language              [7]
   "\\(//"				;   		        [9] 
   "\\(\\([^/]\\|/[^/]\\)*\\)"		; Version	        [10]
   "\\)?"))

(defun sgml-pubid-parts (pubid)
  (nconc
   (if (string-match sgml-formal-pubid-regexp pubid)
       (nconc
	(list
	 (cons ?o (sgml-transliterate-file (sgml-matched-string pubid 2)))
	 (cons ?c (downcase (sgml-matched-string pubid 4)))
	 (cons ?d (sgml-transliterate-file (sgml-matched-string pubid 5)))
	 ;; t alias for d  (%T used by sgmls)
	 (cons ?t (sgml-transliterate-file (sgml-matched-string pubid 5)))
	 (cons ?l (downcase (sgml-matched-string pubid 7))))
	(if (match-beginning 9)
	    (list (cons ?v (sgml-transliterate-file
			    (sgml-matched-string pubid 10)))))))))


(defun sgml-canonize-pubid (pubid)
  (if (string-match sgml-formal-pubid-regexp pubid)
      (concat
       (sgml-matched-string pubid 1)	; registered indicator
       (sgml-matched-string pubid 2)	; Owner
       "//"
       (upcase (sgml-matched-string pubid 4)) ; class
       " "
       (sgml-matched-string pubid 5)	; Text description
       "//"
       (upcase (sgml-matched-string pubid 7)) ; Language
       "//"
       (if (match-beginning 9)
	   (sgml-matched-string pubid 10) ""))))

(defun sgml-pub-expand-char (c parts)
  (cdr-safe (assq (downcase c) parts)))

(defun sgml-transliterate-file (string)
  (mapconcat (function (lambda (c)
			 (char-to-string
			  (or (cdr-safe (assq c sgml-public-transliterations))
			      c))))
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

;;; Entity references and positions

(defstruct (sgml-eref
	    (:constructor sgml-make-eref (entity start end))
	    (:type list))
  entity
  start					; type: epos
  end)

(defun sgml-make-epos (eref pos)
  (cons eref pos))

(defun sgml-epos-eref (epos)
  (if (consp epos)
      (car epos)))

(defun sgml-epos-pos (epos)
  (if (consp epos)
      (cdr epos)
    epos))

(defun sgml-bpos-p (epos)
  (numberp epos))

(defun sgml-strict-epos-p (epos)
  (consp epos))

(defun sgml-epos (pos)
  "Convert a buffer position POS into a epos."
  (if sgml-current-eref
      (sgml-make-epos sgml-current-eref pos)
    pos))

(defun sgml-epos-erliest (epos)
  (while (consp epos)
    (setq epos (sgml-eref-start (sgml-epos-eref epos))))
  epos)

(defun sgml-epos-latest (epos)
  (while (consp epos)
    (setq epos (sgml-eref-end (sgml-epos-eref epos))))
  epos)

(defun sgml-epos-promote (epos)
  (while (and (consp epos)
	      (= (cdr epos) 1))
    (setq epos (sgml-eref-start (car epos))))
  (sgml-epos-latest epos))


;;; Parameter entities and files

(defun sgml-push-to-entity (entity &optional ref-start)
  (let ((cb (current-buffer))
	(buf (generate-new-buffer " *entity*"))
	eref
	file)
    ;;*** should eref be argument to fun?
    (setq eref (sgml-make-eref
		entity
		(sgml-epos (or ref-start (point)))
		(sgml-epos (point))))
    (sgml-debug "Enter entity ref %S" eref)
    (set-buffer buf)
    (make-local-variable 'sgml-current-eref)
    (setq sgml-current-eref eref)
    (set-syntax-table sgml-parser-syntax)
    (make-local-variable 'sgml-previous-buffer)
    (setq sgml-previous-buffer cb)
    (make-local-variable 'sgml-parameter-name)
    (setq sgml-parameter-name entity)
    (sgml-entity-insert-text entity)
    (goto-char (point-min))))

(defun sgml-pop-entity ()
  (cond ((and (boundp 'sgml-previous-buffer)
	      (bufferp sgml-previous-buffer))
	 (sgml-debug "Exit entity")
	 (kill-buffer (prog1 (current-buffer)
			(set-buffer sgml-previous-buffer)))
	 t)))

(defun sgml-goto-epos (epos)
  "Goto a position in an entity given by EPOS."
  (assert epos)
  (cond ((sgml-bpos-p epos)
	 (goto-char epos))
	(t
	 (let ((eref (sgml-epos-eref epos)))
	   (sgml-goto-epos (sgml-eref-end eref))
	   (sgml-push-to-entity (sgml-eref-entity eref)
				(sgml-epos-pos (sgml-eref-start eref))))
	 (goto-char (sgml-epos-pos epos)))))

(defun sgml-pop-all-entities ()
  (while (sgml-pop-entity)))

(defun sgml-any-open-param/file ()
  "Return true if there currently is a parameter or file open."
  (and (boundp 'sgml-previous-buffer)
       sgml-previous-buffer))


;;;; Parse tree

(defstruct (sgml-tree
	    (:type vector)
	    (:constructor sgml-make-tree
			  (eltype stag-epos stag-len  parent level
				  excludes includes pstate net-enabled
				  conref &optional shortmap pshortmap)))
  eltype				; element object
  ;;start					; start point in buffer
  ;;end					; end point in buffer
  stag-epos				; start-tag entity position
  etag-epos				; end-tag entity position
  stag-len				; length of start-tag
  etag-len				; length of end-tag
  parent				; parent tree
  level					; depth of this node
  excludes				; current excluded elements
  includes				; current included elements
  pstate				; state in parent
  next					; next sibling tree
  content				; child trees
  net-enabled				; if NET enabled (t this element,
					;  other non-nil, some parent)
  conref				; if conref attribute used
  shortmap				; shortmap at start of element
  pshortmap				; parents shortmap
)


;;element-end (e):
;;     If bpos-p (etag-epos (e)):
;;          return etag-epos (e) + etag-len (e)
;;     If etag-len (e) = 0: return promote (etag-epos (e))
;;     else: return latest (etag-epos (e))
(defun sgml-tree-end (tree)
  "Buffer position after end of TREE"
  (let ((epos (sgml-tree-etag-epos tree))
	(len (sgml-tree-etag-len tree)))
    (cond ((sgml-bpos-p epos)
	   (+ epos len))
	  ((zerop len)
	   (sgml-epos-promote epos))
	  (t
	   (sgml-epos-latest epos)))))


;;;; (text) Element view of parse tree

(defmacro sgml-alias-fields (orig dest &rest fields)
  (let ((macs nil))
    (while fields
      (push
       (` (defmacro (, (intern (format "%s-%s" dest (car fields)))) (element)
	    (, (format "Return %s field of ELEMENT." (car fields)))
	    (list
	     '(, (intern (format "%s-%s" orig (car fields))))
	     element)))
       macs)
      (setq fields (cdr fields)))
    (cons 'progn macs)))

(sgml-alias-fields sgml-tree sgml-element
  eltype				; element object
  ;;  start					; start point in buffer
  stag-epos
  etag-epos
  stag-len				; length of start-tag
  etag-len				; length of end-tag
  parent				; parent tree
  level					; depth of this node
  excludes				; current excluded elements
  includes				; current included elements
  pstate				; state in parent
  net-enabled				; if NET enabled
  )

(defun sgml-element-model (element)
  "Declared content or content model of ELEMENT."
  (sgml-eltype-model (sgml-tree-eltype element)))

(defun sgml-element-name (element)
  "Return name (symbol) of ELEMENT."
  (sgml-tree-eltype element))

(defun sgml-element-gi (element)
  "Return general identifier (string) of ELEMENT."
  (sgml-eltype-name (sgml-tree-eltype element)))

(defun sgml-element-appdata (element prop)
  "Return the application data named PROP associated with the type of ELEMENT."
  (sgml-eltype-appdata (sgml-tree-eltype element) prop))

(defmacro sgml-element-stag-optional (element)
  "True if start-tag of ELEMENT is omissible."
  (`(sgml-eltype-stag-optional (sgml-tree-eltype (, element)))))

(defmacro sgml-element-etag-optional (element)
  "True if end-tag of ELEMENT is omissible."
  (`(sgml-eltype-etag-optional (sgml-tree-eltype (, element)))))

(defun sgml-element-attlist (element)
  "Return the attribute specification list of ELEMENT."
  (sgml-eltype-attlist (sgml-tree-eltype element)))

(defun sgml-element-mixed (element)
  "True if ELEMENT has mixed content."
  (sgml-eltype-mixed (sgml-tree-eltype element)))

(defun sgml-element-start (element)
  "Position before start of ELEMENT."
  (sgml-epos-promote (sgml-tree-stag-epos element)))

(defun sgml-element-stag-end (element)
  "Position after start-tag of ELEMENT."
  (let ((epos (sgml-tree-stag-epos element))
	(len (sgml-tree-stag-len element)))
    (cond ((sgml-bpos-p epos)
	   (+ epos len))
	  ((zerop len)
	   (sgml-epos-promote epos))
	  (t
	   (sgml-epos-latest epos)))))

(defun sgml-element-empty (element)
  "True if ELEMENT is empty."
  (or (eq sgml-empty (sgml-element-model element))
      (sgml-tree-conref element)))

(defun sgml-element-data-p (element)
  "True if ELEMENT can have data characters in its content."
  (or (sgml-element-mixed element)
      (eq sgml-cdata (sgml-element-model element))
      (eq sgml-rcdata (sgml-element-model element))))

(defun sgml-element-context-string (element)
  "Return string describing context of ELEMENT."
  (if (eq element sgml-top-tree)
      ""
    (format "in %s %s"
	    (sgml-element-gi element)
	    (sgml-element-context-string (sgml-tree-parent element)))))

;;;; Display and Mode-line

(defun sgml-update-display ()
  (when (not (eq this-command 'keyboard-quit))
    ;; Don't let point be inside an invisible region
    (when (and (get-text-property (point) 'invisible)
	       (eq (get-text-property (point) 'invisible)
		   (get-text-property (1- (point)) 'invisible)))
      (setq sgml-last-element nil)	; May not be valid after point moved
      (if (memq this-command '(backward-char previous-line backward-word))
	  (goto-char (or (previous-single-property-change (point) 'invisible)
			 (point-max)))
	(goto-char (or (next-single-property-change (point) 'invisible)
		       (point-max)))))
    (sit-for 0)
    (when (and (not (input-pending-p))
	       (or sgml-live-element-indicator
		   sgml-set-face))
      (let ((deactivate-mark nil)
	    (sgml-suppress-warning t)
	    (oldname sgml-current-element-name))
	(condition-case nil
	    (save-excursion
	      (cond ((and (memq this-command sgml-users-of-last-element)
			  sgml-last-element)
		     (setq sgml-current-element-name
			   (sgml-element-gi sgml-last-element)))
		    (t
		     (sgml-parse-to (point) (function input-pending-p))
		     (unless (input-pending-p)
		       (setq sgml-current-element-name
			     (sgml-element-gi sgml-current-tree))))))
	  (error (setq sgml-current-element-name "*error*")))
	(condition-case nil
	    (progn
	      (unless (input-pending-p)
		(force-mode-line-update)
		(sit-for 0)
		(save-excursion
		  (sgml-parse-to (window-end) (function input-pending-p) t)))
	      (sit-for 1)
	      (unless (input-pending-p)
		(save-excursion
		  (sgml-parse-to (point-max) (function input-pending-p) t))))
	  (error nil))))))

(defun sgml-set-active-dtd-indicator ()
  (set (make-local-variable 'sgml-active-dtd-indicator)
       (list (format " [%s" (or sgml-document-element "ANY"))
	     '(sgml-live-element-indicator ("/" sgml-current-element-name))
	     "]"))
  (force-mode-line-update))

;;;; Set markup type

(defun sgml-set-markup-type (type)
  "Set the type of the markup parsed to TYPE.
The markup starts at position given by variable sgml-markup-start and
ends at point."
  (when sgml-set-face
    (sgml-set-face-for sgml-markup-start (point) type))
  (setq sgml-markup-type type))


;;;; Parser state

(defstruct (sgml-pstate
	    (:constructor sgml-make-pstate (dtd top-tree)))
  dtd
  top-tree)

(defsubst sgml-excludes ()
  (sgml-tree-excludes sgml-current-tree))

(defsubst sgml-includes ()
  (sgml-tree-includes sgml-current-tree))

(defsubst sgml-current-mixed-p ()
  (sgml-element-mixed sgml-current-tree))

(defun sgml-set-initial-state (&optional dtd)
  "Set initial state of parsing"
  (make-local-variable 'before-change-function)
  (setq before-change-function 'sgml-note-change-at)
  (set (make-local-variable 'after-change-function)
       'sgml-set-face-after-change)
  (or dtd (setq dtd (sgml-make-dtd "ANY")))
  (setq sgml-document-element (sgml-dtd-doctype dtd))
  (sgml-set-active-dtd-indicator)
  (let ((top-type			; Fake element type for the top
					; node of the parse tree
	 (sgml-make-eltype "#DOC") ; was "Document (no element)"
	 ))
    (setf (sgml-eltype-model top-type)
	  (if (equal sgml-document-element "ANY")
	      sgml-any
	    (sgml-make-primitive-content-token
	     (sgml-eltype-token
	      (sgml-lookup-eltype sgml-document-element dtd)))))
    (setq sgml-buffer-parse-state
	  (sgml-make-pstate dtd
			    (sgml-make-tree top-type
					    0 0 nil 0 nil nil nil nil nil)))))

(defun sgml-set-parse-state (tree where)
  "Set parse state from TREE, either from start of TREE if WHERE is start
or from after TREE if WHERE is after."
  (setq sgml-current-tree tree
	sgml-markup-tree tree)
  (let ((empty
	 (sgml-element-empty tree)))
    (cond ((and (eq where 'start)
		(not empty))
	   (setq sgml-current-state (sgml-element-model sgml-current-tree)
		 sgml-current-shortmap (sgml-tree-shortmap sgml-current-tree)
		 sgml-previous-tree nil)
	   (setq sgml-markup-type
		 (if (and (not (zerop (sgml-tree-stag-len tree)))
			  (sgml-bpos-p (sgml-tree-stag-epos tree)))
		     'start-tag)
		 sgml-markup-start (sgml-element-start sgml-current-tree))
	   (goto-char (+ sgml-markup-start
			 (sgml-tree-stag-len sgml-current-tree))))
	  (t
	   (setq sgml-current-state (sgml-tree-pstate sgml-current-tree)
		 sgml-current-shortmap (sgml-tree-pshortmap sgml-current-tree)
		 sgml-previous-tree sgml-current-tree)
	   (goto-char (sgml-tree-end sgml-current-tree))
	   (setq sgml-markup-type (if empty 'start-tag 'end-tag)
		 sgml-markup-start (- (point)
				      (sgml-tree-etag-len sgml-current-tree)))
	   (setq sgml-current-tree (sgml-tree-parent sgml-current-tree))))
    (assert sgml-current-state)))

(defsubst sgml-final-p (state)
  ;; Test if a state/model can be ended
  (or (not (sgml-model-group-p state))
      (sgml-final state)))

;(defun sgml-current-element-contains-data ()
;  "Retrun true if the current open element is either mixed or is (r)cdata."
;  (or (eq sgml-cdata sgml-current-state)
;      (eq sgml-rcdata sgml-current-state)
;      (sgml-current-mixed-p)))

;(defun sgml-current-element-content-class ()
;  "Return a string describing the type of content in the current element.
;The type can be CDATA, RCDATA, ANY, #PCDATA or none."
;  (cond ((eq sgml-cdata sgml-current-state)
;	 "CDATA")
;	((eq sgml-rcdata sgml-current-state)
;	 "RCDATA")
;	((eq sgml-any sgml-current-state)
;	 "ANY")
;	((sgml-current-mixed-p)
;	 "#PCDATA")
;	(t "")))

(defun sgml-promoted-epos (start end)
  "Return an entity position for start of region START END.
If region is empty, choose return an epos as high in the 
entity hierarchy as possible."
  (let ((epos (sgml-epos start)))
    (when (= start end)
      (while (and (sgml-strict-epos-p epos)
		  (= 1 (sgml-epos-pos epos)))
	(setq epos (sgml-eref-start (sgml-epos-eref epos)))))
    epos))

(defun sgml-open-element (eltype conref before-tag after-tag)
  (let* ((emap (sgml-eltype-shortmap eltype))
	 (newmap (if emap
		     (if (eq 'empty emap)
			 nil
		       (sgml-lookup-shortref-map
			(sgml-dtd-shortmaps sgml-dtd-info)
			emap))
		   sgml-current-shortmap))
	 (nt (sgml-make-tree
	      eltype
	      (sgml-promoted-epos before-tag after-tag) ; stag-epos
	      (- after-tag before-tag)	; stag-len
	      sgml-current-tree		; parent
	      (1+ (sgml-tree-level sgml-current-tree)) ; level
	      (append (sgml-eltype-excludes eltype) (sgml-excludes))
	      (append (sgml-eltype-includes eltype) (sgml-includes))
	      sgml-current-state
	      (if (sgml-tree-net-enabled sgml-current-tree) 1)
	      conref
	      newmap
	      sgml-current-shortmap)))
;; (let ((u (sgml-tree-content sgml-current-tree)))
;;      (cond ((and u (> before-tag (sgml-element-start u)))
;;	     (while (and (sgml-tree-next u)
;;			 (> before-tag
;;			    (sgml-element-start (sgml-tree-next u))))
;;	       (setq u (sgml-tree-next u)))
;;	     (setf (sgml-tree-next u) nt))
;;	    (t
;;	     (setf (sgml-tree-content sgml-current-tree) nt))))
    ;; Install new node in tree
    (cond (sgml-previous-tree
	   (setf (sgml-tree-next sgml-previous-tree) nt))
	  (t
	   (setf (sgml-tree-content sgml-current-tree) nt)))
    ;; Prune tree
    ;; *** all the way up?  tree-end = nil?
    (setf (sgml-tree-next sgml-current-tree) nil)
    ;; Set new state
    (setq sgml-current-state (sgml-eltype-model eltype)
	  sgml-current-shortmap newmap
	  sgml-current-tree nt
	  sgml-previous-tree nil)
    (assert sgml-current-state)
    (setq sgml-markup-tree sgml-current-tree)
    (when (sgml-element-empty sgml-current-tree)
      (sgml-close-element after-tag after-tag))))

(defun sgml-fake-open-element (tree el)
  (sgml-make-tree
   el 0 0 
   tree
   0
   (append (sgml-eltype-excludes el) (sgml-tree-excludes tree))
   (append (sgml-eltype-includes el) (sgml-tree-includes tree))
   nil 
   nil
   nil))

(defun sgml-close-element (before-tag after-tag)
  (when (or (eq sgml-close-element-trap t)
	    (eq sgml-close-element-trap sgml-current-tree))
    (setq sgml-goal (point)))
  (setf (sgml-tree-etag-epos sgml-current-tree)
	(sgml-promoted-epos before-tag after-tag))
  (setf (sgml-tree-etag-len sgml-current-tree) (- after-tag before-tag))
  (run-hooks 'sgml-close-element-hook)
  (setq sgml-markup-tree sgml-current-tree)
  (cond ((eq sgml-current-tree sgml-top-tree)
	 (unless (eobp)
	   (sgml-error "Parse ended")))
	(t
	 (setq sgml-previous-tree sgml-current-tree
	       sgml-current-state (sgml-tree-pstate sgml-current-tree)
	       sgml-current-shortmap (sgml-tree-pshortmap sgml-current-tree)
	       sgml-current-tree (sgml-tree-parent sgml-current-tree))
	 (assert sgml-current-state))))

(defun sgml-note-change-at (at &optional end)
  ;; Inform the cache that there have been some changes after AT
  (when sgml-buffer-parse-state
    (let ((u (sgml-pstate-top-tree sgml-buffer-parse-state)))
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
	       ;; tag no pruning is done.  If the end of the element is
	       ;; implied changing the tag that implied it may change
	       ;; the extent of the element.
	       ((and (sgml-tree-etag-epos u)	
		     (> at (sgml-tree-end u))
		     (or (> (sgml-tree-etag-len u) 0)
			 (sgml-element-empty u)))
		nil) 
	       (t
		(setf (sgml-tree-etag-epos u) nil)
		(cond;; Enter into content if change is clearly in it
		 ((and (sgml-tree-content u)
		       (> at (sgml-element-stag-end (sgml-tree-content u))))
		  (setq u (sgml-tree-content u)))
		 ;; Check if element has no start tag,
		 ;; then it must be pruned as a change could create
		 ;; a valid start tag for the element.
		 ((and (zerop (sgml-tree-stag-len u))
		       (> at (sgml-element-start u)))
		  ;; restart from to with new position
		  ;; this can't loop forever as
		  ;; position allways gets smaler
		  (setq at (sgml-element-start u)
			u sgml-top-tree))
		 (t
		  (setf (sgml-tree-content u) nil))))))))))))



(defun sgml-eltypes-in-state (tree state)
  "Return list of symbols valid in STATE and TREE.
The symbols are the tokens used in the DFAs."
  (let* ((req (if (sgml-model-group-p state)
		  (sgml-required-tokens state)))
	 (elems
	  (if (sgml-model-group-p state)
	      (nconc req
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
    ;; Check for omitable start-tags
    (when (and sgml-omittag-transparent
	       (not (sgml-final-p state))
	       req
	       (null (cdr req)))
      (let ((et (sgml-lookup-eltype (car req))))
	(when (sgml-eltype-stag-optional et)
	  (setq elems
		(nconc elems		; *** possibility of duplicates
		       (sgml-eltypes-in-state
			(sgml-fake-open-element tree et)
			(sgml-eltype-model et)))))))
    elems))

(defun sgml-current-list-of-valid-eltypes ()
  "Returns a list of contextually valid element types (eltype)."
  (let ((elems (sgml-eltypes-in-state sgml-current-tree sgml-current-state))
	(tree sgml-current-tree)
	(state sgml-current-state))
    (when sgml-omittag-transparent
      (while (and tree
		  (sgml-final-p state)
		  (sgml-element-etag-optional tree))
	(setq state (sgml-tree-pstate tree)
	      tree (sgml-tree-parent tree))
	(loop for e in (sgml-eltypes-in-state tree state) do
	      (when (not (memq e elems))
		(setq elems (nconc elems (list e)))))))
    ;; *** Filter out elements that are undefined?
    (sort elems (function string-lessp))))

(defun sgml-current-list-of-endable-eltypes ()
  "Return a list of the element types endable in current state."
  (let* ((elems nil)
	 (tree sgml-current-tree)
	 (state sgml-current-state))
    (while
	(and (sgml-final-p state)
	     (not (eq tree sgml-top-tree))
	     (progn
	       (setq elems
		     (nconc elems (list (sgml-tree-eltype tree)))) 
	       sgml-omittag)
	     (sgml-eltype-etag-optional (sgml-tree-eltype tree)))
      (setq state (sgml-tree-pstate tree)
	    tree (sgml-tree-parent tree)))
    elems))

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
      (sgml-log-message "Line %s in %S "
			(count-lines (point-min) (point)) sgml-parameter-name))
    (sgml-pop-entity))
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

;;;; Parsing delimiters

(eval-and-compile
  (defconst sgml-delimiters
    '("AND"   "&"
      "COM"   "--"
      "CRO"   "&#"
      "DSC"   "]"
      "DSO"   "["
      "DTGC"  "]"
      "DTGO"  "["
      "ERO"   "&"
      "ETAGO" "</"
      "GRPC"  ")"
      "GRPO"  "("
      "LIT"   "\""
      "LITA"  "'"
      "MDC"   ">"
      "MDO"   "<!"
      "MINUS" "-"
      "MSC"   "]]"
      "NET"   "/"
      "OPT"   "?"
      "OR"    "|"
      "PERO"  "%"
      "PIC"   ">"
      "PIO"   "<?"
      "PLUS"  "+"
      "REFC"  ";"
      "REP"   "*"
      "RNI"   "#"
      "SEQ"   ","
      "STAGO" "<"
      "TAGC"  ">"
      "VI"    "="
      ;; Some combinations
      "MS-START" "<!["			; MDO DSO
      "MS-END"   "]]>"			; MSC MDC
      ;; Pseudo
      "NULL"  ""
      )))


(defmacro sgml-is-delim (delim &optional context move offset)
  "Macro for matching delimiters.
Syntax: DELIM &optional CONTEXT MOVE
where DELIM is the delimiter name (string or symbol), 
CONTEXT the contextual constraint, and
MOVE is `nil', `move' or `check'.

Test if the text following point in current buffer matches the SGML
delimiter DELIM.  Also check the characters after the delimiter for
CONTEXT.  Applicable values for CONTEXT is 
`gi' -- name start or TAGC if SHORTTAG YES,
`com' -- if COM or MDC,
`nmstart' -- name start character, 
`stagc' -- TAGC if SHORTTAG YES,
`digit' -- any Digit,
string -- delimiter with that name,
list -- any of the contextual constraints in the list."

  (or offset (setq offset 0))
  (let ((ds (member (upcase (format "%s" delim))
		    sgml-delimiters)))
    (assert ds)
    (setq delim (car ds)
	  ds (cadr ds))
    (cond ((eq context 'gi)
	   (setq context '(nmstart stagc)))
	  ((eq context 'com)
	   (setq context '("COM" "MDC")))
	  ((null context)
	   (setq context '(t)))
	  ((not (listp context))
	   (setq context (list context))))
    (`(if (and				; This and checks that characters
					; of the delimiter
	   (,@(loop for i from 0 below (length ds) collect
		    (` (eq (, (aref ds i))
			   (sgml-following-char (, (+ i offset)))))))
	   (or
	    (,@(loop
		for c in context collect ; context check
		(cond			
		 ((eq c 'nmstart)	; name start character
		  (`(sgml-startnm-char
		     (or (sgml-following-char (, (length ds))) 0))))
		 ((eq c 'stagc)
		  (`(and sgml-current-shorttag
			 (sgml-is-delim "TAGC" nil nil (, (length ds))))))
		 ((eq c 'digit)
		  (`(memq (sgml-following-char (, (length ds)))
			  '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))))
		 ((stringp c)
		  (`(sgml-is-delim (, c) nil nil (, (length ds)))))
		 ((eq c t))
		 (t (error "Context type: %s" c))))
	       )))
	  
	  (progn			; Do operations if delimiter found
	    (,@ (if move (`((forward-char (, (length ds)))))))
	    (,@ (if (not (eq move 'check))
		    '(t))))
	(,@ (if (eq move 'check)
		(`((sgml-delimiter-parse-error (, delim))))))))))

(defmacro sgml-following-char (n)
  (cond ((zerop n)  '(following-char))
	((= n 1)    '(char-after (1+ (point))))
	(t          (` (char-after (+ (, n) (point)))))))

(defun sgml-delimiter-parse-error (delim)
  (sgml-parse-error "Delimiter %s (%s) expected"
		    delim (cadr (member delim sgml-delimiters))))

(defmacro sgml-parse-delim (delim &optional context)
  (`(sgml-is-delim (, delim) (, context) move)))

(defmacro sgml-check-delim (delim &optional context)
  (`(sgml-is-delim (, delim) (, context) check)))

(defmacro sgml-skip-upto (delim)
  "Skip until the delimiter or first char of one of the delimiters.
If DELIM is a string/symbol this is should be a delimiter role.
Characters are skipped until the delimiter is recognized.
If DELIM is a list of delimiters, skip until a character that is first
in any of them."
  (cond
   ((consp delim)
    (list 'skip-chars-forward
	  (concat "^"
		  (loop for d in delim
			concat (let ((ds (member (upcase (format "%s" d))
						 sgml-delimiters)))
				 (assert ds)
				 (let ((s (substring (cadr ds) 0 1)))
				   (if (member s '("-" "\\"))
				       (concat "\\" s)
				     s)))))))
   (t
    (let ((ds (member (upcase (format "%s" delim))
		      sgml-delimiters)))
      (assert ds)
      (setq ds (cadr ds))
      (if (= 1 (length ds))
	  (list 'skip-chars-forward (concat "^" ds))
	(`(and (search-forward (, ds) nil t)
	       (backward-char (, (length ds))))))))))


;;(macroexpand '(sgml-is-delim mdo))
;;(macroexpand '(sgml-parse-delim mdo))
;;(macroexpand '(sgml-check-delim mdo))


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

(defun sgml-check-char (char)
  (cond ((not (sgml-parse-char char))
	 (sgml-parse-error "Expecting %c" char))))

(defun sgml-parse-RE ()
  (or (sgml-parse-char ?\n)
      (sgml-parse-char ?\r)))

(defmacro sgml-startnm-char (c)
  (` (eq ?w (char-syntax (, c)))))

(defun sgml-startnm-char-next ()
  (and (not (eobp))
       (sgml-startnm-char (following-char))))

(defun sgml-name-char (c)
  (and c
       (or (sgml-startnm-char c)
	   (eq ?_ (char-syntax c)))))

(defun sgml-is-end-tag ()
  (sgml-is-delim "ETAGO" nmstart))

(defun sgml-is-enabled-net ()
  (and sgml-current-shorttag
       (sgml-tree-net-enabled sgml-current-tree)
       (sgml-is-delim "NET")))

(defun sgml-is-start-tag ()
  (sgml-is-delim "STAGO" gi))

(defun sgml-parse-s (&optional shortmap)
  (if shortmap
      (or (/= 0 (skip-chars-forward " "))
	  (/= 0 (skip-chars-forward "\t"))
	  (sgml-parse-char ?\n)
	  (sgml-parse-char ?\r))
    (/= 0 (skip-chars-forward " \t\n\r"))))

(defun sgml-parse-processing-instruction ()
  (if (sgml-parse-delim "PIO")
      (progn (sgml-skip-upto "PIC")
	     (sgml-check-delim "PIC")
	     (sgml-set-markup-type 'pi)
	     t)))

(defmacro sgml-general-case (string)  (`(downcase (, string))))
(defmacro sgml-entity-case (string)   string)

(defun sgml-parse-name (&optional entity-name)
  (if (sgml-startnm-char-next)
      (let ((name (buffer-substring (point)
				    (progn (skip-syntax-forward "w_")
					   (point)))))
	(if entity-name
	    (sgml-entity-case name)
	  (sgml-general-case name)))))

(defun sgml-check-name (&optional entity-name)
  (or (sgml-parse-name entity-name)
      (sgml-parse-error "Name expected")))

(defun sgml-parse-nametoken (&optional entity-name)
  "Parses a name token and returns a string or nil if no nametoken."
  (if (sgml-name-char (following-char))
      (let ((name (buffer-substring (point)
				    (progn (skip-syntax-forward "w_")
					   (point)))))
	(if entity-name
	    (sgml-entity-case name)
	  (sgml-general-case name)))))

(defun sgml-check-nametoken ()
  (or (sgml-parse-nametoken)
      (sgml-parse-error "Name token expected")))

(defun sgml-gname-symbol (string)
  "Convert a string to a general name/nametoken/numbertoken."
  (intern (sgml-general-case string)))

(defun sgml-ename-symbol (string)
  "Convert a string to an entity name."
  (intern (sgml-entity-case string)))

(defun sgml-parse-general-entity-ref ()
  (if (sgml-parse-delim "ERO" nmstart)
      (let* ((name (sgml-parse-name t))
	     (ent (sgml-lookup-entity name
				      (sgml-dtd-entities sgml-dtd-info))))
	(or (sgml-parse-delim "REFC")
	    (sgml-parse-char ?\n))
	(sgml-set-markup-type 'entity)
	(cond ((null ent)
	       (sgml-log-warning
		"Undefined entity %s" name))
	      ((sgml-entity-data-p ent)
	       (sgml-pcdata-move))
	      (t
	       (sgml-push-to-entity ent sgml-markup-start)))
	t)))

(defun sgml-parse-parameter-entity-ref ()
  "Parse and push to a parameter entity, return nil if no ref here."
  ;;(setq sgml-markup-start (point))
  (if (sgml-parse-delim "PERO" nmstart)
      (let* ((name (sgml-parse-name t))
	     (ent (sgml-lookup-entity name
				      (sgml-dtd-parameters sgml-dtd-info))))
	(or (sgml-parse-delim "REFC")
	    (sgml-parse-char ?\n))
	;;(sgml-set-markup-type 'param)
	(cond (ent
	       (sgml-push-to-entity ent))
	      (t
	       (sgml-log-warning
		"Undefined parameter entity %s" name)))
	t)))

(defun sgml-parse-comment ()
  (if (sgml-parse-delim "COM")
      (progn (sgml-skip-upto "COM")
	     (sgml-check-delim "COM")
	     t)))

(defun sgml-skip-ps ()
  "Move point forward stopping before a char that isn't a parameter separator."
  (while
      (or (sgml-parse-s)
	  (if (eobp) (sgml-pop-entity))
	  (sgml-parse-parameter-entity-ref)
	  (sgml-parse-comment))))

(defun sgml-parse-ds ()
;71  ds   = 5 s | EE | 60+ parameter entity reference
;         | 91 comment declaration
;         | 44 processing instruction
;         | 93 marked section declaration ***
  (or (and (eobp) (sgml-pop-entity))	;EE
      (sgml-parse-s)			;5 s
      ;;(sgml-parse-comment-declaration)	;91 comment declaration
      (sgml-parse-parameter-entity-ref)
      (sgml-parse-processing-instruction)))

(defun sgml-skip-ds ()
  (while (sgml-parse-ds)))

(defmacro sgml-parse-rni (&optional name)
  "Parse a RNI (#) return nil if none; with optional NAME, 
a RNI must be followed by NAME."
  (cond
   (name
    (` (if (sgml-parse-delim "RNI")
	   (sgml-check-token (, name)))))
   (t '(sgml-parse-delim "RNI"))))

(defun sgml-check-token (name)
  (or (equal (sgml-check-name) name)
      (sgml-parse-error "Reserved name not expected")))

(defun sgml-parse-literal ()
  "Parse a literal and return a string, if no literal return nil."
  (let (lita start value)
    (cond ((or (sgml-parse-delim "LIT")
	       (setq lita (sgml-parse-delim "LITA")))
	   (setq start (point))
	   (if lita
	       (sgml-skip-upto "LITA")
	     (sgml-skip-upto "LIT"))
	   (setq value (buffer-substring start (point)))
	   (if lita
	       (sgml-check-delim "LITA")
	     (sgml-check-delim "LIT"))
	   value))))

(defun sgml-parse-minimum-literal ()
  "Parse a quoted SGML string and return it, if no string return nil."
  (cond
   ((memq (following-char) '(?\" ?\'))
    (let* ((qchar (following-char))
	   (blanks " \t\r\n")
	   (qskip (format "^%s%c" blanks qchar))
	   (start (point))
	   (value			; accumulates the literal value
	    "")
	   (spaced ""))
      (forward-char 1)
      (skip-chars-forward blanks)
      (while (not (sgml-parse-char qchar))
	(cond ((eobp)
	       (goto-char start)
	       (sgml-parse-error "Unterminated literal"))
	      ((sgml-parse-s)
	       (setq spaced " "))
	      (t
	       (setq value
		     (concat value spaced
			     (buffer-substring
			      (point)
			      (progn (skip-chars-forward qskip)
				     (point))))
		     spaced ""))))
      value))))

(defun sgml-skip-cdata ()
  "Move point forward until there is a end-tag open after point."
  (while (progn (skip-chars-forward "^</")
		(not (or (sgml-is-end-tag)
			 (sgml-is-enabled-net))))
    (forward-char 1)))

(defun sgml-skip-tag ()
  (when (sgml-parse-char ?<)
    (sgml-parse-char ?/)
    (unless (search-forward-regexp
	       "\\([^\"'<>/]\\|\"[^\"]*\"\\|'[^']*'\\)*"
	       nil t)
      (sgml-error "Invalid tag"))
    (or (sgml-parse-char ?>)
	(sgml-parse-char ?/))))


;;;; Shortref maps

(eval-and-compile
  (defconst sgml-shortref-list
    '(
      "\t"				;&#TAB
      "\n"				;&#RE;
      "\001"				;&#RS;
      "\001B"
      "\001\n"
      "\001B\n"
      "B\n"
      " "				;&#SPACE;
      "BB"
      "\""				;&#34;
      "#"
      "%"
      "'"
      "("
      ")"
      "*"
      "+"
      ","
      "-"
      "--"
      ":"
      ";"
      "="
      "@"
      "["
      "]"
      "^"
      "_"
      "{"
      "|"
      "}"
      "~")))

(eval-and-compile
  (defun sgml-shortref-index (string)
    (let ((pos (member string sgml-shortref-list))
	  (len (length sgml-shortref-list)))
      (and pos (- len (length pos))) )))

(defun sgml-make-shortmap (pairs)
  "Create a shortreference map from PAIRS.
Where PAIRS is a list of (delim . ename)."
  (let ((map
	 (make-vector (1+ (length sgml-shortref-list))
		      nil))
	index)
    (loop for p in pairs 
	  for delim = (car p)
	  for name = (cdr p)
	  do
	  (setq index (sgml-shortref-index delim))
	  (cond ((null index)
		 (sgml-log-warning
		  "Illegal short reference delimiter '%s'" delim))
		(t
		 (aset map index name))))
    ;; Compute a suitable string for skip-chars-forward that
    ;; can be used to skip over pcdata
    (aset map
	  (eval-when-compile (length sgml-shortref-list))
	  (if (some (function
		     (lambda (r) (aref map (sgml-shortref-index r))))
		    '("\001B\n" "B\n" " " "BB"))
	      "^<]/& \n\t\"#%'()*+,\\-:;+@[]\\^_{|}~"
	    "^<]/&\n\t\"#%'()*+,\\-:;+@[]\\^_{|}~"))
    map))

(defun sgml-shortmap-skipstring (map)
  (aref map (eval-when-compile (length sgml-shortref-list))))

(defconst sgml-shortref-oneassq
  (loop for d in sgml-shortref-list
	for c = (aref d 0)
	when (and (= 1 (length d))
		  (/= 1 c) (/= 10 c))
	collect (cons c (sgml-shortref-index d))))

(defun sgml-parse-B ()
  (/= 0 (skip-chars-forward " \t")))

(defun sgml-deref-shortmap (map &optional nobol)
  "Identify shortref delimiter at point and return entity name.
Also move point.  Return nil, either if no shortref or undefined."

  (macrolet
      ((delim (x) (` (aref map (, (sgml-shortref-index x))))))
    (let ((i 0))
      (while (numberp i)
	(setq i
	      (cond
	       ((and (bolp) (zerop i)) ; Either "\001" "\001B"
					; "\001\n" "\001B\n"
		(cond ((sgml-parse-B)	; "\001B"
		       (if (eolp)
			   (delim "\001B\n")
			 (delim "\001B")))
		      ((sgml-parse-RE) (delim "\001\n"))
		      ((delim "\001"))
		      (t 1)))
	       ((cond ((sgml-parse-char ?\t) (setq i (delim "\t")) t)
		      ((sgml-parse-char ? )  (setq i (delim " "))  t))
		(cond ((sgml-parse-B) (setq i (delim "BB"))))
		(cond ((sgml-parse-char ?\n) 
		       (delim "B\n"))
		      (t i)))
	       ((sgml-parse-RE) (delim "\n"))
	       ((sgml-parse-chars ?- ?-) (delim "--"))
	       ;; The other one character delimiters
	       ((setq i (assq (following-char) sgml-shortref-oneassq))
		(when i (forward-char 1))
		(aref map (cdr i))))))
      i)))

;;; Table of shortref maps

(defun sgml-make-shortref-table ()
  (list nil))

(defun sgml-add-shortref-map (table name map)
  (nconc table (list (cons name map))))

(defun sgml-lookup-shortref-map (table name)
  (cdr (assoc name (cdr table))))


;;;; Parse markup declarations

(defun sgml-skip-until-dsc ()
  (while (progn
	   (sgml-skip-upto ("DSO" "DSC" "LITA" "LIT" "COM"))
	   (not (sgml-parse-delim "DSC")))
    (cond ((sgml-parse-literal))
	  ((sgml-parse-delim "DSO")
	   (sgml-skip-until-dsc))
	  ((sgml-parse-comment))
	  (t (forward-char 1)))))

(defun sgml-skip-upto-mdc ()
  "Move point forward until end of current markup declaration.
Assumes starts with point inside a markup declaration."
  (while (progn
	   (sgml-skip-upto ("DSO" "MDC" "LIT" "LITA" "COM")) 
	   (not (sgml-is-delim "MDC")))
    (cond ((sgml-parse-delim "DSO")
	   (sgml-skip-until-dsc))
	  ((sgml-parse-literal))
	  ((sgml-parse-comment))
	  (t (forward-char 1)))))

(defun sgml-do-sgml-declaration ()
  (sgml-skip-upto-mdc)
  (setq sgml-markup-type 'sgml))

(defun sgml-do-doctype ()
  (let (sgml-markup-start)
    (cond ((or (null sgml-dtd-info)
	       (equal (sgml-dtd-doctype sgml-dtd-info) "ANY"))
					; Parse the DOCTYPE
	   (sgml-check-doctype-body))
	  (t				; Has doctype already been defined
	   (sgml-skip-upto-mdc))))
  (setq sgml-markup-type 'doctype))

(defun sgml-do-marked-section ()
  (let ((status nil))
    (while (progn (sgml-skip-ps)
		  (not (sgml-parse-char ?\[)))
      (push (sgml-check-name)
	    status))
    (cond
     ((member "ignore" status)
      (sgml-skip-marked-section)
      (sgml-set-markup-type 'ignored))
     ((or (member "cdata" status)
	  (member "rcdata" status))
      (or (search-forward "]]>" nil t)
	  (sgml-error "CDATA marked section not terminated"))
      (sgml-set-markup-type sgml-cdata))
     (t
      (sgml-set-markup-type 'ms-start)))))
  
(defun sgml-skip-marked-section ()
  (while (progn
	   (sgml-skip-upto ("MS-START" "MS-END"))
	   (when (eobp) (sgml-error "Marked section unterminated"))
	   (not (sgml-parse-delim "MS-END")))
    (cond ((sgml-parse-delim "MS-START")
	   ;;(search-forward "[")
	   (sgml-skip-marked-section))
	  (t (forward-char 1)))))

(defun sgml-do-usemap ()
  (let (mapname associated)
    ;;(setq sgml-markup-type 'usemap)
    (unless (sgml-parse-rni "empty")
      (setq mapname (sgml-check-name)))
    (sgml-skip-ps)
    (cond
     ((sgml-is-delim "MDC")
      (sgml-debug "USEMAP %s" (if mapname mapname "#EMPTY"))
      (setq sgml-current-shortmap
	    (if mapname
		(or (sgml-lookup-shortref-map
		     (sgml-dtd-shortmaps sgml-dtd-info)
		     mapname)
		    (sgml-error "Undefined shortref map %s" mapname)))))
     (t
      ;; Should be handled by psgml-dtd
      (sgml-do-usemap-element mapname)))))

(defconst sgml-markup-declaration-table
  '(("sgml"     . sgml-do-sgml-declaration)
    ("doctype"  . sgml-do-doctype)
    ("element"  . sgml-declare-element)
    ("entity"   . sgml-declare-entity)
    ("usemap"   . sgml-do-usemap)
    ("shortref" . sgml-declare-shortref)
    ("notation" . sgml-declare-notation)
    ("attlist"  . sgml-declare-attlist)
    ("uselink"  . sgml-skip-upto-mdc)
    ("linktype" . sgml-skip-upto-mdc)
    ("link"     . sgml-skip-upto-mdc)
    ("idlink"   . sgml-skip-upto-mdc)
    ))

(defun sgml-parse-markup-declaration (option)
  "Parse a markup declartion.
OPTION can be `prolog' if parsing the prolog or `dtd' if parsing the
dtd or `ignore' if the declaration is to be ignored."
  (cond
   ((sgml-parse-delim "MDO" (nmstart "COM" "MDC"))
    (cond
     ((sgml-startnm-char-next)
      (setq sgml-markup-type nil)
      (let* ((tok (sgml-parse-nametoken))
	     (rut (assoc tok sgml-markup-declaration-table)))
	(when (and (not (memq option '(prolog ignore)))
		   (member tok '("sgml" "doctype")))
	  (sgml-error "%s declaration is only valid in prolog" tok))
	(when (and (not (memq option '(dtd ignore)))
		   (member tok '("element" "entity" "attlist" "notation" 
				 "shortref")))
	  (sgml-error "%s declaration is only valid in doctype" tok))
	(cond ((eq option 'ignore)
	       (sgml-skip-upto-mdc))
	      (rut (sgml-skip-ps)
		   (funcall (cdr rut)))
	      (t (sgml-parse-error
		  "Illegal markup declaration %s" tok)))))
     (t
      (setq sgml-markup-type 'comment)))
    (sgml-skip-ps)
    (sgml-check-delim "MDC")
    (unless (eq option 'ignore)		; Set the markup type given
      (when sgml-markup-type
	(sgml-set-markup-type sgml-markup-type)))
    t)
   ((sgml-parse-delim "MS-START")
    (sgml-do-marked-section))))


;;;; Parsing attribute values

(defvar sgml-conref-flag nil
  "This variable is set by `sgml-parse-attribute-specification-list'
if a CONREF attribute is parsed.")

(defun sgml-parse-attribute-specification-list (&optional eltype)
  "Parse an attribute specification list.
Optional argument ELTYPE, is used to resolve omitted name=.
Returns a list of attspec (attribute specification)."
  (setq sgml-conref-flag nil)
  (let ((attlist (if eltype (sgml-eltype-attlist eltype)))
	name val asl attdecl)
    (while (setq name (progn (sgml-parse-s)
			     (sgml-parse-nametoken)))
      (sgml-parse-s)
      (cond ((sgml-parse-delim "VI")
	     (sgml-parse-s)
	     (setq val (sgml-check-attribute-value-specification))
	     (when eltype
	       (or (setq attdecl (sgml-lookup-attdecl name attlist))
		   (sgml-log-warning
		    "Attribute %s not declared for element %s"
		    name (sgml-eltype-name eltype)))))
	    ((null eltype)
	     (sgml-parse-error "Expecting a ="))
	    ((progn
	       (unless sgml-current-shorttag
		 (sgml-log-warning
		  "Must have attribute name when SHORTTAG NO"))
	       (setq attdecl
		     (sgml-find-attdecl-for-value (setq val name)
						  eltype))))
	    (t
	     (sgml-log-warning
	      "%s is not in any name group for element %s."
	      val
	      (sgml-eltype-name eltype))))
      ;; *** What happens when eltype is nil ??
      (when attdecl
	(push (sgml-make-attspec (sgml-attdecl-name attdecl) val)
	      asl)
	(when (sgml-default-value-type-p 'conref
					 (sgml-attdecl-default-value attdecl))
	  (setq sgml-conref-flag t))))
    asl))

(defun sgml-check-attribute-value-specification ()
  (or (sgml-parse-literal)
      (sgml-parse-nametoken t)		; Not really a nametoken, but an
					; undelimited literal
      (sgml-parse-error "Expecting an attribute value: literal or token")))

(defun sgml-find-attdecl-for-value (value eltype)
  "Find the attribute declaration of ELTYPE that has VALUE in its name group.
VALUE is a string.  Returns nil or an attdecl."
  (let ((al (sgml-eltype-attlist eltype))
	dv)
    (while (and al
		(or (atom (setq dv (sgml-attdecl-declared-value (car al))))
		    (not (member value
				 (sgml-declared-value-token-group dv)))))
      (setq al (cdr al)))
    (if al (car al))))

;¤¤\end{codeseg}


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
;; the partial parse tree.  The primitive parse operation is to parse
;; until a goal point in the buffer has been passed.  In addition to
;; this it is possible to "trap" closing of elements.  Either for a
;; specific element or for any element.  When the trap is sprung the
;; parse is ended.  This is used to extend the parse tree.  When the
;; trap is used the parser is usually called with the end of the
;; buffer as the goal point.

(defun sgml-need-dtd ()
  "Make sure that an eventual DTD is parsed or loaded."
  (when (null sgml-buffer-parse-state)	; first parse in this buffer
    (sgml-set-initial-state)		; fall back DTD
    (add-hook 'pre-command-hook 'sgml-reset-log)
    (if sgml-default-dtd-file
	(sgml-load-dtd sgml-default-dtd-file)
      (let ((buf (and sgml-parent-document
		      (find-file-noselect (if (consp sgml-parent-document)
					      (car sgml-parent-document)
					    sgml-parent-document)))))
	(if (or (null buf) (eq buf (current-buffer)))
	    (save-excursion (sgml-parse-prolog))
	  ;; get DTD from parent document
	  (let (dtd)
	    (save-excursion
	      (set-buffer buf)
	      (sgml-need-dtd)
	      (setq dtd (sgml-pstate-dtd sgml-buffer-parse-state)))
	    (when (consp sgml-parent-document)
	      (setq dtd (copy-sgml-dtd dtd))
	      (let ((doctypename (second sgml-parent-document)))
		(setf (sgml-dtd-doctype dtd)
		      (sgml-general-case
		       (if (symbolp doctypename)
			   (symbol-name doctypename)
			  doctypename)))))
	    (sgml-set-initial-state dtd))))))
  (setq sgml-dtd-info (sgml-pstate-dtd sgml-buffer-parse-state)
	sgml-top-tree (sgml-pstate-top-tree sgml-buffer-parse-state))
  (sgml-set-global))

(defun sgml-set-global ()
  (setq sgml-current-omittag sgml-omittag
	sgml-current-shorttag sgml-shorttag)
  (aset sgml-current-entity-map 0 sgml-local-catalogs)
  (aset sgml-current-entity-map 1 sgml-catalog-files)
  (aset sgml-current-entity-map 2 sgml-system-path)
  (aset sgml-current-entity-map 3 sgml-public-map))


(defun sgml-parse-until-end-of (sgml-close-element-trap &optional extra-cond)
  "Parse until the SGML-CLOSE-ELEMENT-TRAP has ended,
or if it is t, any additional element has ended,
or if nil, until end of buffer."
  (sgml-parse-to (point-max) extra-cond)
  (when (eobp)				; End of buffer, can imply
					; end of any open element.
    (while (prog1 (not
		   (or (eq sgml-close-element-trap t)
		       (eq sgml-close-element-trap sgml-current-tree)
		       (eq sgml-current-tree sgml-top-tree)))
	     (sgml-implied-end-tag "buffer end" (point) (point))))))

(defun sgml-parse-to (sgml-goal &optional extra-cond quiet)
  "Parse until (at least) SGML-GOAL.
Optional argument EXTRA-COND should be a function.  This function is 
called in the parser loop, and the loop is exited if the function returns t.
If third argument QUIT is non-nil, no \"Parsing...\" message will be displayed."
  (sgml-need-dtd)
  (sgml-find-start-point (min sgml-goal (point-max)))
  (assert sgml-current-tree)
  (let ((bigparse (and (not quiet) (> (- sgml-goal (point)) 10000))))
    (when bigparse
      (sgml-message "Parsing..."))
    (sgml-with-parser-syntax
     (sgml-parser-loop extra-cond))
    (when bigparse
      (sgml-message ""))))

(defun sgml-reparse-buffer (shortref-fun)
  "Reparse the buffer and let SHORTREF-FUN take care of short references.
SHORTREF-FUN is called with the entity as argument and `sgml-markup-start'
pointing to start of short ref and point pointing to the end."
  (sgml-note-change-at (point-min))
  (let ((sgml-shortref-handler shortref-fun))
    (sgml-parse-until-end-of nil)))

(defun sgml-pcdata-move ()
  "Moify parser state to reflect parsed data."
  (let (new-state)
    (while				; Until token accepted
	(cond
	 ((eq sgml-current-state sgml-any) nil)
	 ((setq new-state
		(sgml-get-move sgml-current-state sgml-pcdata-token))
	  (setq sgml-current-state new-state)
	  nil)
	 ((sgml-do-implied "data character"))))))

(defun sgml-parse-pcdata ()
  (/= 0
      (if sgml-current-shortmap
	  (skip-chars-forward (sgml-shortmap-skipstring sgml-current-shortmap))
	(skip-chars-forward "^<]/&"))))

(defsubst sgml-do-pcdata ()
  ;; Parse pcdata
  (sgml-pcdata-move)
  (forward-char 1)
  (sgml-parse-pcdata)
  (sgml-set-markup-type nil))

(defun sgml-parser-loop (extra-cond)
  (let (tem)
    (while (and (eq sgml-current-tree sgml-top-tree)
		(or (< (point) sgml-goal) sgml-current-eref)
		(progn (setq sgml-markup-start (point)
			     sgml-markup-type nil)
		       (or (sgml-parse-s)
			   (sgml-parse-markup-declaration 'prolog)
			   (sgml-parse-processing-instruction)))))
    (while (and (or (< (point) sgml-goal) sgml-current-eref)
		(not (if extra-cond (funcall extra-cond))))
      (assert sgml-current-tree)
      (setq sgml-markup-start (point)
	    sgml-markup-type nil)
      (cond
       ((eobp) (sgml-pop-entity))
       ((or (sgml-parse-delim "ETAGO" gi)
	    (sgml-is-enabled-net))
	(sgml-do-end-tag))
       ((and (or (eq sgml-current-state sgml-cdata)
		 (eq sgml-current-state sgml-rcdata)))
	(sgml-skip-cdata))
       ((and sgml-current-shortmap
	     (or (setq tem (sgml-deref-shortmap sgml-current-shortmap))
		 ;; Restore position, to consider the delim for S+ or data
		 (progn (goto-char sgml-markup-start)
			nil)))
	(funcall sgml-shortref-handler 
		 (or (sgml-lookup-entity tem (sgml-dtd-entities sgml-dtd-info))
		     (sgml-error "Entity %s undefined (referenced by shortref)"
				 tem))))
       ((and (not (sgml-current-mixed-p))
	     (sgml-parse-s sgml-current-shortmap)))
       ((sgml-parse-delim "STAGO" gi)
	(sgml-do-start-tag))
       ((sgml-parse-general-entity-ref))
       ((sgml-parse-markup-declaration nil))
       ((sgml-parse-delim "MS-END")	; end of marked section
	(sgml-set-markup-type 'ms-end))
       ((sgml-parse-processing-instruction))
       (t
	(sgml-do-pcdata))))))

(defun sgml-handle-shortref (entity)
  (sgml-set-markup-type 'shortref)
  (unless (sgml-entity-data-p entity)
    (sgml-push-to-entity entity sgml-markup-start)))

(defun sgml-do-start-tag ()    
  ;; Assume point after STAGO
  (setq sgml-conref-flag nil)
  (let (temp net-enabled et asl)
    (setq et (if (sgml-is-delim "TAGC")	; empty start-tag
		 (sgml-do-empty-start-tag)
	       (sgml-lookup-eltype (sgml-check-name))))
    (unless (sgml-eltype-model et)
      (and sgml-warn-about-undefined-elements
	   (sgml-log-warning "Undefined element %s" (sgml-eltype-name et)))
      (setf (sgml-eltype-model et) sgml-any))
    (unless (sgml-parse-delim "TAGC")	; optimize common case
      (setq asl (sgml-parse-attribute-specification-list et))
      (or
       (if (sgml-parse-delim "NET")
	   (prog1 (setq net-enabled t)
	     (or sgml-current-shorttag
		 (sgml-log-warning
		  "NET enabling start-tag is not allowed with SHORTTAG NO"))))
       (sgml-check-tag-close)))
    (while				; Until token accepted
	(cond
	 ((eq sgml-current-state sgml-any) nil)
	 ((and (not (memq et (sgml-excludes)))
	       (setq temp (sgml-get-move sgml-current-state et)))
	  (setq sgml-current-state temp)
	  nil)
	 ((and (memq et (sgml-includes))
	       (not (memq et (sgml-excludes))))
	  nil)
	 ((sgml-do-implied
	   (format "%s start-tag" (sgml-eltype-name et))))))
    (sgml-set-markup-type 'start-tag)
    (sgml-open-element et sgml-conref-flag sgml-markup-start (point))
    (when net-enabled
      (setf (sgml-tree-net-enabled sgml-current-tree) t))))

(defun sgml-do-empty-start-tag ()
  "Return eltype to use if empty start tag"
  (cond
   ;; Document element if no element is open
   ((eq sgml-current-tree sgml-top-tree)
    (sgml-lookup-eltype	
     (sgml-dtd-doctype sgml-dtd-info)))
   ;; If omittag use current open element
   (sgml-current-omittag
    (sgml-tree-eltype sgml-current-tree))
   ;; Find the eltype of the last closed element.
   ;; If element has a left sibling then use that
   (sgml-previous-tree	
    (sgml-tree-eltype sgml-previous-tree))
   ;; No sibling, last closed must be found in enclosing element
   (t
    (loop named outer
	  for current = sgml-current-tree then (sgml-tree-parent current)
	  for parent  = (sgml-tree-parent current)
	  do;; Search for a parent with a child before current
	  (when (eq parent sgml-top-tree) (sgml-error "No previously closed element"))
	  (unless (eq current (sgml-tree-content parent))
	    ;; Search content of u for element before current
	    (loop for c = (sgml-tree-content parent) then (sgml-tree-next c)
		  do (when (eq current (sgml-tree-next c))
		       (return-from outer (sgml-tree-eltype c)))))))))


(defun sgml-do-end-tag ()
  "Assume point after </ or at / in a NET"
  (let ((gi "Null")			; Name of element to end or "NET"
	et				; Element type of end tag
	found)				; Set to true when found element to end
    (cond ((sgml-parse-delim "TAGC")		; empty end-tag
	   (setq et (sgml-tree-eltype sgml-current-tree)))
	  ((sgml-parse-char ?/))	; NET
	  (t
	   (setq et (sgml-lookup-eltype (sgml-check-name)))
	   (sgml-parse-s)
	   (sgml-check-tag-close)))
    (when et (setq gi (sgml-eltype-name et)))
    ;; Room for improvment:  Check if eltype is defined and that it
    ;; is in the tree.  Otherwise ignore with warning.
    (while (not found)			; Loop until correct element to
					; end is found
      (unless (sgml-final-p sgml-current-state)
	(sgml-log-warning
	 "%s element can't end here, need one of %s; %s end-tag out of context"
	 (sgml-element-gi sgml-current-tree)
	 (sgml-required-tokens sgml-current-state)
	 gi))
      (when (eq sgml-current-tree sgml-top-tree)
	(sgml-error "%s end-tag ended document and parse" gi))
      (setq found (or (eq et (sgml-tree-eltype sgml-current-tree))
		      (and (null et)	; Null end-tag
			   (eq t (sgml-tree-net-enabled sgml-current-tree)))))
      (unless found
	(sgml-implied-end-tag (format "%s end-tag" gi)
			      sgml-markup-start sgml-markup-start))))
  (sgml-set-markup-type 'end-tag)
  (sgml-close-element sgml-markup-start (point)))

(defun sgml-is-goal-after-start (goal tree)
  (and tree
       ;;(not (zerop (sgml-tree-stag-len tree)))
       (> goal (sgml-element-start tree))))

(defun sgml-find-start-point (goal)
  (let ((u sgml-top-tree))
    (while
	(cond
	 ((sgml-is-goal-after-start goal (sgml-tree-next u))
	  (setq u (sgml-tree-next u)))
	 ((and (sgml-tree-etag-epos u)
	       (if (> (sgml-tree-etag-len u) 0) ; if threre is an end-tag
		   (>= goal (sgml-tree-end u))  ; precisely after is after
		 (> goal (sgml-tree-end u))))   ; else it could possibly
					; become part of the element
	  (sgml-set-parse-state u 'after)
	  nil)
	 ((sgml-is-goal-after-start goal (sgml-tree-content u))
	  (setq u (sgml-tree-content u)))
	 (t
	  (sgml-set-parse-state u 'start)
	  nil)))))

(defun sgml-check-tag-close ()
  (or
   (sgml-parse-delim "TAGC")
   (if (or (sgml-is-delim "STAGO" gi)
	   (sgml-is-delim "ETAGO" gi))
       (or sgml-current-shorttag
	   (sgml-log-warning
	    "Unclosed tag is not allowed with SHORTTAG NO")))
   (sgml-error "Invalid character in markup %c"
	       (following-char))))
  
(defun sgml-do-implied (type &optional temp)
  (cond
   ((sgml-final-p sgml-current-state)
    (sgml-implied-end-tag type sgml-markup-start sgml-markup-start)
    t)
   ((and (setq temp (sgml-required-tokens sgml-current-state))
	 (null (cdr temp)))
    (setq sgml-current-state
	  (sgml-get-move sgml-current-state (car temp)))
    (sgml-open-element (sgml-token-eltype (car temp))
		       nil sgml-markup-start sgml-markup-start)
    (unless (and sgml-current-omittag
		 (sgml-element-stag-optional sgml-current-tree))
      (sgml-log-warning
       "%s start-tag implied by %s; not minimizable"
       (car temp) type))
    t)
   (t (sgml-log-warning "%s out of context" type)
      nil)))

(defun sgml-implied-end-tag (type start end)
  (cond ((eq sgml-current-tree sgml-top-tree)
	 (unless (= start (point-max))
	   (sgml-error
	    "document ended by %s" type)))
	((not
	  (and sgml-current-omittag
	       (sgml-element-etag-optional sgml-current-tree)))
	 (sgml-log-warning
	  "%s end-tag implied by %s; not minimizable"
	  (sgml-element-gi sgml-current-tree)
	  type)))
  (sgml-close-element start end))


;;;; Parsing tasks and extending the element view of the parse tree

(defun sgml-find-context-of (pos)
  "Find the parser context for POS, returns the parse tree.
Also sets sgml-current-tree and sgml-current-state.  If POS is in
markup, sgml-markup-type will be a symbol identifying the markup
type.  It will be nil otherwise."
  (save-excursion
    (sgml-parse-to pos)
    (cond ((and (> (point) pos)
		sgml-markup-type)
	   ;;(setq sgml-current-state sgml-markup-type)
	   (cond ((memq sgml-markup-type '(start-tag end-tag))
		  (setq sgml-current-tree sgml-markup-tree))))
	  (t
	   (setq sgml-markup-type nil)))
    sgml-current-tree))

(defun sgml-parse-to-here ()
  "Find context of point.
See documentation of sgml-find-context-of."
  (sgml-find-context-of (point)))

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
		      (>= pos (sgml-element-start (sgml-tree-next u))))
		 (setq u (sgml-tree-next u))) ; continue searching next node
		((and (sgml-tree-etag-epos u)
		      (>= pos (sgml-tree-end u)))
		 (setq u (sgml-tree-parent u)) ; must be parent node
		 nil)
		((and (sgml-tree-content u)
		      (>= pos (sgml-element-start (sgml-tree-content u))))
		 (setq u (sgml-tree-content u))))) ; search content
      u)))

(defun sgml-find-previous-element (pos &optional in-element)
  "Find the element before POS and return it, error if non found.
If in IN-ELEMENT is given look for previous element in IN-ELEMENT else
look in current element.  If this element has no content elements but
end at POS, it will be returned as previous element."
  (save-excursion
    ;; Parse to point; now the previous element is in the parse tree
    (sgml-parse-to pos)
    ;; containing element may be given or obtained from parser
    (or in-element (setq in-element sgml-current-tree))
    ;; in-element is the containing element
    (let* ((c				; this is the content of the
					; containing element
	    (sgml-tree-content in-element)))	
      (while
	  (cond
	   ((null c)			; If c = Nil: no previous element.
	    ;; But maybe the containing element ends at pos too.
	    (cond ((= pos (sgml-element-end in-element))
		   (setq c in-element))) ; Previous is parent!
	    nil)
	   ((<= pos (sgml-element-start c)) ; Pos before first content el
	    (setq c nil))		; No, previous element.
	   ((null (sgml-tree-next c)) nil) ; No next, c must be the prev el
	   ((>= (sgml-element-start (sgml-tree-next c)) pos)
	    nil)
	   (t
	    (setq c (sgml-tree-next c)))))
      (or c
	  (error "No previous element in %s element"
		 (sgml-element-gi in-element))))))

(defun sgml-find-element-after (pos &optional in-element)
  "Find the first element starting after POS.
Returns parse tree; error if no element after POS."
  (setq in-element (or in-element
		       (save-excursion (sgml-find-context-of pos))))
  (or
   ;; First try to find element after POS in IN-ELEMENT/current element
   (let ((c				; content of in-element
	  (sgml-element-content in-element)))
     (while (and c
		 (> pos (sgml-element-start c)))
       (setq c (sgml-element-next c)))
     c)
   ;; If there is no more elements IN-ELEMENT/current element try
   ;; to identify the element containing the character after POS.
   ;; If this element starts at POS, use it for element after POS.
   (let ((el (sgml-find-element-of pos)))
     (if (and el (= pos (sgml-element-start el)))
	 el))
   (progn
     (sgml-message "")			; force display of log buffer
     (error "No more elements in %s element"
	    (sgml-element-gi in-element)))))

(defun sgml-element-content (element)
  "First element in content of ELEMENT, or nil."
  (when (null (or (sgml-tree-content element)
		  (sgml-tree-etag-epos element)))
    (save-excursion (sgml-parse-until-end-of t)))
  (sgml-tree-content element))

(defun sgml-element-next (element)
  "Next sibling of ELEMENT."
  (unless (sgml-tree-etag-epos element)
    (save-excursion (sgml-parse-until-end-of element)))
  (unless (or (sgml-tree-next element)
	      (sgml-tree-etag-epos (sgml-tree-parent element)))
    (save-excursion (sgml-parse-until-end-of t)))
  (sgml-tree-next element))

(defun sgml-element-etag-start (element)
  "Last position in content of ELEMENT and start of end-tag, if any."
  (unless (sgml-tree-etag-epos element)
    (save-excursion
      (sgml-parse-until-end-of element)))
  (assert (sgml-tree-etag-epos element))
  (sgml-epos-promote (sgml-tree-etag-epos element)))

(defun sgml-element-end (element)
  "First position after ELEMENT."
  (sgml-element-etag-start element)	; make end be defined
  (sgml-tree-end element))

(defun sgml-read-element-name (prompt)
  (sgml-parse-to-here)
  (cond ((and ;;sgml-buffer-eltype-map
	      (not (eq sgml-current-state sgml-any)))
	 (let ((tab
		(mapcar (function (lambda (x) (cons (symbol-name x) nil)))
			(sgml-current-list-of-valid-eltypes))))
	   (cond ((null tab)
		  (error "No element valid at this point"))
		 (t
		  (completing-read prompt tab nil t
				   (and (null (cdr tab)) (caar tab)))))))
	(t
	 (read-from-minibuffer prompt))))

(defun sgml-element-attribute-specification-list (element)
  "Return the attribute specification list for ELEMENT.
This is a list of (attname value) lists."
  (if (> (sgml-element-stag-len element) 2)
      (save-excursion
	(sgml-with-parser-syntax
	 (sgml-goto-epos (sgml-element-stag-epos element))       
	 (sgml-check-delim "STAGO")
	 (sgml-check-name)
	 (prog1 (sgml-parse-attribute-specification-list
		 (sgml-element-eltype element))
	   (sgml-pop-all-entities))))))

(defun sgml-find-attribute-element ()
  "Return the element to which an attribute editing command should be applied."
  (let ((el (sgml-find-element-of (point))))
    (save-excursion
      (sgml-parse-to (point))
      ;; If after a start-tag of an empty element return that element
      ;; instead of current element
      (if (eq sgml-markup-type 'start-tag)
	  sgml-markup-tree		; the element of the start-tag
	el))))


(defun sgml-element-attval (element attribute)
  "Return the value of the ATTRIBUTE in ELEMENT, string or nil."
  (let ((asl (sgml-element-attribute-specification-list element))
	(def (sgml-attdecl-default-value
	      (sgml-lookup-attdecl attribute (sgml-element-attlist element)))))
    (or (sgml-attspec-attval (sgml-lookup-attspec attribute asl))
	(sgml-default-value-attval def))))


(defun sgml-cohere-name (x)
  "Convert X into a string where X can be a string, a symbol or an element."
  (cond ((stringp x) x)
	((symbolp x) (symbol-name x))
	(t (sgml-element-gi x))))

(defun sgml-start-tag-of (element)
  "Return the start-tag for ELEMENT."
  (format "<%s>" (sgml-cohere-name element)))

(defun sgml-end-tag-of (element)
  "Return the end-tag for ELEMENT (token or element)."
  (format "</%s>" (sgml-cohere-name element)))

(defun sgml-top-element ()
  "Return the document element."
  (sgml-element-content (sgml-find-context-of (point-min))))

;;;; Provide

(provide 'psgml-parse)

;;; psgml-parse.el ends here
