;;;; psgml-other.el --- Part of SGML-editing mode with parsing support
;; $Id$

;; Copyright (C) 1994 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>

;; 
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

;;; Part of psgml.el

;;; Menus for use with FSF Emacs 19


;;;; Code:

(require 'psgml)
(require 'easymenu)

(defvar sgml-max-menu-size (/ (* (frame-height) 2) 3)
  "*Max number of entries in Tags and Entities menus before they are split
into several panes.")


;;;; Menu bar

;;;(define-key sgml-mode-map [menu-bar sgml-dtd]
;;;  (cons "DTD" (make-sparse-keymap "DTD")))

(easy-menu-define
 sgml-dtd-menu sgml-mode-map "DTD menu"
 '("DTD"))

(defconst sgml-dtd-root-menu
  '("DTD"
    ["Parse DTD"  sgml-parse-prolog t]
    ("Info"
     ["Describe element type"	sgml-describe-element-type	t]
     ["Describe entity"		sgml-describe-entity		t]
     ["List elements" 		sgml-list-elements 		t]
     ["List attributes" 	sgml-list-attributes 		t]
     ["List terminals" 		sgml-list-terminals 		t]
     ["List content elements" 	sgml-list-content-elements 	t]
     ["List occur in elements" 	sgml-list-occur-in-elements 	t]
     )
    "--"
    ["Load Parsed DTD"  sgml-load-dtd t]
    ["Save Parsed DTD"  sgml-save-dtd t]
    ))

(define-key sgml-mode-map [menu-bar sgml-view]
  (cons "View" (make-sparse-keymap "View")))

(defvar sgml-markup-menu (make-sparse-keymap "Markup"))
(fset 'sgml-markup-menu sgml-markup-menu)
(define-key sgml-mode-map [menu-bar sgml-markup]
  '("Markup" . sgml-markup-menu ))

(easy-menu-define
 sgml-move-menu
 sgml-mode-map
 "Menu of move commands"
 '("Move"
   ["Next trouble spot" sgml-next-trouble-spot t]
   ["Next data field"   sgml-next-data-field   t]
   ["Forward element"	sgml-forward-element t]
   ["Backward element"  sgml-backward-element t]
   ["Up element"	sgml-up-element t]
   ["Down element"	sgml-down-element t]
   ["Backward up element" sgml-backward-up-element t]
   ["Beginning of element" sgml-beginning-of-element t]
   ["End of element"	sgml-end-of-element t]
   )
 )

(easy-menu-define
 sgml-modify-menu
 sgml-mode-map
 "Menu of modification commands"
 '("Modify"
   ["Normalize"			sgml-normalize	t]
   ["Expand All Short References"	sgml-expand-all-shortrefs t]
   ["Expand Entity Reference"	sgml-expand-entity-reference t]
   ["Normalize Element"		sgml-normalize-element t]
   ["Make Character Reference"	sgml-make-character-reference t]
   ["Unmake Character Reference"	(sgml-make-character-reference t) t]
   ["Fill Element"		sgml-fill-element t]
   ["Change Element Name..."	sgml-change-element-name t]
   ["Edit Attributes..."	sgml-edit-attributes t]
   ["Kill Markup"		sgml-kill-markup t]
   ["Kill Element"		sgml-kill-element t]
   ["Untag Element"		sgml-untag-element t]
   ["Decode Character Entities"  sgml-charent-to-display-char t]
   ["Encode Characters"		sgml-display-char-to-charent t]
   )
 )


;;(define-key sgml-mode-map [menu-bar sgml-entities]
;;  '("Entities" . sgml-entities-menu))

;;(define-key sgml-mode-map [menu-bar sgml-tags]
;;  '("Tags" . sgml-tags-menu))

;;(define-key sgml-mode-map [menu-bar sgml]
;;  (cons "SGML" (make-sparse-keymap "SGML")))

(easy-menu-define
 sgml-main-menu sgml-mode-map
 "Main menu"
 '("SGML"
   ["Reset Buffer"	normal-mode t]
   ["End Element"	sgml-insert-end-tag t]
   ["Show Context"	sgml-show-context t]
   ["What Element"	sgml-what-element t]
   ["List Valid Tags"	sgml-list-valid-tags t]
   ["Show/Hide Warning Log"  sgml-show-or-clear-log t]
   ["Validate"		sgml-validate t]
   ["File Options >"	sgml-file-options-menu t]
   ["User Options >"	sgml-user-options-menu t]
   ["Save File Options"  sgml-save-options t]
   ["Submit Bug Report"  sgml-submit-bug-report t]
   )
 )



;;;; SGML menu

;;;(define-key sgml-mode-map [menu-bar sgml report-buf]
;;;  '("Submit Bug Report" . sgml-submit-bug-report))
;;;(define-key sgml-mode-map [menu-bar sgml save-options]
;;;  '("Save File Options" . sgml-save-options))
;;;(define-key sgml-mode-map [menu-bar sgml user-options]
;;;  '("User Options >" . sgml-user-options-menu))
;;;(define-key sgml-mode-map [menu-bar sgml file-options]
;;;  '("File Options >" . sgml-file-options-menu))
;;;(define-key sgml-mode-map [menu-bar sgml char-chent]
;;;  '("Encode characters" . sgml-display-char-to-charent))
;;;(define-key sgml-mode-map [menu-bar sgml chent-char]
;;;  '("Decode character entities" . sgml-charent-to-display-char))
;;;(define-key sgml-mode-map [menu-bar sgml fill]
;;;  '("Fill Element" . sgml-fill-element))
;;;(define-key sgml-mode-map [menu-bar sgml normalize]
;;;  '("Normalize" . sgml-normalize))
;;;(define-key sgml-mode-map [menu-bar sgml validate]
;;;  '("Validate" . sgml-validate))
;;;(define-key sgml-mode-map [menu-bar sgml show-log]
;;;  '("Show/Hide Warning Log" . sgml-show-or-clear-log))
;;;(define-key sgml-mode-map [menu-bar sgml show-tags]
;;;  '("List Valid Tags" . sgml-list-valid-tags))
;;;(define-key sgml-mode-map [menu-bar sgml change-name]
;;;  '("Change Element Name" . sgml-change-element-name))
;;;(define-key sgml-mode-map [menu-bar sgml edit-attributes]
;;;  '("Edit Attributes..." . sgml-edit-attributes))
;;;(define-key sgml-mode-map [menu-bar sgml next-trouble]
;;;  '("Next Trouble Spot" . sgml-next-trouble-spot)) 
;;;(define-key sgml-mode-map [menu-bar sgml what-element]
;;;  '("What Element" . sgml-what-element))
;;;(define-key sgml-mode-map [menu-bar sgml show-context]
;;;  '("Show Context" . sgml-show-context))
;;;(define-key sgml-mode-map [menu-bar sgml insert-end-tag]
;;;  '("End Element" . sgml-insert-end-tag))
;;;(define-key sgml-mode-map [menu-bar sgml next-data]
;;;  '("Next Data Field" . sgml-next-data-field))
;;;(define-key sgml-mode-map [menu-bar sgml reset]
;;;  '("Reset" . normal-mode))


;;;; DTD menu

;;;(define-key sgml-mode-map [menu-bar sgml-dtd blank-c]
;;;  '("" . nil))
;;;(define-key sgml-mode-map [menu-bar sgml-dtd save]
;;;  '("Save Parsed DTD" . sgml-save-dtd))
;;;(define-key sgml-mode-map [menu-bar sgml-dtd load]
;;;  '("Load Parsed DTD" . sgml-load-dtd))
;;;(define-key sgml-mode-map [menu-bar sgml-dtd parse]
;;;  '("Parse DTD" . sgml-parse-prolog))


;;;(let ((menu
;;;       '))
;;;  (define-key sgml-mode-map [menu-bar sgml-dtd info]
;;;    (let ((map (make-sparse-keymap (car menu))))
;;;      (loop for e in (reverse (cdr menu)) do
;;;	    (define-key map (vector (aref e 1))
;;;	      (cons (aref e 0) (aref e 1))))
;;;      (cons (car menu) map))))



;;;; View menu

(define-key sgml-mode-map [menu-bar sgml-view unhide]
  '("Show All Tags" . sgml-show-tags))
(define-key sgml-mode-map [menu-bar sgml-view hide-attributes]
  '("Hide Attributes" . sgml-hide-attributes))
(define-key sgml-mode-map [menu-bar sgml-view hide-tags]
  '("Hide Tags" . sgml-hide-tags))

(define-key sgml-mode-map [menu-bar sgml-view unfold-all]
  '("Unfold All" . sgml-unfold-all))
(define-key sgml-mode-map [menu-bar sgml-view fold-region]
  '("Fold Region" . sgml-fold-region))
(define-key sgml-mode-map [menu-bar sgml-view expand]
  '("Expand" . sgml-expand-element))
(define-key sgml-mode-map [menu-bar sgml-view unfold-element]
  '("Unfold Element" . sgml-unfold-element))
(define-key sgml-mode-map [menu-bar sgml-view unfold]
  '("Unfold Line" . sgml-unfold-line))
(define-key sgml-mode-map [menu-bar sgml-view subfold]
  '("Fold Subelement"   . sgml-fold-subelement))
(define-key sgml-mode-map [menu-bar sgml-view fold]
  '("Fold Element"   . sgml-fold-element))


;;;; Markup menu

(define-key sgml-markup-menu [blank-c]
  '("" . nil))

;;(define-key sgml-markup-menu [ entity]
;;  (sgml-markup "<!entity ... >" "<!entity \r>\n"))
;;(define-key sgml-markup-menu [ attlist]
;;  (sgml-markup "<!attlist ... >" "<!attlist \r>\n"))
;;(define-key sgml-markup-menu [ element]
;;  (sgml-markup "<!element ... >" "<!element \r>\n"))
;;(define-key sgml-markup-menu [ doctype]
;;  (sgml-markup "<!doctype ...>"
;;	       "<!doctype \r -- public or system --\n[\n]>\n"))

;;(define-key sgml-markup-menu [blank1]
;;  '("" . nil))

;;(define-key sgml-markup-menu [lv-comment]
;;  (sgml-markup "Local variables comment"
;;	       "<!--\nLocal variables:\n\rEnd:\n-->\n"))
;;(define-key sgml-markup-menu [ comment]
;;  (sgml-markup "Comment" "<!-- \r -->\n"))


;;(define-key sgml-markup-menu [blank2]
;;  '("" . nil))

;;(define-key sgml-markup-menu [ temp]
;;  (sgml-markup "TEMP marked section" "<![TEMP[\r]]>"))
;;(define-key sgml-markup-menu [ rcdata]
;;  (sgml-markup "RCDATA marked section" "<![RCDATA[\r]]>\n"))
;;(define-key sgml-markup-menu [ cdata]
;;  (sgml-markup "CDATA marked section" "<![CDATA[\r]]>\n"))
;;(define-key sgml-markup-menu [ ms]
;;  (sgml-markup "Marked section" "<![ [\r]]>\n"))


(define-key sgml-markup-menu [entities]
  '("Insert Entity" . sgml-entities-menu))

(define-key sgml-markup-menu [entities]
  '("Insert Entity" . sgml-entities-menu))

(define-key sgml-markup-menu [attributes]
  '("Insert Attribute" . sgml-attrib-menu))

(define-key sgml-markup-menu [tag-region]
  '("Tag Region" . sgml-tag-region-menu))

(define-key sgml-markup-menu [insert-end-tag]
  '("Insert End-Tag" . sgml-end-tag-menu))

(define-key sgml-markup-menu [insert-start-tag]
  '("Insert Start-Tag" . sgml-start-tag-menu))

(define-key sgml-markup-menu [insert-element]
  '("Insert Element" . sgml-element-menu))


;;;; Key Commands

;; Doesn't this work in Lucid? ***
(define-key sgml-mode-map [?\M-\C-\ ] 'sgml-mark-element)

(define-key sgml-mode-map [S-mouse-1] 'sgml-tags-menu)


;;;; Pop Up Menus

(defun sgml-popup-menu (event title entries)
  "Display a popup menu."
  (x-popup-menu
   event
   (let ((menus (list (cons title entries))))
     (cond ((> (length entries)
	       sgml-max-menu-size)
	    (setq menus
		  (loop for i from 1 while entries
			collect
			(prog1 (cons
				(format "%s %d" title i)
				(subseq entries 0 (min (length entries)
						       sgml-max-menu-size)))
			  (setq entries (nthcdr sgml-max-menu-size
						entries)))))))
     (cons title menus))))


;;;; Build Custom Menus

(defun sgml-build-custom-menus ()
  ;; Build custom menus
  (sgml-add-custom-entries
   sgml-markup-menu
   (mapcar (function (lambda (e)
		       (sgml-markup (car e) (cadr e))))
	   sgml-custom-markup))
  (easy-menu-change ()
		  "DTD"
		  (append (cdr sgml-dtd-root-menu)
			  (list "----")
			  (loop for e in sgml-custom-dtd collect
				(vector (first e)
					(` (sgml-doctype-insert '(,@(cdr e))))
					t))))
;;;  (sgml-add-custom-entries
;;;   (lookup-key sgml-mode-map [menu-bar sgml-dtd])
;;;   (mapcar (function
;;;	    (lambda (e)
;;;	      (cons (first e)
;;;		    (` (lambda ()
;;;			 (interactive)
;;;			 (apply 'sgml-doctype-insert '(, (cdr e))))))))
;;;	   sgml-custom-dtd))
  )


(defun sgml-add-custom-entries (keymap entries)
  "Add to KEYMAP the ENTRIES, a list of (name . command) pairs.
The entries are added last in keymap and a blank line precede it."
  (let ((l keymap)
	(last (last keymap)))		; cons with keymap name
    ;; Find the cons before 'blank-c' event, or last cons.
    (while (and (cdr l)
		(consp (cadr l))
		(not (eq 'blank-c (caadr l))))
      (setq l (cdr l)))
    ;; Delete entries after
    (setcdr l nil)
    (when entries			; now add the entries
      (setcdr l
	      (cons
	       '(blank-c "")		; a blank line before custom entries
	       (loop for i from 0 as e in entries
		     collect (cons (intern (concat "custom" i)) e)))))
    ;; add keymap name to keymap
    (setcdr (last keymap) last)))






;;;; Insert with properties

(defvar sgml-write-protect-intagible
  (not (boundp 'emacs-minor-version)))

(defun sgml-insert (props format &rest args)
  (let ((start (point)))
    (insert (apply (function format)
		   format
		   args))
    (when (and sgml-write-protect-intagible
	       (getf props 'intangible))
	  (setf (getf props 'read-only) t))
    (add-text-properties start (point) props)))


;;;; Set face of markup

(defun sgml-set-face-for (start end type)
  (let ((current (overlays-at start))
	(face (cdr (assq type sgml-markup-faces)))
	(pos start)
	o)
    (while current
      (cond ((and (null o)
		  (eq type (overlay-get (car current) 'type)))
	     (setq o (car current)))
	    ((overlay-get (car current) 'type)
	     (delete-overlay (car current))))
      (setq current (cdr current)))
    (while (< (setq pos (next-overlay-change pos))
	      end)
      (setq current (overlays-at pos))
      (while current
	(when (overlay-get (car current) 'type)
	  (delete-overlay (car current)))
	(setq current (cdr current))))
    (cond (o
	   (move-overlay o start end))
	  (face
	   (setq o (make-overlay start end))
	   (overlay-put o 'type type)
	   (overlay-put o 'face face)))))

(defun sgml-set-face-after-change (start end &optional pre-len)
  (when sgml-set-face
    (loop for o in (overlays-at start)
	  do (cond
	      ((not (overlay-get o 'type)))
	      ((= start (overlay-start o))
	       (move-overlay o end (overlay-end o)))))))

(defalias 'next-overlay-at 'next-overlay-change) ; fix bug in cl.el

(defun sgml-clear-faces ()
  (interactive)
  (loop for o being the overlays
	if (overlay-get o 'type)
	do (delete-overlay o)))


;;;; Provide

(provide 'psgml-other)

;;; psgml-other.el ends here
