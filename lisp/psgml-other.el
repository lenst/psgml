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

(easy-menu-define
 sgml-dtd-menu sgml-mode-map "DTD menu"
 '("DTD"))

(defconst sgml-dtd-root-menu
  '("DTD"
    ["Parse DTD"  sgml-parse-prolog t]
    ("Info"
     ["General DTD info"	sgml-general-dtd-info           t]
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

(easy-menu-define
 sgml-view-menu sgml-mode-map "View menu"
 '("View"
   ["Fold Element"	sgml-fold-element	t]
   ["Fold Subelement"	sgml-fold-subelement	t]
   ["Unfold Line"	sgml-unfold-line	t]
   ["Unfold Element"	sgml-unfold-element	t]
   ["Expand"		sgml-expand-element	t]
   ["Fold Region"	sgml-fold-region	t]
   ["Unfold All"	sgml-unfold-all		t]
   ["Hide Tags"		sgml-hide-tags		t]
   ["Hide Attributes"	sgml-hide-attributes	t]
   ["Show All Tags"	sgml-show-tags		t]
   )
 )


(easy-menu-define
 sgml-markup-menu sgml-mode-map "Markup menu"
 '("Markup")
)

(defconst sgml-markup-root-menu
  '("Markup"
    ["Insert Element"	sgml-element-menu	t]
    ["Insert Start-Tag" sgml-start-tag-menu	t]
    ["Insert End-Tag"	sgml-end-tag-menu	t]
    ["Tag Region"	sgml-tag-region-menu	t]
    ["Insert Attribute" sgml-attrib-menu	t]
    ["Insert Entity"	sgml-entities-menu	t]
    ))

(easy-menu-define
 sgml-move-menu sgml-mode-map "Menu of move commands"
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
   ))

(easy-menu-define
 sgml-modify-menu sgml-mode-map "Menu of modification commands"
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

(easy-menu-define
 sgml-main-menu sgml-mode-map "Main menu"
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


;;;; Key Commands

;; Doesn't this work in Lucid? ***
(define-key sgml-mode-map [?\M-\C-\ ] 'sgml-mark-element)

(define-key sgml-mode-map [S-mouse-1] 'sgml-tags-menu)


;;;; Pop Up Menus

(defun sgml-popup-menu (event title entries)
  "Display a popup menu.
ENTRIES is a list where every element has the form (STRING . VALUE) or
STRING."
  (x-popup-menu
   event
   (let ((menus (list (cons title entries))))
     (cond ((> (length entries)
	       sgml-max-menu-size)
	    (setq menus
		  (loop for i from 1 while entries
			collect
			(let ((submenu
			       (subseq entries 0 (min (length entries)
						      sgml-max-menu-size))))
			  (setq entries (nthcdr sgml-max-menu-size
						entries))
			  (cons
			   (format "%s '%s'-'%s'"
				   title
				   (sgml-range-indicator (caar submenu))
				   (sgml-range-indicator (caar (last submenu))))
			   submenu))))))
     (cons title menus))))

(defun sgml-range-indicator (string)
  (substring string
	     0
	     (min (length string) sgml-range-indicator-max-length)))

(defun sgml-popup-multi-menu (event title menus)
  "Display a popup menu.
MENUS is a list of menus on the form (TITLE ITEM1 ITEM2 ...).
ITEM should have to form (STRING EXPR) or STRING.  The EXPR gets evaluated
if the item is selected."
  (nconc menus '(("---" "---")))	; Force x-popup-menu to use two level
					; menu even if there is only one entry
					; on the first level
  (eval (car (x-popup-menu event (cons title menus)))))



;;;; Build Custom Menus

(defun sgml-build-custom-menus ()
  ;; Build custom menus
;;  (sgml-add-custom-entries
;;   sgml-markup-menu
;;   (mapcar (function (lambda (e)
;;		       (sgml-markup (car e) (cadr e))))
;;	   sgml-custom-markup))
  (easy-menu-define
   sgml-markup-menu sgml-mode-map "Markup menu"
   (append sgml-markup-root-menu
	   (list "----")
	   (loop for e in sgml-custom-markup collect
		 (vector (first e)
			 (` (sgml-markup '(,@(cdr e))))
			 t))))
  (easy-menu-define
   sgml-dtd-menu sgml-mode-map "DTD menu"
   (append sgml-dtd-root-menu
	   (list "----")
	   (loop for e in sgml-custom-dtd collect
		 (vector (first e)
			 (` (sgml-doctype-insert '(,@(cdr e))))
			 t)))))


;(defun sgml-add-custom-entries (keymap entries)
;  "Add to KEYMAP the ENTRIES, a list of (name . command) pairs.
;The entries are added last in keymap and a blank line precede it."
;  (let ((l keymap)
;	(last (last keymap)))		; cons with keymap name
;    ;; Find the cons before 'blank-c' event, or last cons.
;    (while (and (cdr l)
;		(consp (cadr l))
;		(not (eq 'blank-c (caadr l))))
;      (setq l (cdr l)))
;    ;; Delete entries after
;    (setcdr l nil)
;    (when entries			; now add the entries
;      (setcdr l
;	      (cons
;	       '(blank-c "")		; a blank line before custom entries
;	       (loop for i from 0 as e in entries
;		     collect (cons (intern (concat "custom" i)) e)))))
;    ;; add keymap name to keymap
;    (setcdr (last keymap) last)))






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
		  (eq type (overlay-get (car current) 'sgml-type)))
	     (setq o (car current)))
	    ((overlay-get (car current) 'sgml-type)
	     (delete-overlay (car current))))
      (setq current (cdr current)))
    (while (< (setq pos (next-overlay-change pos))
	      end)
      (setq current (overlays-at pos))
      (while current
	(when (overlay-get (car current) 'sgml-type)
	  (delete-overlay (car current)))
	(setq current (cdr current))))
    (cond (o
	   (move-overlay o start end)
	   (if (null (overlay-get o 'face))
	       (overlay-put o 'face face)))
	  (face
	   (setq o (make-overlay start end))
	   (overlay-put o 'sgml-type type)
	   (overlay-put o 'face face)))))

(defun sgml-set-face-after-change (start end &optional pre-len)
  (when sgml-set-face
    (loop for o in (overlays-at start)
	  do (cond
	      ((not (overlay-get o 'sgml-type)))
	      ((= start (overlay-start o))
	       (move-overlay o end (overlay-end o)))))))

(defalias 'next-overlay-at 'next-overlay-change) ; fix bug in cl.el

(defun sgml-clear-faces ()
  (interactive)
  (loop for o being the overlays
	if (overlay-get o 'sgml-type)
	do (delete-overlay o)))


;;;; Provide

(provide 'psgml-other)

;;; psgml-other.el ends here
