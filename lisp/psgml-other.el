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


;;;; Menu bar

(defvar sgml-markup-menu (make-sparse-keymap "Markup"))
(fset 'sgml-markup-menu sgml-markup-menu)

(define-key sgml-mode-map [menu-bar sgml-dtd]
  (cons "DTD" (make-sparse-keymap "DTD")))

(define-key sgml-mode-map [menu-bar sgml-fold]
  (cons "Fold" (make-sparse-keymap "Fold")))

(define-key sgml-mode-map [menu-bar sgml-markup]
  '("Markup" . sgml-markup-menu ))

;;(define-key sgml-mode-map [menu-bar sgml-entities]
;;  '("Entities" . sgml-entities-menu))

;;(define-key sgml-mode-map [menu-bar sgml-tags]
;;  '("Tags" . sgml-tags-menu))

(define-key sgml-mode-map [menu-bar sgml]
  (cons "SGML" (make-sparse-keymap "SGML")))


;;;; SGML menu

(define-key sgml-mode-map [menu-bar sgml report-buf]
  '("Submit bug report" . sgml-submit-bug-report))
(define-key sgml-mode-map [menu-bar sgml save-options]
  '("Save options" . sgml-save-options))
(define-key sgml-mode-map [menu-bar sgml options]
  '("Options..." . sgml-options-menu))
(define-key sgml-mode-map [menu-bar sgml fill]
  '("Fill element" . sgml-fill-element))
(define-key sgml-mode-map [menu-bar sgml normalize]
  '("Normalize" . sgml-normalize))
(define-key sgml-mode-map [menu-bar sgml validate]
  '("Validate" . sgml-validate))
(define-key sgml-mode-map [menu-bar sgml show-log]
  '("Show/hide warning log" . sgml-show-or-clear-log))
(define-key sgml-mode-map [menu-bar sgml show-tags]
  '("List valid tags" . sgml-list-valid-tags))
(define-key sgml-mode-map [menu-bar sgml change-name]
  '("Change element name" . sgml-change-element-name))
(define-key sgml-mode-map [menu-bar sgml edit-attributes]
  '("Edit attributes" . sgml-edit-attributes))
(define-key sgml-mode-map [menu-bar sgml next-trouble]
  '("Next trouble spot" . sgml-next-trouble-spot)) 
(define-key sgml-mode-map [menu-bar sgml what-element]
  '("What element" . sgml-what-element))
(define-key sgml-mode-map [menu-bar sgml show-context]
  '("Show context" . sgml-show-context))
(define-key sgml-mode-map [menu-bar sgml insert-end-tag]
  '("End element" . sgml-insert-end-tag))
(define-key sgml-mode-map [menu-bar sgml next-data]
  '("Next data field" . sgml-next-data-field))


;;;; DTD menu

(define-key sgml-mode-map [menu-bar sgml-dtd blank-c]
  '("" . nil))
(define-key sgml-mode-map [menu-bar sgml-dtd save]
  '("Save parsed DTD" . sgml-save-dtd))
(define-key sgml-mode-map [menu-bar sgml-dtd load]
  '("Load parsed DTD" . sgml-load-dtd))
(define-key sgml-mode-map [menu-bar sgml-dtd parse]
  '("Parse DTD" . sgml-parse-prolog))


;;;; Fold menu

(define-key sgml-mode-map [menu-bar sgml-fold unfold-all]
  '("Unfold all" . sgml-unfold-all))
(define-key sgml-mode-map [menu-bar sgml-fold fold-region]
  '("Fold region" . sgml-fold-region))
(define-key sgml-mode-map [menu-bar sgml-fold expand]
  '("Expand" . sgml-expand-element))
(define-key sgml-mode-map [menu-bar sgml-fold unfold-element]
  '("Unfold element" . sgml-unfold-element))
(define-key sgml-mode-map [menu-bar sgml-fold unfold]
  '("Unfold line" . sgml-unfold-line))
(define-key sgml-mode-map [menu-bar sgml-fold subfold]
  '("Fold subelement"   . sgml-fold-subelement))
(define-key sgml-mode-map [menu-bar sgml-fold fold]
  '("Fold element"   . sgml-fold-element))


;;;; Markup menu

(define-key sgml-markup-menu [blank-c]
  '("" . nil))

(define-key sgml-markup-menu [ entity]
  (sgml-markup "<!entity ... >" "<!entity \r>\n"))
(define-key sgml-markup-menu [ attlist]
  (sgml-markup "<!attlist ... >" "<!attlist \r>\n"))
(define-key sgml-markup-menu [ element]
  (sgml-markup "<!element ... >" "<!element \r>\n"))
(define-key sgml-markup-menu [ doctype]
  (sgml-markup "<!doctype ...>"
	       "<!doctype \r -- public or system --\n[\n]>\n"))

(define-key sgml-markup-menu [blank1]
  '("" . nil))

(define-key sgml-markup-menu [lv-comment]
  (sgml-markup "Local variables comment"
	       "<!--\nLocal variables:\n\rEnd:\n-->\n"))
(define-key sgml-markup-menu [ comment]
  (sgml-markup "Comment" "<!-- \r -->\n"))


(define-key sgml-markup-menu [blank2]
  '("" . nil))

(define-key sgml-markup-menu [ temp]
  (sgml-markup "TEMP marked section" "<![TEMP[\r]]>"))
(define-key sgml-markup-menu [ rcdata]
  (sgml-markup "RCDATA marked section" "<![RCDATA[\r]]>\n"))
(define-key sgml-markup-menu [ cdata]
  (sgml-markup "CDATA marked section" "<![CDATA[\r]]>\n"))
(define-key sgml-markup-menu [ ms]
  (sgml-markup "Marked section" "<![ [\r]]>\n"))

(define-key sgml-markup-menu [blank3]
  '("" . nil))

(define-key sgml-markup-menu [entities]
  '("Insert entity" . sgml-entities-menu))

(define-key sgml-markup-menu [attributes]
  '("Insert attribute" . sgml-attrib-menu))

(define-key sgml-markup-menu [tag-region]
  '("Tag region" . sgml-tag-region-menu))

(define-key sgml-markup-menu [insert-end-tag]
  '("Insert end-tag" . sgml-end-tag-menu))

(define-key sgml-markup-menu [insert-start-tag]
  '("Insert start-tag" . sgml-start-tag-menu))

(define-key sgml-markup-menu [insert-element]
  '("Insert element" . sgml-element-menu))


;;;; Key commands

;; Doesn't this work in Lucid? ***
(define-key sgml-mode-map [?\M-\C-\ ] 'sgml-mark-element)

(define-key sgml-mode-map [S-mouse-1] 'sgml-tags-menu)


;;;; Build custom menus

(defun sgml-build-custom-menus ()
  ;; Build custom menus
  (sgml-add-custom-entries
   sgml-markup-menu
   (mapcar (function (lambda (e)
		       (sgml-markup (car e) (cadr e))))
	   sgml-custom-markup))
  (sgml-add-custom-entries
   (lookup-key sgml-mode-map [menu-bar sgml-dtd])
   (mapcar (function
	    (lambda (e)
	      (cons (first e)
		    (` (lambda ()
			 (interactive)
			 (apply 'sgml-doctype-insert '(, (cdr e))))))))
	   sgml-custom-dtd)))


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
	o)
    (while current
      (cond ((and (null o)
		  (eq type (overlay-get (car current) 'type)))
	     (setq o (car current)))
	    ((overlay-get (car current) 'type)
	     (delete-overlay (car current))))
      (setq current (cdr current)))
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
	       (move-overlay o end (overlay-end o)))
	      (t (delete-overlay o))))))

(defalias 'next-overlay-at 'next-overlay-change) ; fix bug in cl.el

(defun sgml-clear-faces ()
  (interactive)
  (loop for o being the overlays
	if (overlay-get o 'type)
	do (delete-overlay o)))


;;;; Provide

(provide 'psgml-other)

;;; psgml-other.el ends here
