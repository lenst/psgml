;;;; psgml-lucid.el --- Part of SGML-editing mode with parsing support
;; $Id$

;; Copyright (C) 1994 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>
;;	   William M. Perry <wmperry@indiana.edu>

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

;;; Menus for use with Lucid Emacs


;;;; Code:

;;; These functions are needed for menus, etc. to work in lucid emacs

(defun sgml-fix-x-menu (menudesc)
  "Take a menu for x-popup-menu and return a lucid menu."
  (cons (car menudesc)			; title
	(mapcar (function
		 (lambda (item)
		   ;; item is (string . value)
		   (vector (car item)
			   (list 'quote (cdr item))
			   t)))
		(cdr menudesc))))


(defun x-popup-menu (pos menudesc)
  "My hacked up function to do a blocking popup menu..."
  (let ((echo-keystrokes 0)
	event menu)
    (cond
     ((stringp (car menudesc))		; deck of meues
      (setq menu (if (null (cddr menudesc)) ; only one menu
		     (sgml-fix-x-menu (cadr menudesc))
		   (cons (car menudesc)
			 (mapcar (function sgml-fix-x-menu)
				 (cdr menudesc))))))
     ((listp (car menudesc))		; deck of keymaps
      (error "NIY"))
     (t					; keymap
      (error "NIY")))
    (popup-menu menu)
    (cadr
     (catch 'popup-done
       (while t
	 (setq event (next-command-event event))
	 (cond ((and (menu-event-p event)
		     (eq 'quote (car-safe (event-object event))))
		(throw 'popup-done (event-object event)))
	       ((and (menu-event-p event)
		     (or (eq (event-object event) 'abort)
			 (eq (event-object event) 'menu-no-selection-hook)))
		(signal 'quit nil))
	       ((button-release-event-p event) ; don't beep twice
		nil)
	       (t
		(beep)
		(message "please make a choice from the menu."))))))))

(defun sgml-install-lucid-menus ()
  "Install lucid menus for psgml mode"
  (set-buffer-menubar (copy-sequence default-menubar))
  (add-menu nil (car sgml-sgml-menu) (cdr sgml-sgml-menu) "Help")
  (add-menu nil (car sgml-markup-menu) (cdr sgml-markup-menu) "Help")
  (add-menu nil (car sgml-fold-menu) (cdr sgml-fold-menu) "Help")
;;  (add-menu nil (car sgml-tags-menu) (cdr sgml-tags-menu) "Help")
;;  (add-menu nil (car sgml-entities-menu) (cdr sgml-entities-menu) "Help")
  (add-menu nil (car sgml-dtd-menu) (cdr sgml-dtd-menu) "Help"))

(defvar sgml-markup-menu
  '("Markup"
    ["Insert tag" (sgml-tags-menu last-command-event) t]
    ["Insert entity" (sgml-entities-menu last-command-event) t]
    "----------"
    ["Marked section" (sgml-insert-markup "<![ [\r]]>\n") t]
    ["CDATA marked section" (sgml-insert-markup "<![CDATA[\r]]>\n") t]
    ["RCDATA marked section" (sgml-insert-markup "<![RCDATA[\r]]>\n") t]
    ["TEMP marked section" (sgml-insert-markup "<![TEMP[\r]]>") t]
    "----------"
    ["Doctype" (sgml-insert-markup "<!doctype \r -- public or system -- [\n]>\n") t]
    ["Comment" (sgml-insert-markup "<!-- \r -->\n") t]
    ["Local variables comment" (sgml-insert-markup "<!--\nLocal Variables:\nmode: sgml\n\rEnd:\n-->\n")
     t]
    "----------"
    ["<!entity ... >" (sgml-insert-markup "<!entity \r>\n") t]
    ["<!attlist ... >" (sgml-insert-markup "<!attlist \r>\n") t]
    ["<!element ... >" (sgml-insert-markup "<!element \r>\n") t]
    ))

(defvar sgml-dtd-menu
  '("DTD"
    ["Parse DTD" sgml-parse-prolog t]
    ["Load parsed DTD" sgml-load-dtd t]
    ["Save parsed DTD" sgml-save-dtd t]
    ))

(defvar sgml-fold-menu
  '("Fold"
    ["Fold element" sgml-fold-element t]
    ["Fold subelement" sgml-fold-subelement t]
    ["Fold region" sgml-fold-region t]
    ["Unfold line" sgml-unfold-line t]
    ["Unfold element" sgml-unfold-element t]
    ["Unfold all" sgml-unfold-all t]
    ["Expand" sgml-expand-element t]
    ))

(defvar sgml-entities-menu
  '("Entities"
    ["Show entities" (sgml-entities-menu last-command-event) t]
    ))

(defvar sgml-tags-menu
  '("Tags"
    ["Show valid tags" (sgml-tags-menu last-command-event) t]
    ))

(defvar sgml-sgml-menu
  '("SGML"
    ["Next data field"  sgml-next-data-field t]
    ["End element" sgml-insert-end-tag t]
    ["Show context" sgml-show-context t]
    ["What element" sgml-what-element t]
    ["Next trouble spot" sgml-next-trouble-spot t]
    ["Edit attributes" sgml-edit-attributes nil]
    ["Change element name" sgml-change-element-name t]
    ["Show valid tags" sgml-list-valid-tags t]
    ["Show/hide warning log" sgml-show-or-clear-log t]
    ["Normalize" sgml-normalize t]
    ["Fill element" sgml-fill-element t]
    ["Options" sgml-options-menu t]
    ["Save options" sgml-save-options t]
    ))

(defun sgml-build-custom-menus ()
  (and sgml-custom-markup (add-menu-item '("Markup") "------------" nil t))
  (mapcar (function
	   (lambda (x)
	     (add-menu-item '("Markup") (nth 0 x)
			    (list 'sgml-insert-markup (nth 1 x)) t)))
	  sgml-custom-markup)
  (and sgml-custom-dtd (add-menu-item '("DTD") "-------------" nil t))
  (mapcar (function
	   (lambda (x)
	     (add-menu-item '("DTD") (nth 0 x)
			    (list 'sgml-doctype-insert
				  (cadr x) (caddr x))
			    
			    t)))
	  sgml-custom-dtd))

(define-key sgml-mode-map [button3] 'sgml-tags-menu)


;;;; Emulate text properties


(set-face-underline-p (make-face 'underline) t) 


(defun add-text-properties (start end props)
  ;; First truncate existing extents
  (let ((e (extent-at start)))
    (when e
      (set-extent-endpoints e (extent-start-position e) start)))
  (when props
  (let ((e (make-extent start end)))
    (when (getf props 'read-only)
      (set-extent-attribute e 'write-protected))
    (when (getf props 'category)
      (set-extent-face e (find-face 'underline))
      (set-extent-data e (getf props 'category))))
    )
)



;;;; Provide

(provide 'psgml-lucid)


;;; psgml-lucid.el ends here
