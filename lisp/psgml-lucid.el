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

(defun x-popup-menu (pos menudesc)
  "My hacked up function to do a blocking popup menu..."
  (let ((echo-keystrokes 0)
	event menu)
    (setq menudesc (cdr (car (cdr menudesc)))) ; remove the title
    (while menudesc
      (setq menu (cons (vector (car (car menudesc))
			       (list (car (car menudesc))) t) menu)
	    menudesc (cdr menudesc)))
    (setq menu (cons "WWW" menu))
    (popup-menu menu)
    (catch 'popup-done
      (while t
	(setq event (next-command-event event))
	(cond ((and (menu-event-p event) (stringp (car-safe
						   (event-object event))))
	       (throw 'popup-done (event-object event)))
	      ((and (menu-event-p event)
		    (or (eq (event-object event) 'abort)
			(eq (event-object event) 'menu-no-selection-hook)))
	       (signal 'quit nil))
	      ((button-release-event-p event);; don't beep twice
	       nil)
	      (t
	       (beep)
	       (message "please make a choice from the menu.")))))))

(defun sgml-install-lucid-menus ()
  "Install lucid menus for psgml mode"
  (set-buffer-menubar (copy-sequence default-menubar))
  (add-menu nil (car sgml-sgml-menu) (cdr sgml-sgml-menu) "Help")
  (add-menu nil (car sgml-markup-menu) (cdr sgml-markup-menu) "Help")
  (add-menu nil (car sgml-fold-menu) (cdr sgml-fold-menu) "Help")
  (add-menu nil (car sgml-tags-menu) (cdr sgml-tags-menu) "Help")
  (add-menu nil (car sgml-entities-menu) (cdr sgml-entities-menu) "Help")
  (add-menu nil (car sgml-dtd-menu) (cdr sgml-dtd-menu) "Help"))

(defvar sgml-markup-menu
  '("Markup"
    ["<!entity ... >" (insert "<!entity \r>\n") t]
    ["<!attlist ... >" (insert "<!attlist \r>\n") t]
    ["<!element ... >" (insert "<!element \r>\n") t]
    "----------"
    ["Local variables comment" (insert "<!--\nLocal variables:\n\rEnd:\n-->\n")
     t]
    ["Comment" (insert "<!-- \r -->\n") t]
    ["Doctype" (insert "<!doctype \r -- public or system -- [\n]>\n") t]
    "----------"
    ["TEMP marked section" (insert "<![TEMP[\r]]>") t]
    ["RCDATA marked section" (insert "<![RCDATA[\r]]>\n") t]
    ["CDATA marked section" (insert "<![CDATA[\r]]>\n") t]
    ["Marked section" (insert "<![ [\r]]>\n") t]
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
    ["Save options" sgml-save-options t]
    ["Options" sgml-options-menu t]
    ["Show warning log" sgml-show-or-clear-log t]
    ["Show valid tags" sgml-list-valid-tags t]
    ["Change element name" sgml-change-element-name t]
    ["Edit attributes" sgml-edit-attributes t]
    ["Next trouble spot" sgml-next-trouble-spot t]
    ["Show context" sgml-show-context t]
    ["End element" sgml-insert-end-tag t]
    ["Next data field" sgml-next-data-field t]
    ))

(defun sgml-build-custom-menus ()
  (and sgml-custom-markup (add-menu-item '("Markup") "------------" nil t))
  (mapcar (function
	   (lambda (x)
	     (add-menu-item '("Markup") (nth 0 x) (list 'insert (nth 1 x)) t)))
	  sgml-custom-markup)
  (and sgml-custom-dtd (add-menu-item '("Markup") "-------------" nil t))
  (mapcar (function
	   (lambda (x)
	     (add-menu-item '("Markup") (nth 0 x)
			    (list 'progn
				  (list 'if (nth 1 x)
					(list 'insert (nth 1 x)))
				  (list 'if (nth 2 x)
					(list 'sgml-load-dtd (nth 2 x)))) t)))
	  sgml-custom-dtd))
  
(define-key sgml-mode-map [button3] 'sgml-tags-menu)

(provide 'psgml-lucid)
