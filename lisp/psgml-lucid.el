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

(require 'psgml)


;;;; These functions are needed for menus, etc. to work in lucid emacs

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
       (while (popup-menu-up-p)
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
  (add-menu nil (car sgml-dtd-menu) (cdr sgml-dtd-menu) "Help"))

(defvar sgml-markup-menu
  '("Markup"
    ["Insert element" (sgml-element-menu last-command-event) t]
    ["Insert start-tag" (sgml-start-tag-menu last-command-event) t]
    ["Insert end-tag" (sgml-end-tag-menu last-command-event) t]
    ["Tag region" (sgml-tag-region-menu last-command-event) t]
    ["Insert attribute" (sgml-attrib-menu last-command-event) t]
    ["Insert entity" (sgml-entities-menu last-command-event) t]
    "----------"
    ["Marked section" (sgml-insert-markup "<![ [\r]]>\n") t]
    ["CDATA marked section" (sgml-insert-markup "<![CDATA[\r]]>\n") t]
    ["RCDATA marked section" (sgml-insert-markup "<![RCDATA[\r]]>\n") t]
    ["TEMP marked section" (sgml-insert-markup "<![TEMP[\r]]>") t]
    "----------"
    ["Comment" (sgml-insert-markup "<!-- \r -->\n") t]
    ["Local variables comment" (sgml-insert-markup "<!--\nLocal Variables:\nmode: sgml\n\rEnd:\n-->\n")
     t]
    "----------"
    ["<!doctype ... >" (sgml-insert-markup "<!doctype \r -- public or system --\n[\n]>\n") t]
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

(defvar sgml-sgml-menu
  (append
   '("SGML"
     ["Next data field"  sgml-next-data-field t]
     ["End element" sgml-insert-end-tag t]
     ["Show context" sgml-show-context t]
     ["What element" sgml-what-element t]
     ["Next trouble spot" sgml-next-trouble-spot t]
     ["Edit attributes" sgml-edit-attributes t]
     ["Change element name" sgml-change-element-name t]
     ["Show valid tags" sgml-list-valid-tags t]
     ["Show/hide warning log" sgml-show-or-clear-log t]
     ["Validate" sgml-validate t]
     ["Normalize" sgml-normalize t]
     ["Fill element" sgml-fill-element t])
   (if (not (boundp 'emacs-major-version))
       '(["Options" sgml-options-menu t])
     (list
      (cons "Options"
	    (mapcar
	     (function
	      (lambda (var)
		(vector (capitalize
			 (mapconcat (function (lambda (x)
						(if (= x ?-) " "
						  (char-to-string x))))
				    (substring (symbol-name var) 5 nil)
				    ""))
			(list 'setq var (list 'not var))
			':style 'toggle ':selected var)))
	     (loop for v in sgml-user-options
		   if (eq 'toggle (sgml-variable-type v))
		   collect v)))
      (cons "Indent Step"
	    (mapcar
	     (function
	      (lambda (entry)
		(vector (car entry)
			(list 'setq 'sgml-indent-step (cdr entry))
			':style 'radio ':active t
			':selected
			(list 'eq 'sgml-indent-step (cdr entry)))))
	     '(("None" . nil)
	       ("0" . 0) ("1" . 1) ("2" . 2) ("3" . 3)
	       ("4" . 4) ("5" . 5) ("6" . 6) ("7" . 7) ("8" . 8))))))
   '(["Save options" sgml-save-options t]
     ["Submit bug report" sgml-submit-bug-report t]
     )))


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
			    (list 'apply ''sgml-doctype-insert
				  (list 'quote (cdr x)))
			    t)))
	  sgml-custom-dtd))

(define-key sgml-mode-map [button3] 'sgml-tags-menu)


;;;; Insert with properties

(facep 'underline)



(defun sgml-insert (props format &rest args)
  (let ((start (point))
	tem face)
    (insert (apply (function format)
		   format
		   args))
    (remf props 'rear-nonsticky)	; not useful in Lucid

    ;; Copy face prop from category
    (when (setq tem (getf props 'category))
      (when (setq tem (get tem 'face))
	  (set-face-underline-p (make-face 'underline) t)
	  (setf (getf props 'face) tem)))

    (add-text-properties start (point) props)

    ;; A read-only value of 1 is used for the text after values
    ;; and this should in Lucid be open at the front.
    (if (eq 1 (getf props 'read-only))
	(set-extent-property
	 (extent-at start nil 'read-only)
	 'start-open t))))


;;;; Set face of markup

(defun sgml-set-face-for (start end type)
  (let ((face (cdr (assq type sgml-markup-faces)))
	o)
    (loop for e being the extents from start to end
	  do (when (extent-property e 'type)
	       (cond ((and (null o)
			   (eq type (extent-property e 'type)))
		      (setq o e))
		     (t (delete-extent e)))))

    (cond (o
	   (set-extent-endpoints o start end))
	  (face
	   (setq o (make-extent start end))
	   (set-extent-property o 'type type)
	   (set-extent-property o 'face face)
	   (set-extent-face o face)))))

(defun sgml-set-face-after-change (start end &optional pre-len)
  (when sgml-set-face
    (let ((o (extent-at start nil 'type)))
      (cond
       ((null o))
       ((= start (extent-start-position o))
	(set-extent-endpoints o end (extent-end-position o)))
       (t (delete-extent o))))))

;(defalias 'next-overlay-at 'next-overlay-change) ; fix bug in cl.el

(defun sgml-clear-faces ()
  (interactive)
  (loop for o being the overlays
	if (extent-property o 'type)
	do (delete-extent o)))


;;;; Functions not in Lucid Emacs

(unless (fboundp 'frame-width)
  (defalias 'frame-width 'screen-width))



;;;; Provide

(provide 'psgml-lucid)


;;; psgml-lucid.el ends here
