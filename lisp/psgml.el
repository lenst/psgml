;;;; psgml.el --- SGML-editing mode with parsing support
;; $Id$

;; Copyright (C) 1993, 1994 Lennart Staflin
;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Author: Lennart Staflin <lenst@lysator.liu.se>
;; 	James Clark <jjc@clark.com>

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

;; Major mode for editing the SGML document-markup language.

;; Send bugs to lenst@lysator.liu.se

;; LIMITATIONS

;; - only accepts the referece concrete syntax, though it does allow
;;   unlimited lengths on names
;; - no interpretation of character- and general entity references
;;   (implicitly assumes entities only contain data),
;; - Features not supported: subdoc, concur, datatag, rank, shortref,
;;   and shorttag (most of shortag is supported)

;; WHAT IT CAN DO

;; - Identify structural errors (but it is not a validator)
;; - Menus for inserting tags with only the contextually valid tags
;; - Edit attribute values in separate window with information about types 
;;   and defaults
;; - Hide attributes
;; - Fold elements
;; - Indent according to element nesting depth
;; - Show context
;; - Structure editing: move and kill by element
;; - Find next data context

;; TODO
;; - SHORTREF
;; - Better error recovery in the parser

;; BUGS
;; - "*SGML LOG*" buffer handling is confusing



;;;; Code:

(defconst psgml-version "0.4a4"
  "Version of psgml package.")

(defconst psgml-maintainer-address "lenst@lysator.liu.se")

(require 'cl)

(defvar sgml-debug nil)

(defmacro sgml-debug (&rest x) 
  (list 'if 'sgml-debug (cons 'sgml-log-warning x)))


;;;; Variables
;;; User settable options:

(defvar sgml-markup-faces '((start-tag . bold)
			    (end-tag . bold)
			    (comment . italic)
			    (pi . bold)
			    (sgml . bold)
			    (doctype . bold))
  "*Alist of markup to face mappings.
Each element looks like (MARKUP-TYPE . FACE).
Possible values for MARKUP-TYPE is:
comment	- comment declaration
doctype	- doctype declaration
end-tag 
ignored	- ignored marked section
ms-end	- marked section start, if not ignored 
ms-start - marked section end, if not ignored
pi	- processing instruction
sgml	- SGML declaration
start-tag")

(defvar sgml-buggy-subst-char-in-region 
  (or (not (boundp 'emacs-minor-version))
      (not (natnump emacs-minor-version))
      (< emacs-minor-version 23))
  "*If non-nil, work around a bug in subst-char-in-region.
The bug sets the buffer modified.  If this is set, folding commands
will be slower.")

(defvar sgml-set-face nil
  "*If non-nil, psgml will set the face of parsed markup.")
(put 'sgml-set-face 'sgml-desc "Set face of parsed markup")

(defvar sgml-live-element-indicator nil
  "*If non-nil, indicate current element in mode line.
NOTE: this implies that every command can cause a parse.
Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'sgml-live-element-indicator)

(defvar sgml-offer-save t
  "*If non-nil, ask about saving modified buffers before \\[sgml-validate] is run.")

(defvar sgml-parent-document nil
  "*File name (a string) of a file containing a DOCTYPE declaration to use,
or a list (FILENAME DOCTYPENAME), where FILENAME is a file name of a file '
containing a DOCTYPE declaration to use with the modification that the
document type name is DOCTYPENAME.
Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'sgml-parent-document)
(put 'sgml-parent-document 'sgml-type 'string)

(defvar sgml-tag-region-if-active nil
  "*If non-nil, the Tags menu will tag a region if the region is 
considered active by emacs.  If nil, region must be active and
transient-mark-mode must be on for the region to be tagged.")

(defvar sgml-normalize-trims t
  "*If non-nil, sgml-normalize will trim off white space from end of element
when adding end tag.")

(defvar sgml-omittag t
  "*Set to non-nil, if you use OMITTAG YES.
Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'sgml-omittag)

(defvar sgml-shorttag t
  "*Set to non-nil, if you use SHORTTAG YES.
Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'sgml-shorttag)

(defvar sgml-minimize-attributes nil
  "*Determines minimization of attributes inserted by edit-attributes.
Actually two things are done
1. If non-nil, omit attribute name, if attribute value is from a token group.
2. If 'max, omit attributes with default value.
Setting this variable automatically makes it local to the current buffer.")
(make-local-variable 'sgml-minimize-attributes)

(defvar sgml-max-menu-size 30
  "*Max number of entries in Tags and Entities menus before they are split
into several panes.")

(defvar sgml-always-quote-attributes t
  "*If non-nil, quote all attribute values inserted after finishing edit attributes.
Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'sgml-always-quote-attributes)

(defvar sgml-auto-insert-required-elements t
  "*If non-nil, automatically insert required elements in the content
of an inserted element.")

(defvar sgml-balanced-tag-edit t
  "*If non-nil, always insert start-end tag pairs.")

(defvar sgml-omittag-transparent (not sgml-balanced-tag-edit)
  "*If non-nil, will show legal tags inside elements with omitable start tags
and legal tags beyond omitable end tags.")

(defvar sgml-leave-point-after-insert nil
  "*If non-nil, the point will remain after inserted tag(s).
If nil, the point will be placed before the inserted tag(s).")

(defvar sgml-warn-about-undefined-elements t
  "*If non-nil, print a warning when a tag for an undefined element is found.")

(defvar sgml-indent-step 2
  "*How much to increment indent for every element level.
If nil, no indentation.
Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'sgml-indent-step)
(put 'sgml-indent-step 'sgml-type '(("None" . nil) 0 1 2 3 4 5 6 7 8))

(defvar sgml-indent-data nil
  "*If non-nil, indent in data/mixed context also.
Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'sgml-indent-data)

(defvar sgml-system-path
  '("." "~/sgml")
  "*List of directories used to look for system identifiers.")

(defvar sgml-public-map
  '("/usr/local/lib/sgml/%o/%c/%d")

  "*Mapping from public identifiers to file names.
This is a list of possible file names.  To find the file for a public
identifier the elements of the list are used one at the time from the
beginning.  If the element is a string a file name is constructed from
the string by substitution of the whole public identifier for %P,
owner for %O, public text class for %C, and public text description
for %D.  The text class will be converted to lower case and the owner
and description will be transliterated according to the variable
sgml-public-transliterations.  If the file exists it will be the file
used for the public identifier.  An element can also be a dotted pair
(regexp . filename), the filename is a string treated as above, but
only if the regular expression, regexp, matches the public
identifier.")

(defvar sgml-public-transliterations '((? . ?_) (?/ . ?%))
  "*Transliteration for characters that should be avoided in file names.
This is a list of dotted pairs (FROM . TO); where FROM is the the
character to be translated to TO.  This is used when parts of a public
identifier are used to construct a file name.")

(defvar sgml-default-dtd-file nil
  "*This is the default file name for saved DTD.
This is set by sgml-mode from the buffer file name.
Can be changed in the Local variables section of the file.")
(put 'sgml-default-dtd-file 'sgml-type 'string)

(defvar sgml-custom-markup nil
  "*Menu entries to be added to the Markup menu.
The value should be a list of lists of two strings.  The first is a
string is the menu line and the second string is the text inserted
when the menu item is chosen.  The second string can contain a \\r
where the cursor should be left.  Also if a selection is made
according the same rules as for the Tags menu, the selection is
replaced with the second string and \\r is replaced with the
selection.

Example:

  ((\"Version1\" \"<![%Version1[\\r]]>\")
   (\"New page\"  \"<?NewPage>\"))
")

(defvar sgml-custom-dtd nil
  "Menu entries to be added to the DTD menu.
The value should be a list of entrys to be added to the DTD menu.
Every entry should be a list. The first element of the entry is a string
used as the menu entry.  The second element is a string containing a
doctype declaration (this can be nil if no doctype).  The rest of the
list should be a list of variables and values.  For backward
compatibility a singel string instead of a variable is assigned to
sgml-default-dtd-file.  All variables are made buffer local and are also
added to the buffers local variables list.

Example:
   ((\"HTML\" nil
     sgml-default-dtd-file \"~/sgml/html.ced\"
     sgml-omittag nil sgml-shorttag nil)
    (\"HTML+\" \"<!doctype htmlplus system 'htmlplus.dtd'>\"
     \"~/sgml/htmlplus.ced\"
     sgml-omittag t sgml-shorttag nil)
    (\"DOCBOOK\" \"<!doctype docbook system 'docbook.dtd'>\"
     \"~/sgml/docbook.ced\"
     sgml-omittag nil sgml-shorttag t)))
")


;;; Faces used in edit attribute buffer:
(put 'sgml-default 'face 'underline)	; Face for #DEFAULT
(put 'sgml-fixed 'face 'underline)	; Face of #FIXED "..."


;;; sgmls is a free SGML parser available from
;;; ftp.uu.net:pub/text-processing/sgml
;;; Its error messages can be parsed by next-error.
;;; The -s option suppresses output.

(defvar sgml-validate-command
  "sgmls -s"
  "*The command to validate an SGML document.
The file name of current buffer file name will be appended to this,
separated by a space.")

(defvar sgml-validate-error-regexps
  '(("\\(error\\|warning\\) at \\([^,]+\\), line \\([0-9]+\\)" 2 3))
  "Alist of regexps to recognize error messages from sgml-validate.")

(defvar sgml-declaration nil
  "*If this is a string this will be inserted before the file name when 
running the sgml-validate-command.")
(put 'sgml-declaration 'sgml-type 'string)

(defvar sgml-mode-hook nil
  "A hook or list of hooks to be run when entering sgml-mode")

(defconst sgml-user-options
  '(sgml-indent-data
    sgml-indent-step
    sgml-leave-point-after-insert
    sgml-auto-insert-required-elements
    sgml-balanced-tag-edit
    sgml-omittag-transparent
    sgml-warn-about-undefined-elements
    sgml-always-quote-attributes
    sgml-normalize-trims
    sgml-omittag
    sgml-shorttag
    sgml-minimize-attributes
    sgml-live-element-indicator
    sgml-set-face
    sgml-parent-document
    sgml-system-path
    sgml-public-map
    sgml-default-dtd-file
    sgml-validate-command
    sgml-declaration
    )
  "User options that can be saved or set from menu."
  )

;;; Internal variables

(defvar sgml-saved-validate-command nil
  "The command last used to validate in this buffer.")

(defvar sgml-running-lucid (string-match "Lucid" emacs-version))

(defvar sgml-mode-map nil "Keymap for SGML mode")

(defvar sgml-active-dtd-indicator nil
  "Displayed in the mode line")


;;;; User options handling

(defun sgml-variable-description (var)
  (or (get var 'sgml-desc)
      (let ((desc (symbol-name var)))
	(if (string= "sgml-" (substring desc 0 5))
	    (setq desc (substring desc 5)))
	(loop for c across-ref desc
	      do (if (eq c ?-) (setf c ? )))
	(capitalize desc))))

(defun sgml-variable-type (var)
  (or (get var 'sgml-type)
      (if (memq (symbol-value var) '(t nil))
	  'toggle)))

(defun sgml-set-local-variable (var val)
  "Set the value of variable VAR to VAL in buffer and local variables list."
  (set (make-local-variable var) val)
  (save-excursion
    (let ((prefix "") 
	  (suffix "")
	  (case-fold-search t))
      (goto-char (max (point-min) (- (point-max) 3000)))
      (cond ((search-forward "Local Variables:" nil t)
	     (setq suffix (buffer-substring (point)
					    (save-excursion (end-of-line 1)
							    (point))))
	     (setq prefix
		   (buffer-substring (save-excursion (beginning-of-line 1)
						     (point))
				     (match-beginning 0))))
	    (t
	     (goto-char (point-max))
	     (unless (bolp)
	       (insert ?\n))
	     (insert
	      "<!-- Keep this comment at the end of the file\n"
	      "Local variables:\n"
	      "mode: sgml\n"
	      "End:\n"
	      "-->\n")
	     (forward-line -3)))
      (let* ((endpos (save-excursion
		       (search-forward (format "\n%send:" prefix))))
	     (varpos (search-forward (format "\n%s%s:" prefix var) endpos t)))
	(cond (varpos
	       (delete-region (point)
			      (save-excursion (end-of-line 1)
					      (point)))
	       (insert (format "%S" val) suffix))
	      (t
	       (goto-char endpos)
	       (beginning-of-line 1)
	       (insert prefix (format "%s:%S" var val) suffix ?\n)))))))

(defun sgml-save-options ()
  "Save user options for sgml-mode that have buffer local values."
  (interactive)
  (let ((l sgml-user-options)
	(bv (buffer-local-variables)))
    (loop for var in sgml-user-options
	  do
	  (when (and (assq var bv)
		     (or (not (eq var 'sgml-default-dtd-file))
			 (stringp sgml-default-dtd-file)))
	    (sgml-set-local-variable var (symbol-value var))))
    ;;(when sgml-default-dtd-file (sgml-set-local-variable 'sgml-default-dtd-file sgml-default-dtd-file))
    ))


;;;; SGML mode: template functions

(defun sgml-markup (entry text)
  (cons entry
	(` (lambda ()
	     (interactive)
	     (sgml-insert-markup (, text))))))

(defun sgml-insert-markup (text)
  (let ((end (sgml-mouse-region))
	before after
	old-text)
    (when end
      (setq old-text (buffer-substring (point) end))
      (delete-region (point) end))
    (setq before (point))
    (insert text)
    (setq after (point))
    (goto-char before)
    (when (search-forward "\r" after t)
      (delete-char -1))
    (when old-text (insert old-text))))

(defun sgml-mouse-region ()
  (let (start end)
    (cond
     (sgml-running-lucid
      (cond
       ((null (mark-marker)) nil)
       (t (setq start (region-beginning)
 		end (region-end)))))
     ((and transient-mark-mode
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
    end))


;;;; SGML mode: indentation 

(defun sgml-indent-or-tab ()
  "Indent line in proper way for current major mode."
  (interactive)
  (if (null sgml-indent-step)
      (insert-tab)
    (funcall indent-line-function)))

;;;; Bug reporting

(eval-and-compile
  (autoload 'reporter-submit-bug-report "reporter"))

(defun sgml-submit-bug-report ()
  "Submit via mail a bug report on PSGML."
  (interactive)
  (and (y-or-n-p "Do you really want to submit a report on PSGML? ")
       (reporter-submit-bug-report
	psgml-maintainer-address
	(concat "psgml.el " psgml-version)
	(list 
	 'sgml-parent-document
	 'sgml-tag-region-if-active
	 'sgml-normalize-trims
	 'sgml-omittag
	 'sgml-shorttag
	 'sgml-minimize-attributes
	 'sgml-always-quote-attributes
	 'sgml-auto-insert-required-elements
	 'sgml-balanced-tag-edit
	 'sgml-omittag-transparent
	 'sgml-leave-point-after-insert
	 'sgml-indent-step
	 'sgml-indent-data
	 ))))


;;;; SGML mode: keys and menus

(if sgml-mode-map
    ()
  (setq sgml-mode-map (make-sparse-keymap)))

;;; Key commands

(define-key sgml-mode-map "\t"    'sgml-indent-or-tab)
;(define-key sgml-mode-map "<" 	  'sgml-insert-tag)
(define-key sgml-mode-map ">"     'sgml-close-angle)
(define-key sgml-mode-map "/"     'sgml-slash)
(define-key sgml-mode-map "\C-c#"    'sgml-make-character-reference)
(define-key sgml-mode-map "\C-c-"    'sgml-untag-element)
(define-key sgml-mode-map "\C-c+"    'sgml-insert-attribute)
(define-key sgml-mode-map "\C-c/"    'sgml-insert-end-tag)
(define-key sgml-mode-map "\C-c<"    'sgml-insert-tag)
(define-key sgml-mode-map "\C-c="    'sgml-change-element-name)
(define-key sgml-mode-map "\C-c\C-a" 'sgml-edit-attributes)
(define-key sgml-mode-map "\C-c\C-c" 'sgml-show-context)
(define-key sgml-mode-map "\C-c\C-d" 'sgml-next-data-field)
(define-key sgml-mode-map "\C-c\C-e" 'sgml-insert-element)
(define-key sgml-mode-map "\C-c\C-k" 'sgml-kill-markup)
(define-key sgml-mode-map "\C-c\C-l" 'sgml-show-or-clear-log)
(define-key sgml-mode-map "\C-c\C-n" 'sgml-up-element)
(define-key sgml-mode-map "\C-c\C-o" 'sgml-next-trouble-spot)
(define-key sgml-mode-map "\C-c\C-p" 'sgml-parse-prolog)
(define-key sgml-mode-map "\C-c\C-q" 'sgml-fill-element)
(define-key sgml-mode-map "\C-c\C-r" 'sgml-tag-region)
(define-key sgml-mode-map "\C-c\C-s" 'sgml-unfold-line)
(define-key sgml-mode-map "\C-c\C-t" 'sgml-list-valid-tags)
(define-key sgml-mode-map "\C-c\C-v" 'sgml-validate)
(define-key sgml-mode-map "\C-c\C-w" 'sgml-what-element)
(define-key sgml-mode-map "\C-c\C-f\C-e" 'sgml-fold-element)
(define-key sgml-mode-map "\C-c\C-f\C-r" 'sgml-fold-region)
(define-key sgml-mode-map "\C-c\C-f\C-s" 'sgml-fold-subelement)
(define-key sgml-mode-map "\C-c\C-f\C-x" 'sgml-expand-element)
(define-key sgml-mode-map "\C-c\r"   'sgml-split-element)
(define-key sgml-mode-map "\C-c\C-u\C-e" 'sgml-unfold-element)
(define-key sgml-mode-map "\C-c\C-u\C-a" 'sgml-unfold-all)
(define-key sgml-mode-map "\C-c\C-u\C-l" 'sgml-unfold-line)

(define-key sgml-mode-map "\e\C-a"   'sgml-beginning-of-element)
(define-key sgml-mode-map "\e\C-e"   'sgml-end-of-element)
(define-key sgml-mode-map "\e\C-f"   'sgml-forward-element)
(define-key sgml-mode-map "\e\C-b"   'sgml-backward-element)
(define-key sgml-mode-map "\e\C-d"   'sgml-down-element)
(define-key sgml-mode-map "\e\C-u"   'sgml-backward-up-element)
(define-key sgml-mode-map "\e\C-k"   'sgml-kill-element)
(define-key sgml-mode-map "\e\C-@"   'sgml-mark-element)
;;(define-key sgml-mode-map [?\M-\C-\ ] 'sgml-mark-element)
(define-key sgml-mode-map "\e\C-h"   'sgml-mark-current-element)
(define-key sgml-mode-map "\e\C-t"   'sgml-transpose-element)
(define-key sgml-mode-map "\M-\t"    'sgml-complete)

;;; Menu bar

(eval-when-compile
  (autoload 'sgml-build-custom-menus "psgml-other")) ; Avoid compiler warnings

;; load menu file at the end


;;;; SGML mode: major mode definition

;;; This section is mostly from sgml-mode by James Clark.

;;;###autoload
(defun sgml-mode ()
  "Major mode for editing SGML.\\<sgml-mode-map>
Makes > display the matching <.  Makes / display matching /.
Use \\[sgml-validate] to validate your document with an SGML parser.

You can find information with:
\\[sgml-show-context]  Show the nesting of elements at cursor position.
\\[sgml-list-valid-tags]  Show the tags valid at cursor position.

Insert tags with completion of contextually valid tags with \\[sgml-insert-tag].
End the current element with \\[sgml-insert-end-tag].  Insert an element (i.e.
both start and end tag) with \\[sgml-insert-element].  Or tag a region with 
\\[sgml-tag-region]. 

To tag a region with the mouse, use transient mark mode or secondary selection.

Structure editing:
\\[sgml-backward-element]  Moves backwards over the previous element.
\\[sgml-forward-element]  Moves forward over the nex element.
\\[sgml-down-element]  Move forward and down one level in the element structure.
\\[sgml-backward-up-element]  Move backward out of this element level.
\\[sgml-beginning-of-element]  Move to after the start tag of the current element.
\\[sgml-end-of-element]  Move to before the end tag of the current element.
\\[sgml-kill-element]  Kill the element following the cursor.

Finding interesting positions
\\[sgml-next-data-field]  Move forward to next point where data is allowed.
\\[sgml-next-trouble-spot]  Move forward to next point where something is 
	amiss with the structure.

Folding and unfolding
\\[sgml-fold-element]  Fold the lines comprising the current element, leaving 
	the first line visible.
\\[sgml-fold-subelement]  Fold the elements in the content of the current element.
	Leaving the first line of every element visible.
\\[sgml-unfold-line]  Show hidden lines in current line.

User options:

sgml-omittag  Set this to reflect OMITTAG in the SGML declaration.
sgml-shortag  Set this to reflect SHORTTAG in the SGML declaration.
sgml-auto-insert-required-elements  If non-nil, automatically insert required 
	elements in the content of an inserted element.
sgml-balanced-tag-edit  If non-nil, always insert start-end tag pairs.
sgml-omittag-transparent  If non-nil, will show legal tags inside elements
	with omitable start tags and legal tags beyond omitable end tags.
sgml-leave-point-after-insert  If non-nil, the point will remain after 
	inserted tag(s).
sgml-warn-about-undefined-elements  If non-nil, print a warning when a tag 
	for a undefined element is found.
sgml-max-menu-size  Max number of entries in Tags and Entities menus before
 	they are split into several panes.
sgml-always-quote-attributes  If non-nil, quote all attribute values 
	inserted after finishing edit attributes.
sgml-minimize-attributes  Determines minimization of attributes inserted by 
	edit-attributes.
sgml-normalize-trims  If non-nil, sgml-normalize will trim off white space 
	from end of element when adding end tag.
sgml-indent-step  How much to increament indent for every element level.
sgml-indent-data  If non-nil, indent in data/mixed context also.
sgml-system-path  List of directorys used to look for system identifiers.
sgml-public-map  Mapping from public identifiers to file names.
sgml-offer-save  If non-nil, ask about saving modified buffers before
		\\[sgml-validate] is run.

All bindings:
\\{sgml-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (setq local-abbrev-table text-mode-abbrev-table)
  (use-local-map sgml-mode-map)
  (setq mode-name "SGML")
  (setq major-mode 'sgml-mode)

  ;; A start or end tag by itself on a line separates a paragraph.
  ;; This is desirable because SGML discards a newline that appears
  ;; immediately after a start tag or immediately before an end tag.

  (set (make-local-variable 'paragraph-separate)
	"^[ \t\n]*$\\|\
^[ \t]*</?\\([A-Za-z]\\([-.A-Za-z0-9= \t\n]\\|\
\"[^\"]*\"\\|'[^']*'\\)*\\)?>$")
  (set (make-local-variable 'paragraph-start)
       paragraph-separate)

  (make-local-variable 'sgml-saved-validate-command)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "<!-- ")
  (make-local-variable 'comment-end)
  (setq comment-end " -->")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'sgml-comment-indent)
  (make-local-variable 'comment-start-skip)
  ;; This will allow existing comments within declarations to be
  ;; recognized.  [Does not work well with auto-fill, Lst/940205]
  ;;(setq comment-start-skip "--[ \t]*")
  (setq comment-start-skip "<!--[ \t]*")
  ;; Added for psgml:
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'sgml-indent-line)
  (make-local-variable 'mode-line-format)
  (setq mode-line-format
	'("" mode-line-modified mode-line-buffer-identification
	  "   " global-mode-string
	  "   %[(" mode-name sgml-active-dtd-indicator
	  minor-mode-alist "%n" mode-line-process ")%]--"
	  (line-number-mode "L%l--") (-3 . "%p") "-%-"))
  (make-local-variable 'sgml-default-dtd-file)
  (when (setq sgml-default-dtd-file (sgml-default-dtd-file))
    (unless (file-exists-p sgml-default-dtd-file)
      (setq sgml-default-dtd-file nil)))
  (run-hooks 'text-mode-hook 'sgml-mode-hook)
  (sgml-build-custom-menus))

(defun sgml-default-dtd-file ()
  (and (buffer-file-name)
       (let ((base (file-name-nondirectory (buffer-file-name))))
	 (concat
	  (cond ((string-match "\\.[^.]+$" base)
		 (substring base 0 (match-beginning 0)))
		(t
		 base))
	  ".ced"))))

(defun sgml-comment-indent ()
  (if (and (looking-at "--")
	   (not (and (eq (char-after (1- (point))) ?!)
		     (eq (char-after (- (point) 2)) ?<))))
      (progn
	(skip-chars-backward " \t")
	(max comment-column (1+ (current-column))))
    0))

(defconst sgml-start-tag-regex
  "<[A-Za-z]\\([-.A-Za-z0-9= \n\t]\\|\"[^\"]*\"\\|'[^']*'\\)*"
  "Regular expression that matches a non-empty start tag.
Any terminating > or / is not matched.")

(defvar sgml-mode-markup-syntax-table nil
  "Syntax table used for scanning SGML markup.")

(if sgml-mode-markup-syntax-table
    ()
  (setq sgml-mode-markup-syntax-table (make-syntax-table))
  (modify-syntax-entry ?< "(>" sgml-mode-markup-syntax-table)
  (modify-syntax-entry ?> ")<" sgml-mode-markup-syntax-table)
  (modify-syntax-entry ?- "_ 1234" sgml-mode-markup-syntax-table)
  (modify-syntax-entry ?\' "\"" sgml-mode-markup-syntax-table))

(defconst sgml-angle-distance 4000
  "*If non-nil, is the maximum distance to search for matching <.")

(defun sgml-close-angle (arg)
  "Insert > and display matching <."
  (interactive "p")
  (insert-char ?> arg)
  (if (> arg 0)
      (let ((oldpos (point))
	    (blinkpos))
	(save-excursion
	  (save-restriction
	    (if sgml-angle-distance
		(narrow-to-region (max (point-min)
				       (- (point) sgml-angle-distance))
				  oldpos))
	    ;; See if it's the end of a marked section.
	    (and (> (- (point) (point-min)) 3)
		 (eq (char-after (- (point) 2)) ?\])
		 (eq (char-after (- (point) 3)) ?\])
		 (re-search-backward "<!\\[\\(-?[A-Za-z0-9. \t\n&;]\\|\
--\\([^-]\\|-[^-]\\)*--\\)*\\["
				     (point-min)
				     t)
		 (let ((msspos (point)))
		   (if (and (search-forward "]]>" oldpos t)
			    (eq (point) oldpos))
		       (setq blinkpos msspos))))
	    ;; This handles cases where the > ends one of the following:
	    ;; markup declaration starting with <! (possibly including a
	    ;; declaration subset); start tag; end tag; SGML declaration.
	    (if blinkpos
		()
	      (goto-char oldpos)
	      (condition-case ()
		  (let ((oldtable (syntax-table))
			(parse-sexp-ignore-comments t))
		    (unwind-protect
			(progn
			  (set-syntax-table sgml-mode-markup-syntax-table)
			  (setq blinkpos (scan-sexps oldpos -1)))
		      (set-syntax-table oldtable)))
		(error nil))
	      (and blinkpos
		   (goto-char blinkpos)
		   (or
		    ;; Check that it's a valid delimiter in context.
		    (not (looking-at
			  "<\\(\\?\\|/?[A-Za-z>]\\|!\\([[A-Za-z]\\|--\\)\\)"))
		    ;; Check that it's not a net-enabling start tag
		    ;; nor an unclosed start-tag.
		    (looking-at (concat sgml-start-tag-regex "[/<]"))
		    ;; Nor an unclosed end-tag.
		    (looking-at "</[A-Za-z][-.A-Za-z0-9]*[ \t]*<"))
		   (setq blinkpos nil)))
	    (if blinkpos
		()
	      ;; See if it's the end of a processing instruction.
	      (goto-char oldpos)
	      (if (search-backward "<?" (point-min) t)
		  (let ((pipos (point)))
		    (if (and (search-forward ">" oldpos t)
			     (eq (point) oldpos))
			(setq blinkpos pipos))))))
	  (if blinkpos
	      (progn
		(goto-char blinkpos)
		(if (pos-visible-in-window-p)
		    (sit-for 1)
		  (message "Matches %s"
			   (buffer-substring blinkpos
					     (progn (end-of-line)
						    (point)))))))))))

;;; I doubt that null end tags are used much for large elements,
;;; so use a small distance here.
(defconst sgml-slash-distance 1000
  "*If non-nil, is the maximum distance to search for matching /.")

(defun sgml-slash (arg)
  "Insert / and display any previous matching /.
Two /s are treated as matching if the first / ends a net-enabling
start tag, and the second / is the corresponding null end tag."
  (interactive "p")
  (insert-char ?/ arg)
  (if (> arg 0)
      (let ((oldpos (point))
	    (blinkpos)
	    (level 0))
	(save-excursion
	  (save-restriction
	    (if sgml-slash-distance
		(narrow-to-region (max (point-min)
				       (- (point) sgml-slash-distance))
				  oldpos))
	    (if (and (re-search-backward sgml-start-tag-regex (point-min) t)
		     (eq (match-end 0) (1- oldpos)))
		()
	      (goto-char (1- oldpos))
	      (while (and (not blinkpos)
			  (search-backward "/" (point-min) t))
		(let ((tagend (save-excursion
				(if (re-search-backward sgml-start-tag-regex
							(point-min) t)
				    (match-end 0)
				  nil))))
		  (if (eq tagend (point))
		      (if (eq level 0)
			  (setq blinkpos (point))
			(setq level (1- level)))
		    (setq level (1+ level)))))))
	  (if blinkpos
	      (progn
		(goto-char blinkpos)
		(if (pos-visible-in-window-p)
		    (sit-for 1)
		  (message "Matches %s"
			   (buffer-substring (progn
					       (beginning-of-line)
					       (point))
					     (1+ blinkpos))))))))))

(eval-and-compile
  (autoload 'compile-internal "compile" ""))

(defun sgml-validate (command)
  "Validate an SGML document.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer *compilation*.
You can then use the command \\[next-error] to find the next error message
and move to the line in the SGML document that caused it."
  (interactive
   (list (read-string "Validate command: "
		      (or sgml-saved-validate-command
			  (concat sgml-validate-command
				  " "
				  (if (stringp sgml-declaration)
				      (concat sgml-declaration " ")
				    "")
				  (let ((name (buffer-file-name)))
				    (and name
					 (file-name-nondirectory name))))))))
  (setq sgml-saved-validate-command command)
  (if sgml-offer-save
      (save-some-buffers nil nil))
  (compile-internal command "No more errors" "SGML validation"
		    nil
		    sgml-validate-error-regexps))



;;;; Autoloads and hooks

(autoload 'sgml-doctype-insert "psgml-parse"
	  nil
	  nil nil)

(autoload 'sgml-parse-prolog "psgml-dtd"
	  "Parse the document prolog to learn the DTD."
	  t nil)
(autoload 'sgml-save-dtd "psgml-dtd"
	  "Save the parsed dtd on FILE."
	  t nil)

(autoload 'sgml-indent-line "psgml-parse"
	  "Indent line, calling parser to determine level unless COL or ELEMENT
is given.  If COL is given it should be the column to indent to.  If
ELEMENT is given it should be a parse tree node, from which the level
is determined."
	  nil nil)

;;; Generated by sgml-build-autoloads

(autoload 'sgml-load-dtd "psgml-parse" "Load a saved DTD from FILE." t)
(autoload 'sgml-show-or-clear-log "psgml-parse" "Show the *SGML LOG* buffer if it is not showing, or clear and
remove it if it is showing." t)
(autoload 'sgml-beginning-of-element "psgml-parse" "Move to after the start-tag of the current element.
If the start-tag is implied, move to the start of the element." t)
(autoload 'sgml-end-of-element "psgml-parse" "Move to before the end-tag of the current element." t)
(autoload 'sgml-backward-up-element "psgml-parse" "Move backward out of this element level.
That is move to before the start-tag or where a start-tag is implied." t)
(autoload 'sgml-up-element "psgml-parse" "Move forward out of this element level.
That is move to after the end-tag or where an end-tag is implied." t)
(autoload 'sgml-forward-element "psgml-parse" "Move forward over next element." t)
(autoload 'sgml-backward-element "psgml-parse" "Move backward over previous element at this level.
With implied tags this is ambigous." t)
(autoload 'sgml-down-element "psgml-parse" "Move forward and down one level in the element structure." t)
(autoload 'sgml-kill-element "psgml-parse" "Kill the element following the cursor." t)
(autoload 'sgml-transpose-element "psgml-parse" "Interchange element before point with element after point, leave point after." t)
(autoload 'sgml-mark-element "psgml-parse" "Set mark after next element." t)
(autoload 'sgml-mark-current-element "psgml-parse" "Set mark at end of current element, and leave point before current element." t)
(autoload 'sgml-change-element-name "psgml-parse" "Replace the name of the current element with a new name.
Eventual attributes of the current element will be translated if 
possible." t)
(autoload 'sgml-untag-element "psgml-parse" "Remove tags from current element." t)
(autoload 'sgml-kill-markup "psgml-parse" "Kill next tag, markup declaration or process instruction." t)
(autoload 'sgml-fold-region "psgml-parse" "Hide (or if prefixarg unhide) region.
If called from a program first two arguments are start and end of
region. And optional third argument true unhides." t)
(autoload 'sgml-fold-element "psgml-parse" "Fold the lines comprising the current element, leaving the first line visible.
This uses the selective display feature." t)
(autoload 'sgml-fold-subelement "psgml-parse" "Fold all elements current elements content, leaving the first lines visible.
This uses the selective display feature." t)
(autoload 'sgml-unfold-line "psgml-parse" "Show hidden lines in current line." t)
(autoload 'sgml-unfold-element "psgml-parse" "Show all hidden lines in current element." t)
(autoload 'sgml-expand-element "psgml-parse" "As sgml-fold-subelement, but unfold first." t)
(autoload 'sgml-unfold-all "psgml-parse" "Show all hidden lines in buffer." t)
(autoload 'sgml-next-data-field "psgml-parse" "Move forward to next point where data is allowed." t)
(autoload 'sgml-next-trouble-spot "psgml-parse" "Move forward to next point where something is amiss with the structure." t)
(autoload 'sgml-list-valid-tags "psgml-parse" "Display a list of the contextually valid tags." t)
(autoload 'sgml-show-context "psgml-parse" "Display where the cursor is in the element hierarchy." t)
(autoload 'sgml-what-element "psgml-parse" "Display what element is under the cursor." t)
(autoload 'sgml-insert-tag "psgml-parse" "Insert a tag, reading tag name in minibuffer with completion.
If the variable sgml-balanced-tag-edit is t, also inserts the
corresponding end tag. If sgml-leave-point-after-insert is t, the point
is left after the inserted tag(s), unless the element has som required
content.  If sgml-leave-point-after-insert is nil the point is left
after the first tag inserted." t)
(autoload 'sgml-insert-element "psgml-parse" "Reads element name from minibuffer and inserts start and end tags." t)
(autoload 'sgml-tag-region "psgml-parse" "Reads element name from minibuffer and inserts start and end tags." t)
(autoload 'sgml-insert-end-tag "psgml-parse" "Insert end-tag for the current open element." t)
(autoload 'sgml-insert-attribute "psgml-parse" "Read attribute name and value from minibuffer and insert attribute spec." t)
(autoload 'sgml-split-element "psgml-parse" "Split the current element at point.
If repeated, the containing element will be split before the beginning
of then current element." t)
(autoload 'sgml-tags-menu "psgml-parse" "Pop up a menu with valid tags and insert the choosen tag.
If the variable sgml-balanced-tag-edit is t, also inserts the
corresponding end tag. If sgml-leave-point-after-insert is t, the point
is left after the inserted tag(s), unless the element has som required
content.  If sgml-leave-point-after-insert is nil the point is left
after the first tag inserted." t)
(autoload 'sgml-element-menu "psgml-parse" "Pop up a menu with valid elements and insert choice.
If sgml-leave-point-after-insert is nil the point is left after the first 
tag inserted." t)
(autoload 'sgml-start-tag-menu "psgml-parse" "Pop up a menu with valid start-tags and insert choice." t)
(autoload 'sgml-end-tag-menu "psgml-parse" "Pop up a menu with valid end-tags and insert choice." t)
(autoload 'sgml-tag-region-menu "psgml-parse" "Pop up a menu with valid elements and tag current region with the choice." t)
(autoload 'sgml-entities-menu "psgml-parse" nil t)
(autoload 'sgml-attrib-menu "psgml-parse" "Pop up a menu of the attributes of the current element
\(or the element whith start-tag before point)." t)
(autoload 'sgml-fill-element "psgml-parse" "Fill bigest enclosing element with mixed content.
If current element has pure element content, recursively fill the
subelements." t)
(autoload 'sgml-edit-attributes "psgml-parse" "Edit attributes of current element.
Editing is done in a separate window." t)
(autoload 'sgml-hide-tags "psgml-parse" nil t)
(autoload 'sgml-show-tags "psgml-parse" nil t)
(autoload 'sgml-hide-attributes "psgml-parse" nil t)
(autoload 'sgml-show-attributes "psgml-parse" nil t)
(autoload 'sgml-normalize "psgml-parse" "Normalize buffer by filling in omitted tags and expanding empty tags.
A  optional argument ELEMENT can be the element to normalize
insted of the whole buffer." t)
(autoload 'sgml-normalize-element "psgml-parse" nil t)
(autoload 'sgml-make-character-reference "psgml-parse" "Convert character after point into a character reference.
If called with a numeric argument, convert a charactyer reference back
to a normal character.  If called from a program, set optional
argument INVERT to non-nil." t)
(autoload 'sgml-complete "psgml-parse" "Complete the word/tag/entity before point.
If it is a tag (starts with < or </) complete with valid tags.
If it is an entity (starts with &) complete with declared entities.
If it is a markup declaration (starts with <!) complete with markup 
declaration names.
If it is something else complete with ispell-complete-word." t)
(autoload 'sgml-options-menu "psgml-parse" nil t)



;;;; Last provisions
(provide 'psgml)
(provide 'sgml-mode)

(cond
 (sgml-running-lucid
  (require 'psgml-lucid)
  (add-hook 'sgml-mode-hook 'sgml-install-lucid-menus))
 (t
  (require 'psgml-other)))

;;; psgml.el ends here
