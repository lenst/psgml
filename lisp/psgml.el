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
;; better handling of the warning/error log
;; perhaps start attribute edit if inserting a start tag with required
;;  attributes.
;; Hiding attributes (first test done).
;; Fold all elements with GI

;; BUGS
;;  "*SGML LOG*" buffer handling is confusing

;;;; Code:

(defconst psgml-version "0.3a1"
  "Version of psgml package.")

(require 'cl)

(defvar sgml-debug nil)

(defmacro sgml-debug (&rest x) 
  (list 'if 'sgml-debug (cons 'sgml-log-warning x)))


;;;; Variables
;;; User settable options:

(defvar sgml-normalize-trims t
  "*If non-nil, sgml-normalize will trim off white space from end of element
when adding end tag.")

(defvar sgml-omittag t
  "*Determines interpretation of empty start tag.")

(defvar sgml-minimize-attributes t
  "*Determines minimization of attributes inserted by edit-attributes.
Actually two things are done
1. If non-nil, omit attribute name, if attribute value is from a token group.
2. If 'max, omit attributes with default value.")

(defvar sgml-max-menu-size 30
  "*Max number of entries in Tags and Entities menus before they are split
into several panes.")

(defvar sgml-always-quote-attributes nil
  "*If non-nil, quote all attribute values inserted after finishing edit attributes.")

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
  "*If non-nil, print a warning when a tag for a undefined element is found.")

(defvar sgml-indent-step 2
  "*How much to increment indent for every element level.
If nil, no indentation.")

(defvar sgml-indent-data t
  "*If non-nil, indent in data/mixed context also.")

(defvar sgml-system-path
  '("." "~/sgml")
  "*List of directors used to look for system identifiers.")

(defvar sgml-public-map
  '("/usr/local/lib/sgml/%o/%c/%d")

  "*Mapping from public identifiers to file names.
This is a list of possible file names.  To find the file for a public
identifier the elements of the list are used one at the time from the
beginning.  If the element is a string a file name is constructed from
the string by substitution of owner for %O, public text class for %C,
and public text description for %D.  The text class will be converted
to lower case and the owner and description will be transliterated
according to the variable sgml-public-transliterations.  If the file
exists it will be the file used for the public identifier.  An element
can also be a dotted pair (regexp . filename), the filename is a
string treated as above, but only if the regular expression, regexp,
matches the public identifier.")

(defvar sgml-public-transliterations '((? . ?_) (?/ . ?%))
  "*Transliteration for characters that should be avoided in file names.
This is a list of dotted pairs (FROM . TO); where FROM is the the
character to be translated to TO.  This is used when parts of a public
identifier are used to construct a file name.")

(defvar sgml-default-dtd-file nil
  "*This is the default file name for saved DTD.
This is set by sgml-mode from the buffer file name.
Can be changed in the Local variables section of the file.")

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
  "*Menu entries to be added to the DTD menu.
The value should be a list of lists of three strings.  The first
string is the menu entry.  The second string is a doctype declaration
(this can be nil if no doctype).  The third string is the file name of
the saved DTD (this may also be nil).

Example:

    ((\"Html\" nil \"~/sgml/html.ced\")
     (\"docbook\" \"<!doctype docbook system 'docbook.dtd'>\"
		    \"~/sgml/docbook.ced\"))
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

(defvar sgml-declaration nil
  "*If this is a string this will be inserted before the file name when 
running the sgml-validate-command.")


;;; Internal variables

(defvar sgml-saved-validate-command nil
  "The command last used to validate in this buffer.")

(defvar sgml-mode-map nil "Keymap for SGML mode")


;;;; Local variables editing

(defun sgml-set-local-variable (var val)
  (save-excursion
    (let ((prefix "") 
	  (suffix ""))
      (goto-char (max (point-min) (- (point-max) 3000)))
      (cond ((search-forward "local variables:" nil t)
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
	      "<!--\n"
	      "Local variables:\n"
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
  (interactive)
  (let ((l '(sgml-indent-data
	     sgml-indent-step
	     sgml-leave-point-after-insert
	     sgml-auto-insert-required-elements
	     sgml-balanced-tag-edit
	     sgml-omittag-transparent
	     sgml-warn-about-undefined-elements
	     sgml-always-quote-attributes
	     sgml-normalize-trims
	     sgml-omittag
	     sgml-minimize-attributes))
	(bv (buffer-local-variables)))
    (while l
      (when (assq (car l) bv)
	(sgml-set-local-variable (car l)
				 (symbol-value (car l))))
      (setq l (cdr l)))
    (when sgml-default-dtd-file
      (sgml-set-local-variable 'sgml-default-dtd-file sgml-default-dtd-file))))


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

(defun sgml-doctype-insert (doctype saved-dtd)
  (when doctype
    (sgml-insert-markup doctype))
  (when saved-dtd
    (setq sgml-default-dtd-file saved-dtd)
    (sgml-set-local-variable 'sgml-default-dtd-file saved-dtd)))

(defun sgml-mouse-region ()
  (let (start end)
    (cond
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


;;;; SGML mode: keys and menus

(if sgml-mode-map
    ()
  (setq sgml-mode-map (make-sparse-keymap)))

;;; Menu bar

(defvar sgml-markup-menu (make-sparse-keymap "Markup"))
(fset 'sgml-markup-menu sgml-markup-menu)

(define-key sgml-mode-map [menu-bar sgml-dtd]
  (cons "DTD" (make-sparse-keymap "DTD")))

(define-key sgml-mode-map [menu-bar sgml-fold]
  (cons "Fold" (make-sparse-keymap "Fold")))

(define-key sgml-mode-map [menu-bar sgml-markup]
  '("Markup" . sgml-markup-menu ))

(define-key sgml-mode-map [menu-bar sgml-entities]
  '("Entities" . sgml-entities-menu))

(define-key sgml-mode-map [menu-bar sgml-tags]
  '("Tags" . sgml-tags-menu))

(define-key sgml-mode-map [menu-bar sgml]
  (cons "Sgml" (make-sparse-keymap "Sgml")))


;;; Sgml menu

(define-key sgml-mode-map [menu-bar sgml save-options]
  '("Save options" . sgml-save-options))
(define-key sgml-mode-map [menu-bar sgml options]
  '("Options..." . sgml-options-menu))
(define-key sgml-mode-map [menu-bar sgml fill]
  '("Fill element        [C-c C-q]" . sgml-fill-element))
(define-key sgml-mode-map [menu-bar sgml normalize]
  '("Normalize                " . sgml-normalize))
(define-key sgml-mode-map [menu-bar sgml show-log]
  '("Show/hide warning log  [C-c C-l]" . sgml-show-or-clear-log))
(define-key sgml-mode-map [menu-bar sgml show-tags]
  '("List valid tags     [C-c C-t]" . sgml-list-valid-tags))
(define-key sgml-mode-map [menu-bar sgml change-name]
  '("Change element name [C-c =]" . sgml-change-element-name))
(define-key sgml-mode-map [menu-bar sgml edit-attributes]
  '("Edit attributes     [C-c C-a]" . sgml-edit-attributes))
(define-key sgml-mode-map [menu-bar sgml next-trouble]
  '("Next trouble spot   [C-c C-o]" . sgml-next-trouble-spot)) 
(define-key sgml-mode-map [menu-bar sgml what-element]
  '("What element        [C-c C-w]" . sgml-what-element))
(define-key sgml-mode-map [menu-bar sgml show-context]
  '("Show context        [C-c C-c]" . sgml-show-context))
(define-key sgml-mode-map [menu-bar sgml insert-end-tag]
  '("End element         [C-c /]" . sgml-insert-end-tag))
(define-key sgml-mode-map [menu-bar sgml next-data]
  '("Next data field     [C-c C-d]" . sgml-next-data-field))


;;; DTD menu

(define-key sgml-mode-map [menu-bar sgml-dtd save]
  '("Save parsed DTD" . sgml-save-dtd))
(define-key sgml-mode-map [menu-bar sgml-dtd load]
  '("Load parsed DTD" . sgml-load-dtd))
(define-key sgml-mode-map [menu-bar sgml-dtd parse]
  '("Parse DTD" . sgml-parse-prolog))


;;; Fold menu

(define-key sgml-mode-map [menu-bar sgml-fold unfold-all]
  '("Unfold all      [C-c C-u C-a]" . sgml-unfold-all))
(define-key sgml-mode-map [menu-bar sgml-fold fold-region]
  '("Fold region     [C-c C-f C-r]" . sgml-fold-region))
(define-key sgml-mode-map [menu-bar sgml-fold expand]
  '("Expand          [C-c C-f C-x]" . sgml-expand-element))
(define-key sgml-mode-map [menu-bar sgml-fold unfold-element]
  '("Unfold element  [C-c C-u C-e]" . sgml-unfold-element))
(define-key sgml-mode-map [menu-bar sgml-fold unfold]
  '("Unfold line     [C-c C-s]" . sgml-unfold-line))
(define-key sgml-mode-map [menu-bar sgml-fold subfold]
  '("Fold subelement [C-c C-f C-s]"   . sgml-fold-subelement))
(define-key sgml-mode-map [menu-bar sgml-fold fold]
  '("Fold element    [C-c C-f C-e]"   . sgml-fold-element))


;;; Markup menu

(define-key sgml-markup-menu [ entity]
  (sgml-markup "<!entity ... >" "<!entity \r>\n"))
(define-key sgml-markup-menu [ attlist]
  (sgml-markup "<!attlist ... >" "<!attlist \r>\n"))
(define-key sgml-markup-menu [ element]
  (sgml-markup "<!element ... >" "<!element \r>\n"))

(define-key sgml-markup-menu [blank1]
  '("" . nil))

(define-key sgml-markup-menu [lv-comment]
  (sgml-markup "Local variables comment"
	       "<!--\nLocal variables:\n\rEnd:\n-->\n"))
(define-key sgml-markup-menu [ comment]
  (sgml-markup "Comment" "<!-- \r -->\n"))
(define-key sgml-markup-menu [ doctype]
  (sgml-markup "Doctype"
	       "<!doctype \r -- public or system -- [\n]>\n"))

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


;;; Key commands

;(define-key sgml-mode-map "<" 'sgml-insert-tag)
(define-key sgml-mode-map ">" 'sgml-close-angle)
(define-key sgml-mode-map "/" 'sgml-slash)
(define-key sgml-mode-map "\C-c\C-v" 'sgml-validate)
(define-key sgml-mode-map "\C-c/"    'sgml-insert-end-tag)
(define-key sgml-mode-map "\C-c<"    'sgml-insert-tag)
(define-key sgml-mode-map "\C-c="    'sgml-change-element-name)
(define-key sgml-mode-map "\C-c\C-a" 'sgml-edit-attributes)
(define-key sgml-mode-map "\C-c\C-c" 'sgml-show-context)
(define-key sgml-mode-map "\C-c\C-d" 'sgml-next-data-field)
(define-key sgml-mode-map "\C-c\C-e" 'sgml-insert-element)
(define-key sgml-mode-map "\C-c\C-l" 'sgml-show-or-clear-log)
(define-key sgml-mode-map "\C-c\C-n" 'sgml-up-element)
(define-key sgml-mode-map "\C-c\C-o" 'sgml-next-trouble-spot)
(define-key sgml-mode-map "\C-c\C-p" 'sgml-parse-prolog)
(define-key sgml-mode-map "\C-c\C-r" 'sgml-tag-region)
(define-key sgml-mode-map "\C-c\C-s" 'sgml-unfold-line)
(define-key sgml-mode-map "\C-c\C-t" 'sgml-list-valid-tags)
(define-key sgml-mode-map "\C-c\C-q" 'sgml-fill-element)

(define-key sgml-mode-map "\C-c\C-f\C-e" 'sgml-fold-element)
(define-key sgml-mode-map "\C-c\C-f\C-r" 'sgml-fold-region)
(define-key sgml-mode-map "\C-c\C-f\C-s" 'sgml-fold-subelement)
(define-key sgml-mode-map "\C-c\C-f\C-x" 'sgml-expand-element)

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
(define-key sgml-mode-map [?\M-\C-\ ] 'sgml-mark-element)
(define-key sgml-mode-map "\e\C-h"   'sgml-fold-element)
(define-key sgml-mode-map "\e\C-t"   'sgml-transpose-element)
(define-key sgml-mode-map "\M-\t"    'sgml-complete)

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

sgml-auto-insert-required-elements  If non-nil, automatically insert required 
	elements in the content of an inserted element.
sgml-balanced-tag-edit  If non-nil, always insert start-end tag pairs.
sgml-omittag-transparent  If non-nil, will show legal tags inside elements
	with omitable start tags and legal tags beyond omitable end tags.
sgml-leave-point-after-insert  If non-nil, the point will remain after 
	inserted tag(s).
sgml-warn-about-undefined-elements  If non-nil, print a warning when a tag 
	for a undefined element is found.
sgml-indent-step  How much to increament indent for every element level.
sgml-indent-data  If non-nil, indent in data/mixed context also.
sgml-system-path  List of directorys used to look for system identifiers.
sgml-public-map  Mapping from public identifiers to file names.

All bindings:
\\{sgml-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (setq local-abbrev-table text-mode-abbrev-table)
  (use-local-map sgml-mode-map)
  (setq mode-name "SGML")
  (setq major-mode 'sgml-mode)
  (make-local-variable 'paragraph-start)
  ;; A start or end tag by itself on a line separates a paragraph.
  ;; This is desirable because SGML discards a newline that appears
  ;; immediately after a start tag or immediately before an end tag.
  (setq paragraph-start
	"^[ \t\n]*$\\|\
\\(</?\\([A-Za-z]\\([-.A-Za-z0-9= \t\n]\\|\"[^\"]*\"\\|'[^']*'\\)*\\)?>$\\)")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate
	"^[ \t\n]*$\\|\
^</?\\([A-Za-z]\\([-.A-Za-z0-9= \t\n]\\|\"[^\"]*\"\\|'[^']*'\\)*\\)?>$")
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
  (make-local-variable 'sgml-default-dtd-file)
  (when (setq sgml-default-dtd-file (sgml-default-dtd-file))
    (unless (file-exists-p sgml-default-dtd-file)
      (setq sgml-default-dtd-file nil)))
  
  ;; Build custom menus
  (when sgml-custom-markup
    (define-key sgml-mode-map [menu-bar sgml-markup blank-c] '("" . nil)))
  (loop for e in (reverse sgml-custom-markup)
	for i from 0
	do (define-key sgml-mode-map
	     (vector 'menu-bar 'sgml-markup (intern (concat "custom" i)))
	     (sgml-markup (car e) (cadr e))))
  (when sgml-custom-dtd
    (define-key sgml-mode-map [menu-bar sgml-dtd blank-c] '("" . nil)))
  (loop for e in (reverse sgml-custom-dtd)
	for i from 0
	do (define-key sgml-mode-map
	     (vector 'menu-bar 'sgml-dtd (intern (concat "custom" i)))
	     (cons
	      (first e)
	      (` (lambda ()
		   (interactive)
		   (sgml-doctype-insert (, (second e)) (, (third e))))))))
  ;;
  (run-hooks 'text-mode-hook 'sgml-mode-hook))

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
  (compile-internal command "No more errors" "SGML validation"
		    nil
		   '(("error at \\([^,]+\\), line \\([0-9]+\\)" 1 2))))



;;;; Autoloads and provides

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

(autoload 'sgml-load-dtd "psgml-parse"
	  "Load a saved DTD from FILE."
	  t nil)
(autoload 'sgml-show-or-clear-log "psgml-parse"
	  "Show the *SGML LOG* buffer if it is not showing, or clear and
remove it if it is showing."
	  t nil)
(autoload 'sgml-next-data-field "psgml-parse"
	  "Move forward to next point where data is allowed."
	  t nil)
(autoload 'sgml-next-trouble-spot "psgml-parse"
	  "Move forward to next point where something is amiss with the structure."
	  t nil)
(autoload 'sgml-list-valid-tags "psgml-parse"
	  "Display a list of the contextually valid tags."
	  t nil)
(autoload 'sgml-show-context "psgml-parse"
	  "Display where the cursor is in the element hierarchy."
	  t nil)
(autoload 'sgml-insert-end-tag "psgml-parse"
	  "Insert end tag for the current open element."
	  t nil)
(autoload 'sgml-insert-tag "psgml-parse"
	  "Insert a tag, reading tag name in minibuffer with completion.
If the variable sgml-balanced-tag-edit is t, also inserts the
corresponding end tag. If sgml-leave-point-after-insert is t, the point
is left after the inserted tag(s), unless the element has som required
content.  If sgml-leave-point-after-insert is nil the point is left
after the first tag inserted."
	  t nil)
(autoload 'sgml-tags-menu "psgml-parse"
	  "Pop up a menu with valid tags and insert the choosen tag.
If the variable sgml-balanced-tag-edit is t, also inserts the
corresponding end tag. If sgml-leave-point-after-insert is t, the point
is left after the inserted tag(s), unless the element has som required
content.  If sgml-leave-point-after-insert is nil the point is left
after the first tag inserted."
	  t nil)
(autoload 'sgml-insert-element "psgml-parse"
	  "Reads element name from minibuffer and inserts start and end tags."
	  t nil)
(autoload 'sgml-tag-region "psgml-parse"
	  "Reads element name from minibuffer and inserts start and end tags."
	  t nil)
(autoload 'sgml-edit-attributes "psgml-parse"
	  "Edit attributes of start tag before point.
Editing is done in a separate window."
	  t nil)
(autoload 'sgml-hide-attributes "psgml-parse"
	  nil
	  t nil)
(autoload 'sgml-show-attributes "psgml-parse"
	  nil
	  t nil)
(autoload 'sgml-options-menu "psgml-parse"
	  nil
	  t nil)
(autoload 'sgml-beginning-of-element "psgml-parse"
	  "Move to after the start tag of the current element.
If the start tag is implied, move to the start of the element."
	  t nil)
(autoload 'sgml-end-of-element "psgml-parse"
	  "Move to before the end tag of the current element."
	  t nil)
(autoload 'sgml-backward-up-element "psgml-parse"
	  "Move backward out of this element level.
That is move to before the start tag or where a start tag is implied."
	  t nil)
(autoload 'sgml-up-element "psgml-parse"
	  "Move forward out of this element level.
That is move to after the end tag or where an end tag is implied."
	  t nil)
(autoload 'sgml-forward-element "psgml-parse"
	  "Move forward over next element."
	  t nil)
(autoload 'sgml-backward-element "psgml-parse"
	  "Move backward over previous element at this level.
With implied tags this is ambigous."
	  t nil)
(autoload 'sgml-down-element "psgml-parse"
	  "Move forward and down one level in the element structure."
	  t nil)
(autoload 'sgml-kill-element "psgml-parse"
	  "Kill the element following the cursor."
	  t nil)
(autoload 'sgml-transpose-element "psgml-parse"
	  "Interchange element before point with element after point, leave point after."
	  t nil)
(autoload 'sgml-mark-element "psgml-parse"
	  "Set mark at end of current element, and leave point before current element."
	  t nil)
(autoload 'sgml-fold-region "psgml-parse"
	  "Hide (or if prefixarg unhide) region.
If called from a program first two arguments are start and end of
region. And optional third argument true unhides."
	  t nil)
(autoload 'sgml-fold-element "psgml-parse"
	  "Fold the lines comprising the current element, leaving the first line visible.
This uses the selective display feature."
	  t nil)
(autoload 'sgml-fold-subelement "psgml-parse"
	  "Fold all elements current elements content, leaving the first lines visible.
This uses the selective display feature."
	  t nil)
(autoload 'sgml-unfold-line "psgml-parse"
	  "Show hidden lines in current line."
	  t nil)
(autoload 'sgml-change-element-name "psgml-parse"
	  "Replace the name (generic identifyer) of the current element with a new name."
	  t nil)
(autoload 'sgml-unfold-element "psgml-parse"
	  "Show all hidden lines in current element."
	  t nil)
(autoload 'sgml-expand-element "psgml-parse"
	  "As sgml-fold-subelement, but unfold first."
	  t nil)
(autoload 'sgml-unfold-all "psgml-parse"
	  "Show all hidden lines in buffer."
	  t nil)
(autoload 'sgml-entities-menu "psgml-parse"
	  nil
	  t nil)

(autoload 'sgml-fill-element "psgml-parse"
	  nil
	  t nil)
(autoload 'sgml-normalize "psgml-parse"
	  "Normalize buffer by filling in omitted tags and expanding empty tags."
	  t nil)
(autoload 'sgml-what-element "psgml-parse"
	  nil
	  t nil)

(autoload 'sgml-complete "psgml-parse" nil t nil)

(provide 'psgml)
(provide 'sgml-mode)

;;; psgml.el ends here
