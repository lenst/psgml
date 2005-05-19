(require 'custom)

(defgroup psgml ()
  "PSGML customizations."
  :prefix "sgml-")

(defcustom sgml-insert-missing-element-comment t
  "*If true, and sgml-auto-insert-required-elements also true,
`sgml-insert-element' will insert a comment if there is an element required
but there is more than one to choose from."
  :type 'boolean
  :group 'psgml)

(defcustom sgml-insert-end-tag-on-new-line nil
  "*If true, `sgml-insert-element' will put the end-tag on a new line
after the start-tag. Useful on slow terminals if you find the end-tag after
the cursor irritating."
  :type 'boolean
  :group 'psgml)

(defcustom sgml-doctype nil
  "*If set, this should be the name of a file that contains the doctype
declaration to use.
Setting this variable automatically makes it local to the current buffer."
  :tag "Doctype file"
  :type 'file
  :group 'psgml)

(make-variable-buffer-local 'sgml-doctype)

(defcustom sgml-system-identifiers-are-preferred nil
  "*If nil, PSGML will look up external entities by searching the catalogs
in `sgml-local-catalogs' and `sgml-catalog-files' and only if the
entity is not found in the catalogs will a given system identifer be
used. If the variable is non-nil and a system identifer is given, the
system identifier will be used for the entity. If no system identifier
is given the catalogs will searched."
  :type 'boolean
  :group 'psgml)

(defcustom sgml-range-indicator-max-length 9
  "*Maximum number of characters used from the first and last entry
of a submenu to indicate the range of that menu."
  :type 'integer
  :group 'psgml)

(defvar sgml-default-doctype-name nil
  "*Document type name to use if no document type declaration is present.")
(put 'sgml-default-doctype-name 'sgml-type 'string-or-nil)

(defvar sgml-markup-faces '((start-tag 	. bold)
			    (end-tag 	. bold)
			    (comment 	. italic)
			    (pi 	. bold)
			    (sgml 	. bold)
			    (doctype 	. bold)
			    (entity 	. bold-italic)
			    (shortref   . bold))
  "*List of markup to face mappings.
Element are of the form (MARKUP-TYPE . FACE).
Possible values for MARKUP-TYPE is:
comment	- comment declaration
doctype	- doctype declaration
end-tag 
ignored	- ignored marked section
ms-end	- marked section start, if not ignored 
ms-start- marked section end, if not ignored
pi	- processing instruction
sgml	- SGML declaration
start-tag
entity  - general entity reference
shortref- short reference")

(defvar sgml-buggy-subst-char-in-region 
  (or (not (boundp 'emacs-minor-version))
      (not (natnump emacs-minor-version))
      (and (eq emacs-major-version 19)
           (< emacs-minor-version 23)))
  "*If non-nil, work around a bug in subst-char-in-region.
The bug sets the buffer modified.  If this is set, folding commands
will be slower.")

(defvar sgml-set-face nil
  "*If non-nil, psgml will set the face of parsed markup.")
(put 'sgml-set-face 'sgml-desc "Set face of parsed markup")

(defvar sgml-auto-activate-dtd nil
  "*If non-nil, loading a sgml-file will automatically try to activate its DTD.
Activation means either to parse the document type declaration or to
load a previously saved parsed DTD.  The name of the activated DTD
will be shown in the mode line.")
(put 'sgml-auto-activate-dtd 'sgml-desc "Auto Activate DTD")

(defvar sgml-offer-save t
  "*If non-nil, ask about saving modified buffers before \\[sgml-validate] is run.")

(defvar sgml-parent-document nil
  "*Used when the current file is part of a bigger document.

The variable describes how the current file's content fit into the element
hierarchy. The variable should have the form

  (PARENT-FILE CONTEXT-ELEMENT* TOP-ELEMENT (HAS-SEEN-ELEMENT*)?)

PARENT-FILE	is a string, the name of the file contatining the
		document entity.
CONTEXT-ELEMENT is a string, that is the name of an element type.
		It can occur 0 or more times and is used to set up
		exceptions and short reference map. Good candidates
		for these elements are the elements open when the
		entity pointing to the current file is used. 
TOP-ELEMENT	is a string that is the name of the element type
		of the top level element in the current file. The file
		should contain one instance of this element, unless
		the last \(lisp) element of sgml-parent-document is a
		list. If it is a list, the top level of the file
		should follow the content model of top-element. 
HAS-SEEN-ELEMENT is a string that is the name of an element type. This
	        element is satisfied in the content model of top-element.

Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'sgml-parent-document)
(put 'sgml-parent-document 'sgml-type 'list)

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
(put 'sgml-omittag 'sgml-desc "OMITTAG")

(defvar sgml-shorttag t
  "*Set to non-nil, if you use SHORTTAG YES.

Setting this variable automatically makes it local to the current buffer.")

(make-variable-buffer-local 'sgml-shorttag)
(put 'sgml-shorttag 'sgml-desc "SHORTTAG")

(defvar sgml-namecase-general t
  "*Set to non-nil, if you use NAMECASE GENERAL YES.

Setting this variable automatically makes it local to the current buffer.")

(make-variable-buffer-local 'sgml-namecase-general)
(put 'sgml-namecase-general 'sgml-desc "NAMECASE GENERAL")
