(require 'psgml-parse)

(eval-when-compile
  (unless (member "JSP-STAGO" sgml-delimiters)
    (setq sgml-delimiters
          (list*
           "JSP-STAGO" "<%"
           "JSP-TAGC"  "%>"
           sgml-delimiters))))

(defun psgml-parse-jps-tag ()
  (when (sgml-parse-delim "JSP-STAGO")
    (search-forward (sgml-delim "JSP-TAGC") nil 'noerror)
    (sgml-set-markup-type 'comment)
    t))

(add-to-list 'sgml-parser-loop-hook 'psgml-parse-jps-tag)
(modify-syntax-entry ?: "_" sgml-parser-syntax)
