
(require 'psgml-api)

(defun sgml-reformat ()
  (interactive)
  (goto-char (sgml-element-start (sgml-top-element)))
  (unless (bolp)
    (insert "\n"))
  (sgml-map-element-modify
   #'sgml--reformat-element
   (sgml-top-element)))

(defun sgml--reformat-element (el)
  (unless (sgml-element-data-p (sgml-element-parent el))
    (goto-char (sgml-element-end el))
    (unless (eolp)
      (insert "\n")
      (sgml-indent-line)))
  (unless (sgml-element-data-p el)
    (goto-char (sgml-element-stag-end el))
    (unless (eolp)
      (insert "\n")
      (sgml-indent-line)))
  (goto-char (sgml-element-stag-end el))
  (backward-char 1)
  (if (bolp) (delete-char -1)))

