(defun psgml-tc13 ()
  (set-buffer (get-buffer-create "*Tc13*"))
  (erase-buffer)
  (xml-mode)
  (insert "<foo/")
  (ignore-errors
    (let ((el (sgml-parse-to-here)))
      (message "el etag-epos=%s etag-len=%s"
               (sgml-tree-etag-epos el)
               (sgml-tree-etag-len el))))
  (insert ">")
  (let ((el (sgml-parse-to-here)))
    (assert (sgml-off-top-p el))))

(psgml-tc13)
