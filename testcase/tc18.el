(with-temp-buffer
  (insert-file-contents "tc18.html")
  (goto-char (point-min))
  (search-forward "em class")
  (sgml-change-element-name "B")
  (beginning-of-line 1)
  (assert (looking-at ".* class="))
  )
