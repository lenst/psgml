(defun psgml-tc15 ()
  ;; Check that completing honors insert-case
  (set-buffer (get-buffer-create "*Tc15*"))
  (erase-buffer)
  (sgml-mode)
  (setq sgml-general-insert-case 'lower)
  (insert "<!DOCTYPE foo [<!ELEMENT foo (#PCDATA)>]>\n<f")
  (sgml-complete)
  (beginning-of-line)
  (let ((case-fold-search nil))
    (assert (looking-at "<foo"))))

(psgml-tc15)
