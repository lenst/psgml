
(let ((sgml-ecat-files '("tc22.ecat"))
      (dd default-directory))
  (save-current-buffer 
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (when (and sgml-buffer-parse-state
                 (sgml-pstate-dtd sgml-buffer-parse-state)
                 (sgml-dtd-merged (sgml-pstate-dtd sgml-buffer-parse-state)))
        (kill-buffer buf))))
  (ignore-errors (delete-file "/tmp/tc22.cdtd"))
  (switch-to-buffer (generate-new-buffer "tc22 temp"))
  (setq default-directory dd)
  (insert-file-contents "tc22.sgml")
  (sgml-mode)
  (sgml-load-doctype)
)
