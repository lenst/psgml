(require 'psgml-parse)
(require 'psgml-edit)

;; psgml-parse.el

(defun sgml-parse-set-appflag (flagsym)
  (loop for name = (sgml-parse-name)
        while name
        for et = (sgml-lookup-eltype name)
        for flag-value = t
        do
        (when (looking-at "@")
          (forward-char 1)
          (let ((attr (sgml-check-name)))
            (sgml-check-delim "VI")
            (let ((val (sgml-parse-literal)))
              (setq flag-value (cons attr val)))))
        (setf (sgml-eltype-appdata et flagsym) flag-value)
        (message "Defining element %s as %s %S" name flagsym flag-value)
        (sgml-skip-cs)))

;; psgml-edit.el

(defun sgml-element-fillable (element)
  (and (sgml-element-mixed element)
       (let ((nofill (sgml-element-appdata element 'nofill)))
         (if (or (eq nofill t) (not (consp nofill)))
             t
           (let ((attr (car nofill))
                 (val  (cdr nofill)))
             (let ((attval (sgml-element-attval element attr)))
               (not (equal val attval))))))))
