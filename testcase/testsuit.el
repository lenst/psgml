;;; testsuit.el -- Test Suit for PSGML

;; $Id$

(require 'cl)
(require 'psgml)
(require 'psgml-parse)

(defconst psgml-test-cases
  '(("tc01.sgml" (warning "Undefined entity.*"))
    ("tc02.xml")
    ("tc03.xml")
    ("tc04.sgml")
    ("tc05.sgml")
    ("tc07.sgml")
    ("tc08.xml")
    ("tc13.el")))

(defun testsuit-run-test-case (case-description)
  (let* ((file (first case-description))
         (expected (rest case-description))
         (sgml-show-warnings t))
    (sgml-reset-log)
    (setq sgml-catalog-assoc nil)       ; To allow testing catalog parsing
    (setq sgml-ecat-assoc nil)
    (message "Testing %s" file)
    (find-file file)
    (condition-case errcode
        (progn
          (if (string-match "\\.el$" (buffer-file-name))
              (progn (eval-buffer))
            (sgml-parse-prolog)
            ;;(sgml-next-trouble-spot)
            (sgml-parse-until-end-of nil)))
      (error
       (if expected
           (case (caar expected)
             (error (debug)))
         (error "Unexptectd %s" errcode))))
    (while (and expected (eq (caar expected) 'warning))
      (let ((warning-pattern (cadar expected)))
        (set-buffer sgml-log-buffer-name)
        (goto-char (point-min))
        (cond ((re-search-forward warning-pattern nil t)
               (setq expected (cdr expected)))
              (t
               (error "No %s warning" warning-pattern)))))
    (when expected
      (error "The expected result %s didn't" expected))))

(defun testsuit-run ()
  (interactive)
  (loop for tc in psgml-test-cases
        do (testsuit-run-test-case tc))
  (message "Done"))
