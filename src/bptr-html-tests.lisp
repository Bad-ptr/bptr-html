(in-package :cl-user)

(defpackage :bptr-html-tests
  (:use :cl :bptr-html :bptr-tests-from-docstrings)
  (:export #:retest-html-tests))

(in-package :bptr-html-tests)

(defun retest-html-tests ()
  (let ((html-tests (make-empty-testsdb)))
    (make-tests-for-package :bptr-html :testsdb html-tests)
    (format *error-output* "~s" (with-output-to-string (ostr)
                                  (run-testsdb :testsdb html-tests :ostream ostr)))))
