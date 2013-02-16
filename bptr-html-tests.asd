#|
  File: bptr-html-tests.asd
  Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
  Date: 2013/02/13 19:32:33
|#


(in-package :cl-user)

(defpackage bptr-html-tests-asd
  (:use :cl :asdf))

(in-package bptr-html-tests-asd)

(defsystem bptr-html-tests
  :depends-on (:bptr-tests-from-docstrings :bptr-html)
  :components ((:module "src"
                        :components
                        ((:file "bptr-html-tests")))))


(defmethod operation-done-p ((o test-op) (c (eql (find-system :bptr-html-tests))))
  nil)

(defmethod perform ((o test-op) (c (eql (find-system :bptr-html-tests))))
  (load-system :bptr-html-tests)
  (flet ((run-tests (&rest args)
           (apply (intern (string '#:retest-html-tests) '#:bptr-html-tests) args)))
    (run-tests)))
