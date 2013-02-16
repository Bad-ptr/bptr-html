#|
Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
|#


(in-package :cl-user)

(defpackage bptr-html-asd
  (:use :cl :asdf))

(in-package :bptr-html-asd)

(defsystem bptr-html
  :version "0.1"
  :author "Constantin Kulikov"
  :license "GPL v2 or any higher."
  :description "My html generator."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :depends-on (:bptr-plist)
  :components ((:module "src"
                        :components
                        ((:file "bptr-html")))))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :bptr-html))))
  nil)

(defmethod perform ((o test-op) (c (eql (find-system :bptr-html))))
  (operate 'load-op :bptr-html-tests)
  (operate 'test-op :bptr-html-tests))
