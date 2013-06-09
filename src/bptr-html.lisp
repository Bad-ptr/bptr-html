(in-package :cl-user)

(defpackage bptr-html
  (:nicknames :bphtml)
  (:documentation "Html generation functions.")
  (:use :common-lisp)
  (:export #:html-tag
           #:html
           #:deftag
           #:deftags
           #:with-html-output
           #:with-html-vars))

(in-package :bptr-html)


(defmacro defparameters (&rest params)
  (cons 'progn (loop :for (par val) :in params
                  :collect `(defparameter ,par ,val))))

(defparameters
    (*html-output-stream* *standard-output*)
    (*html-indentation-string* "  ")
  (*html-indent-cache* nil)
  (*html-indentation-level* 0)
  (*html-human-readable* t))

(defvar *html-defined-tags* (make-hash-table :test 'equal)

  "Defined html tags.")

(defmacro with-html-output (outstream &body body)
  `(let ((*html-output-stream* ,outstream))
     ,@body))

(defmacro with-html-vars ((&key
                           (outstream *html-output-stream*)
                           (indent-str *html-indentation-string*)
                           (indent-lvl *html-indentation-level*)
                           (hreadable *html-human-readable*))
                          &body body)
  `(let ((*html-output-stream* ,outstream)
         (*html-indentation-string* ,indent-str)
         (*html-indentation-level* ,indent-lvl)
         (*html-human-readable* ,hreadable))
     ,@body))

(defun indent (&key (td 0))
  (when *html-human-readable*
    (unless *html-indent-cache*
      (setq *html-indent-cache*
            (with-output-to-string (str)
              (loop :for i :from (+ *html-indentation-level* td) :above 0
                 :do (write-string *html-indentation-string* str)))))
    (write-string *html-indent-cache* *html-output-stream*)))

(defun indent+ ()
  (setq *html-indentation-level* (1+ *html-indentation-level*))
  (when *html-indent-cache*
    (setq *html-indent-cache*
          (concatenate 'string *html-indent-cache* *html-indentation-string*))))

(defun indent- ()
  (when (> *html-indentation-level* 0)
    (setq *html-indentation-level* (1- *html-indentation-level*)))
  (when *html-indent-cache*
    (setq *html-indent-cache*
          (subseq *html-indent-cache* 0
                  (* (length *html-indentation-string*) *html-indentation-level*)))))

(defun prepend-indent (pstr &key (td 0))
  (indent :td td)
  (write-string pstr *html-output-stream*))


(defun newline ()

  "Write newline character"

  (when *html-human-readable*
    (write-char #\Newline *html-output-stream*)))

(defun escape-char (char)
  
  (case char
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\' "&apos;")
    (#\" "&quot;")
    (t (format nil "&#~d;" (char-code char)))))

(defmacro add-escape (name estr)
  (let* ((uname (string-upcase name))
         (param-name (intern (concatenate 'string "*" uname "-ESCAPES*")))
         (f-needesc (intern (concatenate 'string uname "-NEED-ESCAPE")))
         (f-escstr (intern (concatenate 'string uname "-ESCAPE-STR")))
         (f-tostr (intern (concatenate 'string uname "-TO-STR"))))
    `(progn
       (defparameter ,param-name ,estr)
       (defun ,f-needesc (ch)
         (find ch ,param-name))
       (defun ,f-escstr (str)
         (loop :for ch :across str
            :if (,f-needesc ch)
            :do (write-string (escape-char ch) *html-output-stream*)
            :else :do (write-char ch *html-output-stream*)))
       (defun ,f-tostr (elm)
         (typecase elm
           (string (,f-escstr elm))
           ((or symbol keyword) (,f-escstr (string-downcase (symbol-name elm))))
           (t (format *html-output-stream* "~S" elm)))))))

(defmacro add-escapes (&rest args)
  (cons 'progn (loop :for (name str) :in args
                  :collect `(add-escape ,name ,str))))

(add-escapes
 ("body" "<>&")
 ("attrib" "<>&\"'"))



(defun interpreting-write (obj)
  (when obj
    (typecase obj
      (string (write-string obj *html-output-stream*))
      ((or symbol keyword) (write-string (string-downcase (string obj)) *html-output-stream*))
      ((or number integer float real)
       (write-string (write-to-string obj) *html-output-stream*))
      (cons (let ((h (first obj)))
              (cond
                ((consp h) (loop :for el :in obj
                              :do (interpreting-write el)))
                ((and (symbolp h) (fboundp h)) (interpreting-write (apply h (rest obj))))
                ((keywordp h) (case h
                                (:eval (eval (rest obj)))
                                (:escape (body-to-str (interpret-to-string (rest obj))))
                                (:every (loop :for el :in (rest obj)
                                           :do (interpreting-write el)))
                                (:escape-every (loop :for el :in (rest obj)
                                                  :do (body-to-str (interpret-to-string el))))
                                (:newline (newline)
                                          (loop :for el :in (rest obj)
                                             :do (interpreting-write el)))
                                (:indent (indent)
                                         (loop :for el :in (rest obj)
                                            :do (interpreting-write el)))
                                (:indent+ (indent+))
                                (:indent- (indent-))
                                (t (html-tag obj))))
                (t (html-tag obj)))))
      (t (format *html-output-stream* "~S" obj)))))

(defun interpret-to-string (obj)
  (with-output-to-string (str)
    (with-html-output str
      (interpreting-write obj))))


(defun parse-tag-name (obj)
  (let* ((sname (interpret-to-string obj))
         (csid (search "." sname))
         (isid (search "#" sname))
         class id)
    (when isid
      (setq id (subseq sname (1+ isid)))
      (setq sname (subseq sname 0 isid)))
    (when csid
      (setq class (subseq sname (1+ csid)))
      (setq sname (subseq sname 0 csid)))
    (values sname class id)))


(defun deftag (name &rest props)
  (setf (gethash (interpret-to-string name) *html-defined-tags*) props))
(defun deftags (&rest tagdefs-list)
  (loop :for td :in tagdefs-list
     :do (apply #'deftag td)))
(defun getdtag (name)
  (gethash (interpret-to-string name) *html-defined-tags* nil))

(defun html-tag (props)

  "Generate html from list structure.
Example:
 (with-output-to-string (str)
           (bptr-html:WITH-HTML-OUTPUT str
             (bptr-html:html
              (name :asd \"asd\" :dsd \"asdds\" :editable :amputable :pre-tag (pretag \"pretag\")
                    :post-tag \"posttag\" :pre-body (prebd.cls#id \"Hi!\") :post-body post-body
                    :body (tagname.cls#aid :asd \"adsd\"
                                           :body (:mpp :class \"cls\"
                                                      \"eshse body\"
                                                      (br :xclose)
                                                      (br :xclose nil)
                                                      (:every :asd dfd \"asdasd\" (:newline (:indent)) (tag.clcl \"body\"))
                                                      (:escape-every \"<html>test</html>\")(:newline)
                                                      (:escape (br :xclose))
                                                      (\"tete\" :body \"ahaha\")))))))
==>
\"<pretag>
  pretag
</pretag>

<name asd=\\\"asd\\\" dsd=\\\"asdds\\\" editable amputable>
  <prebd class=\\\"cls\\\" id=\\\"id\\\">
    Hi!
  </prebd>

  <tagname class=\\\"cls\\\" id=\\\"aid\\\" asd=\\\"adsd\\\">
    <mpp class=\\\"cls\\\">
      eshse body
      <br/>
      <br>
      </br>
      asddfdasdasd
      <tag class=\\\"clcl\\\">
        body
      </tag>
      &lt;html&gt;test&lt;/html&gt;      
      &lt;br/&gt;
      <tete>
        ahaha
      </tete>
    </mpp>
  </tagname>
  post-body
</name>
posttag
\"
"

  (let ((props (bptr-plist:full-copy props)))
    (multiple-value-bind (name class id) (parse-tag-name (first props))

      (let ((props (bptr-plist:append-or-set-keys-values
                    (getdtag name) (rest props))))

        (multiple-value-bind (props xclose) (bptr-plist:remf-key props :xclose)

          (multiple-value-bind (props vlskeys) (bptr-plist:remf-empty-keys props)
            
            (multiple-value-bind (props nprops)
                (bptr-plist:remfa-keys props
                                       :pre-tag :post-tag :id :class
                                       :pre-body :post-body :body)

              (destructuring-bind  (&key pre-tag post-tag ((:id nid)) ((:class nclass))
                                         pre-body post-body body) nprops

                (when nid (setq id nid))
                (when nclass (setq class nclass))

                (multiple-value-bind (props bs) (bptr-plist:remf-keyless props)
                                        ;
                  (tag-pre pre-tag)
                  (tag-open name class id xclose props vlskeys)
                  (tag-body-pre pre-body)
                  (tag-body body bs)
                  (tag-body-post post-body)
                  (tag-ending name xclose props vlskeys)
                  (tag-post post-tag))))))))))

(defmacro html (props)
  `(html-tag ',props))

(defun tag-pre (pre)
  (when pre
    (indent)
    (interpreting-write pre)
    (newline)))

(defun tag-open (name class id xclose props vlskeys)
  ;(indent)
  (indent+)
  (write-char #\< *html-output-stream*)
  (attrib-to-str name)
  (when class
    (write-string " class=\"" *html-output-stream*)
    (attrib-to-str class)
    (write-char #\" *html-output-stream*))
  (when id
    (write-string " id=\"" *html-output-stream*)
    (attrib-to-str id)
    (write-char #\" *html-output-stream*))
  (bptr-plist:for-kv props (prop val)
    :do (write-char #\Space *html-output-stream*)
    (attrib-to-str prop)
    (write-string "=\"" *html-output-stream*)
    (attrib-to-str val)
    (write-string "\"" *html-output-stream*))
  (loop for el in vlskeys
     :do (write-string " " *html-output-stream*)
       (attrib-to-str el))
  (unless (bptr-plist:get-key-value xclose :xclose)
    (write-char #\> *html-output-stream*)
    (newline)))

(defun tag-body-pre (pre-body)
  (when pre-body
    (indent)
    (interpreting-write pre-body)
    (newline)))

(defun tag-body (body &optional bs)
  (when body
    (typecase body
      (cons (indent)
            (interpreting-write body))
      (t (indent)
         (interpreting-write body)
         (newline))))
  (when bs
    (loop :for el :in bs
       :do (tag-body el))))

(defun tag-body-post (post-body)
  (when post-body
    (indent)
    (interpreting-write post-body)
    (newline)))

(defun tag-ending (name xclose vlskeys props)
  (declare (ignore props) (ignore vlskeys))
  (indent-)
  (if (bptr-plist:get-key-value xclose :xclose)
      (write-string "/>" *html-output-stream*)
      (progn
        (indent)
        (write-string "</" *html-output-stream*)
        (attrib-to-str name)
        (write-char #\> *html-output-stream*)))
  (newline))

(defun tag-post (post)
  (when post
    (indent)
    (interpreting-write post)
    (newline)))
