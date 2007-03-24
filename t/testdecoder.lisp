(in-package :json-test)

(in-suite json)

;; Test decoder

(test json-literal
  (is-true (decode-json-from-string "  true"))
  (is-true (decode-json-from-string "  true "))
  (is-true (decode-json-from-string "true "))
  (is-true (decode-json-from-string "true"))
  (is-false (decode-json-from-string "trUe "))
  (is-false (decode-json-from-string "false"))
  (is-false (decode-json-from-string "null"))
  )

(test json-string
  (is (string= "hello"
              (decode-json-from-string "  \"hello\"")))
  (is (string= "new-line
returned!"
              (decode-json-from-string "\"new-line\\nreturned!\"")))
  (is (string= (make-string 1 :initial-element (code-char (+ (* 10 16) 11)))
              (decode-json-from-string "  \"\\u00ab\""))))

(test json-array
  (is (equalp
       '("hello" "hej" "ciao")
       (decode-json-from-string " [ \"hello\",  \"hej\",
                   \"ciao\" ]")))
  (is (equalp '(1 2 3)
              (decode-json-from-string "[1,2,3]")))
  (is (equalp '(t nil nil)
              (decode-json-from-string "[true,null,false]")))
  (is-false (decode-json-from-string "[]")))

(test json-object
  (is (equalp '((:hello . "hej")
                (:hi . "tjena"))
       (decode-json-from-string " { \"hello\" : \"hej\" ,
                       \"hi\" : \"tjena\"
                     }")))
  (is-false (decode-json-from-string " {  } "))
  (is-false (decode-json-from-string "{}")))

(test json-object-factory
  (let ((*json-object-factory* #'(lambda ()
                                   (make-hash-table)))
        (*json-object-factory-add-key-value* #'(lambda (obj key value)
                                                 (setf (gethash (intern (string-upcase key)) obj)
                                                       value)
                                                 obj))
        (*json-object-factory-return* #'identity)
        obj)
    (setf obj (decode-json-from-string " { \"hello\" : \"hej\" ,
                       \"hi\" : \"tjena\"
                     }"))
    (is (string= "hej" (gethash 'hello obj)))
    (is (string= "tjena" (gethash 'hi obj)))))

(test json-object-camel-case
  (is (equalp '((:hello-key . "hej")
                (:*hi-starts-with-upper-case . "tjena"))
       (decode-json-from-string " { \"helloKey\" : \"hej\" ,
                       \"HiStartsWithUpperCase\" : \"tjena\"
                     }"))))




(test json-number
  (is (= (decode-json-from-string "100") 100))
  (is (= (decode-json-from-string "10.01") 10.01))
  (is (= (decode-json-from-string "-2.3") -2.3))
  (is (= (decode-json-from-string "-2.3e3") -2.3e3))          
  (is (= (decode-json-from-string "-3e4") -3e4))
  (is (= (decode-json-from-string "3e4") 3e4))  
  #+sbcl
  (is (= (decode-json-from-string "2e40") 2d40));;Coerced to double
  (is (equalp (decode-json-from-string "2e444") (funcall *json-make-big-number* "2e444"))))

(defparameter *json-test-files-path* *load-pathname*)

(defun test-file (name)
  (make-pathname :name name :type "json" :defaults *json-test-files-path*))

(defun decode-file (path)
  (with-open-file (stream path
                          :direction :input)
    (decode-json-strict stream)))

;; All test files are taken from http://www.crockford.com/JSON/JSON_checker/test/

(test pass-1
  (decode-file (test-file "pass1")))

(test pass-2
  (decode-file (test-file "pass2")))

(test pass-3
  (decode-file (test-file "pass3")))

(defparameter *ignore-tests* '(
  1 ; says: "A JSON payload should be an object or array, not a string.", but who cares?
  7 ; says: ["Comma after the close"],  ,but decode-file stops parsing after one object has been retrieved
  8 ; says ["Extra close"]] ,but decode-file stops parsing after one object has been retrieved
  10; says {"Extra value after close": true} "misplaced quoted value", but
    ;   decode-file stops parsing after one object has been retrieved
  18; says [[[[[[[[[[[[[[[[[[[["Too deep"]]]]]]]]]]]]]]]]]]]], but there is no formal limit
))

(defparameter *ignore-tests-strict* '(
  18; says [[[[[[[[[[[[[[[[[[[["Too deep"]]]]]]]]]]]]]]]]]]]], but there is no formal limit
))

(test fail-files
  (dotimes (x 24)
    (if (member x *ignore-tests-strict*)
        (is-true t) 
        (5am:signals error 
          (decode-file (test-file (format nil "fail~a" x)))))))

(defun contents-of-file(file)
  (with-open-file (stream file :direction :input)
     (let ((s (make-string (file-length stream))))
      (read-sequence s stream)
      s)))

(test decoder-performance  
  (let* ((json-string (contents-of-file (test-file "pass1")))
         (chars (length json-string))
         (count 1000))
    (format t "Decoding ~a varying chars from memory ~a times." chars count)
    (time
     (dotimes (x count) 
       (let ((discard-soon (decode-json-from-string json-string)))
         (funcall #'identity discard-soon))))));Do something so the compiler don't optimize too much

;;#+when-u-want-profiling
;;(defun profile-decoder-performance()
;;  #+sbcl
;;  (progn
;;    (let ((json-string (contents-of-file (test-file "pass1")))
;;          (count 10))
;;      (format t "Parsing test-file pass1 from memory ~a times." count)
;;      (sb-sprof:with-profiling ()
;;        (dotimes (x count) 
;;          (let ((discard-soon (decode-json-from-string json-string)))
;;            (funcall #'identity discard-soon))))
;;      (sb-sprof:report)
;;      nil)))

(test non-strict-json
   (let ((not-strictly-valid "\"right\\'s of man\""))
     (5am:signals json:json-parse-error
       (json:decode-json-from-string not-strictly-valid))
     (let ((*use-strict-json-rules* nil))
       (declare (special *use-strict-json-rules*))
       (is (string= (json:decode-json-from-string not-strictly-valid)
                    "right's of man")))))

(test test*json-symbols-package*
  (let ((*json-symbols-package* nil)
        x)
    (setf x (decode-json-from-string "{\"x\":1}"))
    (is (equal (symbol-package (caar x))
               (find-package :json-test))))
  (let ((*json-symbols-package* (find-package :cl-user))
        x)
    (setf x (decode-json-from-string "{\"x\":1}"))
    (is (equal (symbol-package (caar x))
               (find-package :cl-user))))
  (let (x)
    (setf x (decode-json-from-string "{\"x\":1}"))
    (is (equal (symbol-package (caar x))
               (find-package :keyword)))))

