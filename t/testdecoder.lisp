(in-package :json-test)

(def-suite json)
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
  (is (equalp '((hello . "hej")
                (hi . "tjena"))
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

(defparameter *json-test-files-path* *load-truename*)

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

