(in-package :json-test)
(in-suite json)

(defmacro with-objects-as-hashtables(&body body)
  ;;For testing, keys are stored as strings
  `(let ((*json-object-factory* #'(lambda ()
                                   (make-hash-table :test #'equalp )))
        (*json-object-factory-add-key-value* #'(lambda (obj key value)
                                                 (setf (gethash key obj)
                                                       value)
                                                 obj))
        (*json-object-factory-return* #'identity))
    ,@body))
    
(test json-string()
   (is (string= (encode-json-to-string (format nil "hello~&hello"))
                 "\"hello\\nhello\""))
   (is (string= (encode-json-to-string (format nil "\"aquote"))
                 "\"\\\"aquote\"")))

(test json-literals
  (is (string= "true" (encode-json-to-string t)))
  (is (string= "null" (encode-json-to-string nil))))

(defun is-same-number(nr)
  "If it gets decoded back ok then it was encoded ok"
  (is (= nr (decode-json-from-string (encode-json-to-string nr)))))

(test json-number
  (is (string= "0" (encode-json-to-string 0)))
  (is (string= "13" (encode-json-to-string 13)))
  (is (string= "13.02" (encode-json-to-string 13.02)))

  (is-same-number 2e10)
  (is-same-number  -1.3234e-10)
  (is-same-number -1280.12356)
  (is-same-number 1d2)
  (is-same-number 1l2)
  (is-same-number 1s2)
  (is-same-number 1f2)
  (is-same-number 1e2))

(defun decode-then-encode (json)
  (with-objects-as-hashtables
    (assert (member (elt json 0) '(#\{ #\[ #\" ))) ;must be json
    (flet ((normalize (string)
             (remove #\Newline (remove #\Space string))))
      (let* ((decoded (decode-json-from-string json))
             (encoded (encode-json-to-string decoded)))
;;        (format t "Json:~a~&" json)
;;        (format t "Encoded:~a" encoded)    
        (is (string= (normalize json)
                     (normalize encoded)))))))

(test test-encode-json-nathan-hawkins
  (let ((foo '((a . 1) (b . 2) (c . 3))))
    (is (string= (encode-json-to-string foo)
                 "{\"a\":1,\"b\":2,\"c\":3}"))))

(test test-encode-json-alist
      (let ((alist `((:HELLO . 100)(:hi . 5)))
            (expected "{\"hello\":100,\"hi\":5}"))
        (is (string= (with-output-to-string (s) (encode-json-alist alist s))
                     expected))))

(test test-encode-json-alist-two
      (let ((alist `((HELLO . 100)(hi . 5)))
            (expected "{\"hello\":100,\"hi\":5}"))
        (is (string= (with-output-to-string (s) (encode-json-alist alist s))
                     expected))))

(test test-encode-json-alist-string
      (let ((alist `((:hello . "hej")(:hi . "tjena")))
            (expected "{\"hello\":\"hej\",\"hi\":\"tjena\"}"))
        (is (string= (with-output-to-string (s) (encode-json-alist alist s))
                     expected))))

(test test-encode-json-alist-camel-case
      (let ((alist `((:hello-message . "hej")(*also-starting-with-upper . "hej")))
            (expected "{\"helloMessage\":\"hej\",\"AlsoStartingWithUpper\":\"hej\"}"))
        (is (string= (with-output-to-string (s) (encode-json-alist alist s))
                     expected))))

(test encode-pass-2
  (decode-then-encode "[[[[[[[[[[[[[[[[[[[\"Not too deep\"]]]]]]]]]]]]]]]]]]]"))

(test encode-pass-3
  (decode-then-encode "{
    \"JSON Test Pattern pass3\": {
        \"The outermost value\": \"must be an object or array.\"
    }
}
"))

;; Test inspired by the file pass1. 
;; There are too many small differences just to decode-encode the whole pass1 file,
;; Instead the difficult parts are in separate tests below.

(test controls
   (decode-then-encode "\"\\\\b\\\\f\\\\n\\\\r\\\\\""))

(test slash
  (let* ((z "\"/ & /\"")
         (also-z "\"/ & \/\"") ;Extra quote
         (x (encode-json-to-string z))
         (also-x (encode-json-to-string also-z))
         (y (decode-json-from-string x))
         (also-y (decode-json-from-string also-x)))
    (is (string= x also-x))
    (is (string= y also-y))
    (is (string= z y))))


(test quoted
  (decode-then-encode "\"&#34; %22 0x22 034 &#x22;\""))

(test alpha-1
  (decode-then-encode "\"abcdefghijklmnopqrstuvwyz\""))

(test alpha-2
  (decode-then-encode "\"ABCDEFGHIJKLMNOPQRSTUVWYZ\""))

(test digit
  (decode-then-encode "\"0123456789\""))

(test special
  (decode-then-encode "\"`1~!@#$%^&*()_+-={':[,]}|;.</>?\""))

(test hex
  (decode-then-encode "\"\u0123\u4567\u89AB\uCDEF\uabcd\uef4A\""))

(test true
  (decode-then-encode "[ true]"))

(test false
  (is (string= (encode-json-to-string (decode-json-from-string "[false]"))
               "[null]")));;We dont separate between false and null
(test null
  (decode-then-encode "[null]"))

(test array
  ;;Since empty lists becomes nil in lisp, they are converted back to null
  (is (string= (encode-json-to-string (decode-json-from-string "[  ]"))
               "null"))
  ;;But you can use vectors
  (is (string= (encode-json-to-string (vector 1 2))
               "[1,2]")))

(test character
  ;;Characters are encoded to strings, but when decoded back to string
  (is (string= (encode-json-to-string #\a) "\"a\"")))


(test hash-table-symbol
  (let ((ht (make-hash-table)))
    (setf (gethash 'symbols-are-now-converted-to-camel-case ht) 5)
    (is (string= (encode-json-to-string ht)
                 "{\"symbolsAreNowConvertedToCamelCase\":5}"))))

(test hash-table-string
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "lower x" ht) 5)
    (is (string= (encode-json-to-string ht)
                 "{\"lower x\":5}"))))


(defparameter *encode-performace-test-string*
  "{
    \"JSON Test Pattern pass3\": {
        \"The outermost value\": \"must be an object or array.\",
        \"In this test\": \"It is an object.\",
        \"Performance-1\" : 123465.578,
        \"Performance-2\" : 12e4,
        \"Performance-2\" : \"asdasdsadsasdasdsdasdasdasdsaaaaaaaaaaaaasdasdasdasdasdsd\",
        \"Performance-3\" : [\"asdasdsadsasdasdsdasdasdasdsaaaaaaaaaaaaasdasdasdasdasdsd\",
                         \"asdasdsadsasdasdsdasdasdasdsaaaaaaaaaaaaasdasdasdasdasdsd\",
                         \"asdasdsadsasdasdsdasdasdasdsaaaaaaaaaaaaasdasdasdasdasdsd\",
                         \"asdasdsadsasdasdsdasdasdasdsaaaaaaaaaaaaasdasdasdasdasdsd\"]
    }
}
")





(test encoder-performance
  (with-objects-as-hashtables      
    (let* ((json-string *encode-performace-test-string*)
           (chars (length json-string))
           (lisp-obj (decode-json-from-string json-string))
           (count 2000))
      (format t "Encoding ~a varying chars  from memory ~a times." chars count)
      (time
       (dotimes (x count) 
         (let ((discard-soon (encode-json-to-string lisp-obj)))
           (funcall #'identity discard-soon)))))))



