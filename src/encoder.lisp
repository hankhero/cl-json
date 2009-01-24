;;;; Copyright (c) 2006-2008 Henrik Hjelte
;;;; Copyright (c) 2008 Hans Hübner (marked parts)
;;;; All rights reserved.
;;;; See the file LICENSE for terms of use and distribution.

(in-package :json)

(defgeneric encode-json (object &optional stream)
  (:documentation "Write a JSON representation of OBJECT to STREAM."))

(defun encode-json-to-string (object)
  "Return the JSON representation of OBJECT as a string."
  (with-output-to-string (stream)
    (encode-json object stream)))

(defmethod encode-json (anything &optional (stream *standard-output*))
  "Encode an OBJECT which is not treated otherwise by encoding the
string which is its printed representation."
  (write-json-string (format nil "~A" anything) stream))

(defmethod encode-json ((nr number) &optional (stream *standard-output*))
  "Write the JSON representation of the number NR to STREAM (or to
*STANDARD-OUTPUT*)."
  (write-json-number nr stream))

(defmethod encode-json ((s string) &optional (stream *standard-output*)) 
  "Write the JSON representation of the string S to STREAM (or to
*STANDARD-OUTPUT*)."
  (write-json-string s stream))

(defmethod encode-json ((c character) &optional (stream *standard-output*))
  "JSON does not define a character type, we encode characters as strings."
  (encode-json (string c) stream))

(defmethod encode-json ((s symbol) &optional (stream *standard-output*))
  "Write the JSON representation of the symbol S to STREAM (or to
*STANDARD-OUTPUT*).  If S is boolean, a boolean literal is written.
Otherwise, the name of S is passed to *LISP-IDENTIFIER-NAME-TO-JSON*
and the result is written as string."
  (let ((mapped (car (rassoc s +json-lisp-symbol-tokens+))))
    (if mapped
        (write-string mapped stream)
        (let ((s (funcall *lisp-identifier-name-to-json* (symbol-name s))))
          (write-json-string s stream)))))


;;; The code below is from Hans Hübner's YASON (with modifications).

(defvar *json-aggregate-first* t
  "T when the first element of a JSON object or array is encoded,
afterward NIL.")

(defun next-aggregate-element (stream)
  "Between two elements of an object or array, print a comma
separator."
  (prog1 *json-aggregate-first*
    (unless *json-aggregate-first*
      (write-char #\, stream))
    (setq *json-aggregate-first* nil)))

(defmacro with-aggregate ((begin-char end-char
                           &optional (stream '*standard-output*))
                          &body body)
  "Run BODY to encode a JSON aggregate type, delimited by BEGIN-CHAR
and END-CHAR."
  `(let ((*json-aggregate-first* t))
     (declare (special *json-aggregate-first*))
     (write-char ,begin-char ,stream)
     (prog1 (progn ,@body)
       (write-char ,end-char ,stream))))

(defmacro with-array ((&optional (stream '*standard-output*)) &body body)
  "Open a JSON array, run BODY, then close the array.  Inside the body,
ENCODE-ARRAY-ELEMENT must be called to encode elements to the opened
array."
  `(with-aggregate (#\[ #\] ,stream) ,@body))

(defmacro with-object ((&optional (stream '*standard-output*)) &body body)
  "Open a JSON object, run BODY, then close the object.  Inside the body,
ENCODE-OBJECT-ELEMENT or WITH-OBJECT-ELEMENT must be called to encode
elements to the object."
  `(with-aggregate (#\{ #\} ,stream) ,@body))

(defun encode-array-element (object &optional (stream *standard-output*))
  "Encode OBJECT as next array element to the last JSON array opened
with WITH-ARRAY in the dynamic context.  OBJECT is encoded using the
ENCODE-JSON generic function, so it must be of a type for which an
ENCODE-JSON method is defined."
  (next-aggregate-element stream)
  (encode-json object stream))

(defun stream-array-element-encoder (stream)
  "Return a function which takes an argument and encodes it to STREAM
as an array element."
  (lambda (element)
    (encode-array-element element stream)))

(defmacro with-object-element ((key &optional (stream '*standard-output*))
                               &body body)
  "Open a new encoding context to encode a JSON object element.  KEY
is the key of the element.  The value will be whatever BODY serializes
to the current JSON output context using one of the stream encoding
functions.  This can be used to stream out nested object structures."
  `(progn
     (next-aggregate-element ,stream)
     (let ((key (encode-json-to-string ,key)))
       (if (char= (aref key 0) #\")
           (write-string key ,stream)
           (encode-json key ,stream)))
     (write-char #\: ,stream)
     ,@body))

(defun encode-object-element (key value
                              &optional (stream *standard-output*))
  "Encode KEY and VALUE as object element to the last JSON object
opened with WITH-OBJECT in the dynamic context.  KEY and VALUE are
encoded using the ENCODE-JSON generic function, so they both must be
of a type for which an ENCODE-JSON method is defined.  If KEY does not
encode to a string, its JSON representation (as a string) is encoded
over again."
  (with-object-element (key stream)
    (encode-json value stream))
  value)

(defun stream-object-element-encoder (stream)
  "Return a function which takes two arguments and encodes them to
STREAM as an object element (key:value pair)."
  (lambda (key value)
    (encode-object-element key value stream)))

;;; End of YASON code.


(defmethod encode-json ((s list) &optional (stream *standard-output*))
  "Write the JSON representation of the list S to STREAM (or to
*STANDARD-OUTPUT*).  If S is a proper alist, it is encoded as a JSON
object, otherwise as a JSON array."
  (handler-case 
      (write-string (with-output-to-string (temp)
                      (call-next-method s temp))
                    stream)
    ((or type-error #+ccl simple-error) (e)
      (declare (ignore e))
      (encode-json-alist s stream))))

(defmethod encode-json((s sequence) stream)
   (let ((first-element t))
     (write-char #\[ stream)    
     (map nil #'(lambda (element) 
                 (if first-element
                     (setf first-element nil)
                     (write-char #\, stream))
                 (encode-json element stream))
         s)
    (write-char #\] stream)))

(defmacro write-json-object (generator-fn stream)
  (let ((strm (gensym))
        (first-element (gensym)))
    `(let ((,first-element t)
           (,strm ,stream))
      (write-char #\{ ,strm)
      (loop
       (multiple-value-bind (more name value)
           (,generator-fn)
         (unless more (return))
         (if ,first-element
             (setf ,first-element nil)
             (write-char #\, ,strm))
         (encode-json name ,strm)
         (write-char #\: ,strm)
         (encode-json value ,strm)))
      (write-char #\} ,strm))))

(defmethod encode-json ((h hash-table) &optional (stream *standard-output*))
  "Write the JSON representation (object) of the hash table H to
STREAM (or to *STANDARD-OUTPUT*)."
  (with-object (stream)
    (maphash (stream-object-element-encoder stream) h)))

(defmethod encode-json ((o standard-object)
                        &optional (stream *standard-output*))
  "Write the JSON representation (object) of the CLOS object O to
STREAM (or to *STANDARD-OUTPUT*)."
  (with-object (stream)
    (map-slots (stream-object-element-encoder stream) o)))

(defun encode-json-alist (alist stream)
  "Write the JSON representation (object) of ALIST to STREAM (or to
*STANDARD-OUTPUT*)."
  (with-object (stream)
    (loop for (key . value) in alist
       do (encode-object-element key value stream))))

(defun encode-json-alist-to-string (alist)
  "Return the JSON representation (object) of ALIST as a string."
  (with-output-to-string (stream)
    (encode-json-alist alist stream)))

(defun encode-json-plist (plist stream)
  "Write the JSON representation (object) of PLIST to STREAM (or to
*STANDARD-OUTPUT*)."
  (with-object (stream)
    (loop for (key value) on plist by #'cddr
       do (encode-object-element key value stream))))

(defun encode-json-plist-to-string (plist)
  "Return the JSON representation (object) of PLIST as a string."
  (with-output-to-string (stream)
    (encode-json-plist plist stream)))

(defun write-json-string (s stream)
  "Write a JSON string representation of S (double-quote-delimited
string) to STREAM."
  (write-char #\" stream)
  (if (stringp s)
      (write-json-chars s stream)
      (encode-json s stream))
  (write-char #\" stream))

(defun write-json-chars (s stream)
  "Write JSON representations (chars or escape sequences) of
characters in string S to STREAM."
  (loop for ch across s
     for code = (char-code ch)
     with special
     if (setq special (car (rassoc ch +json-lisp-escaped-chars+)))
       do (write-char #\\ stream) (write-char special stream)
     else if (< #x1f code #x7f)
       do (write-char ch stream)
     else
       do (let ((special '#.(rassoc-if #'consp +json-lisp-escaped-chars+)))
            (destructuring-bind (esc . (width . radix)) special
              (format stream "\\~C~V,V,'0R" esc radix width code)))))

(defun write-json-number (nr stream)
  "Write the JSON representation of the number NR to STREAM."
  (if (integerp nr)
      (format stream "~d" nr)
      (format stream "~f" nr)))
