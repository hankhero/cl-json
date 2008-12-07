;;;; Copyright (c) 2006-2008 Henrik Hjelte
;;;; Copyright (c) 2008 Hans Hübner (marked parts)
;;;; All rights reserved.
;;;; See the file LICENSE for terms of use and distribution.

(in-package :json)

(defgeneric encode-json (object &optional stream))

(defun encode-json-to-string (object)
  (with-output-to-string (stream)
    (encode-json object stream)))

(defmethod encode-json (anything &optional (stream *standard-output*))
  (write-json-string (format nil "~A" anything) stream))

(defmethod encode-json ((nr number) &optional (stream *standard-output*))
  (write-json-number nr stream))

(defmethod encode-json ((s string) &optional (stream *standard-output*)) 
  (write-json-string s stream))

(defmethod encode-json ((c character) &optional (stream *standard-output*))
  "JSON does not define a character type, we encode characters as strings."
  (encode-json (string c) stream))

(defmethod encode-json ((s symbol) &optional (stream *standard-output*))
  (let ((mapped (car (rassoc s +json-lisp-symbol-tokens+))))
    (if mapped
        (write-string mapped stream)
        (let ((s (funcall *lisp-identifier-name-to-json* (symbol-name s))))
          (write-json-string s stream)))))


;;; The code below is from Hans Hübner's YASON (with modifications).

(defvar *json-aggregate-first* t)

(defun next-aggregate-element (stream)
  (prog1 *json-aggregate-first*
    (unless *json-aggregate-first*
      (write-char #\, stream))
    (setq *json-aggregate-first* nil)))

(defmacro with-aggregate ((begin-char end-char
                           &optional (stream '*standard-output*))
                          &body body)
  `(let ((*json-aggregate-first* *json-aggregate-first*))
     (declare (special *json-aggregate-first*))
     (write-char ,begin-char ,stream)
     (prog1 (progn ,@body)
       (write-char ,end-char ,stream))))

(defmacro with-array ((&optional (stream '*standard-output*)) &body body)
  "Open a JSON array, then run BODY.  Inside the body,
ENCODE-ARRAY-ELEMENT must be called to encode elements to the opened
array."
  `(with-aggregate (#\[ #\] ,stream) ,@body))

(defmacro with-object ((&optional (stream '*standard-output*)) &body body)
  "Open a JSON object, then run BODY.  Inside the body,
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
  (with-object (stream)
    (maphash (stream-object-element-encoder stream) h)
    (maybe-write-prototype 'hash-table)))

(defmethod encode-json ((o standard-object)
                        &optional (stream *standard-output*))
  (with-object (stream)
    (map-object-slots-and-prototype (stream-object-element-encoder stream)
                                    o)))

(defun encode-json-alist (alist stream)
  (with-object (stream)
    (map nil (lambda (pair)
               (destructuring-bind (key . value) pair
                 (encode-object-element key value stream)))
         alist)
    (maybe-write-prototype 'cons)))

(defun encode-json-alist-to-string(alist)
  (with-output-to-string (stream)
    (encode-json-alist alist stream)))

(defun encode-json-plist (plist stream)
  (with-object (stream)
    (loop for (key value) on plist by #'cddr
       do (encode-object-element key value stream))
    (maybe-write-prototype 'list)))

(defun encode-json-plist-to-string (plist)
  (with-output-to-string (stream)
    (encode-json-plist plist stream)))

(defun write-json-string (s stream)
  (write-char #\" stream)
  (if (stringp s)
      (write-json-chars s stream)
      (encode-json s stream))
  (write-char #\" stream))

(defun write-json-chars (s stream)
  (loop for ch across s
     for code = (char-code ch)
     with special
     if (> code #x1f)
       do (let ((special (rassoc-if #'consp +json-lisp-escaped-chars+)))
            (destructuring-bind (esc . (width . radix)) special
              (format stream "\\~C~V,V,'0R" esc radix width code)))
     else if (setq special (car (rassoc ch +json-lisp-escaped-chars+)))
       do (write-char #\\ stream)
     else
       do (write-char ch stream)))

(defun write-json-number (nr stream)
  (if (integerp nr)
      (format stream "~d" nr)
      (format stream "~f" nr)))

(defun maybe-write-prototype (class)
  (if *prototype-name*
      (encode-object-element *prototype-name*
                             (make-object-prototype class))))
