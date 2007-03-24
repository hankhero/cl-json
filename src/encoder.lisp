(in-package :json)

(defparameter *symbol-to-string-fn* #'js::symbol-to-js)

(defgeneric encode-json (object stream))

(defun encode-json-to-string(object)
  (with-output-to-string (stream)
    (encode-json object stream)))

(defmethod encode-json((nr number) stream)
  (write-json-number nr stream))

(defmethod encode-json((s string) stream) 
  (write-json-string s stream))

(defmethod encode-json ((c character) stream)
  "JSON does not define a character type, we encode characters as strings."
  (encode-json (string c) stream))

(defmethod encode-json((s symbol) stream)
  (cond
    ((null s) (write-json-chars "null" stream))
    ((eq 't s) (write-json-chars "true" stream))
    (t (write-json-string (funcall *symbol-to-string-fn* s) stream))))

(defmethod encode-json((s list) stream)
  (handler-case 
      (write-string (with-output-to-string (temp)
                      (call-next-method s temp))
                    stream)
    (type-error (e)
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

(defmethod encode-json((h hash-table) stream)
  (with-hash-table-iterator (generator h)
      (write-json-object generator stream)))

(defmacro with-alist-iterator ((generator-fn alist) &body body)
  (let ((stack (gensym)))
    `(let ((,stack (copy-alist ,alist)))
      (flet ((,generator-fn ()
               (let ((cur (pop ,stack)))
                 (if cur
                     (values t (car cur) (cdr cur))
                     nil))))
        ,@body))))
        
(defun encode-json-alist (alist stream)
  (with-alist-iterator (gen-fn alist)
    (write-json-object gen-fn stream)))

(defun encode-json-alist-to-string(alist)
  (with-output-to-string (stream)
    (encode-json-alist alist stream)))


(defun write-json-string (s stream)
  (write-char #\" stream)
  (if (stringp s)
      (write-json-chars s stream)
      (encode-json s stream))
  (write-char #\" stream))

(defun write-json-chars (s stream)
  (declare (inline lisp-special-char-to-json))
  (loop for ch across s
        for code = (char-code ch)
        for special = (lisp-special-char-to-json ch)
        do
        (cond
          ((and special (not (char= special #\/)))
           (write-char #\\ stream)
           (write-char special stream))
          ((<= code #x1f)
           (format stream "\\u~4,'0x" code))
          (t (write-char ch stream)))))

(defun write-json-number (nr stream)
  (if (integerp nr)
      (format stream "~d" nr)
      (format stream "~f" nr)))
