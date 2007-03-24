(in-package :json)

(defvar *json-symbols-package* (find-package 'keyword) "The package where json-symbols are interned. Default keyword, nil = current package")

(defun json-intern (string)
  (if *json-symbols-package*
      (intern (camel-case-to-lisp string) *json-symbols-package*)
      (intern (camel-case-to-lisp string))))

(defparameter *json-rules* nil)

(defparameter *json-object-factory* #'(lambda () nil))
(defparameter *json-object-factory-add-key-value* #'(lambda (obj key value)
                                                      (push (cons (json-intern key) value)
                                                            obj)))
(defparameter *json-object-factory-return* #'(lambda (obj) (nreverse obj)))
(defparameter *json-make-big-number* #'(lambda (number-string) (format nil "BIGNUMBER:~a" number-string)))

(define-condition json-parse-error (error) ())

(defun decode-json-from-string (json-string)
  (with-input-from-string (stream json-string)
    (decode-json stream)))

(defun decode-json (&optional (stream *standard-input*))
  "Reads a json element from stream"
  (funcall (or (cdr (assoc (peek-char t stream) *json-rules*))
               #'read-json-number)
           stream))

(defun decode-json-strict (&optional (stream *standard-input*))
  "Only objects or arrays on top level, no junk afterwards."
  (assert (member (peek-char t stream) '(#\{ #\[)))
  (let ((object (decode-json stream)))
    (assert (eq :no-junk (peek-char t stream nil :no-junk)))
    object))

;;-----------------------


(defun add-json-dispatch-rule (character fn)
  (push (cons character fn) *json-rules*))

(add-json-dispatch-rule #\t #'(lambda (stream) (read-constant stream "true" t)))

(add-json-dispatch-rule #\f #'(lambda (stream) (read-constant stream "false" nil)))

(add-json-dispatch-rule #\n #'(lambda (stream) (read-constant stream "null" nil)))

(defun read-constant (stream expected-string ret-value)
  (loop for x across expected-string
        for ch = (read-char stream nil nil)
        always (char= ch x)
        finally (return ret-value)))

(defun read-json-string (stream)
  (read-char stream)
  (let ((val (read-json-chars stream '(#\"))))
    (read-char stream)
    val))

(add-json-dispatch-rule #\" #'read-json-string)

(defun read-json-object (stream)
  (read-char stream)
  (let ((obj (funcall *json-object-factory*)))
    (if (char= #\} (peek-char t stream))
        (read-char stream)
        (loop for skip-whitepace = (peek-char t stream)
              for key = (read-json-string stream)
              for separator = (peek-char t stream)
              for skip-separator = (assert (char= #\: (read-char stream)))
              for value = (decode-json stream)
              for terminator = (peek-char t stream)
              for skip-terminator = (assert (member (read-char stream) '(#\, #\})))
              do (setf obj (funcall *json-object-factory-add-key-value* obj key value))
              until (char= #\} terminator)))
    (funcall *json-object-factory-return* obj)))

(add-json-dispatch-rule #\{ #'read-json-object)

(defun read-json-array (stream)
  (read-char stream)
  (if (char= #\] (peek-char t stream))
      (progn (read-char stream) nil)
      (loop for first-in-element = (assert (not (member (peek-char t stream) '(#\, #\]))))
            for element = (decode-json stream)
            for terminator = (peek-char t stream)
            for skip-terminator = (assert (member (read-char stream) '(#\, #\])))
            collect element        
            until (char= #\] terminator))))

(add-json-dispatch-rule #\[ #'read-json-array)

(defparameter *digits* '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(defparameter *json-number-valid-chars* (concatenate 'list *digits* '(#\e #\E #\. #\+ #\-)))

(defun read-json-number (stream)
  (let ((number-string (read-chars-until stream
                                         :terminator-fn #'(lambda (ch)
                                                            (not (member ch *json-number-valid-chars*))))))
    (assert (if (char= (char number-string 0) #\0)
                (or (= 1 (length number-string)) (char= #\. (char number-string 1)))
                t))
    (handler-case 
        (read-from-string number-string)
      (serious-condition (e)
        (let ((e-pos (or (position #\e number-string)
                         (position #\E number-string))))
          (if e-pos
              (handler-case
                  (read-from-string (substitute #\l (aref number-string e-pos) number-string))
                (serious-condition ()
                  (funcall *json-make-big-number* number-string)))
              (error "Unexpected error ~S" e)))))))
    
(defun read-chars-until(stream &key terminator-fn (char-converter #'(lambda (ch stream)
                                                                       (declare (ignore stream))
                                                                       ch)))
  (with-output-to-string (ostr)
    (loop
     (let ((ch (peek-char nil stream nil nil)))
       (when (or (null ch)
                 (funcall terminator-fn ch))
         (return))
       (write-char (funcall char-converter
                            (read-char stream nil nil)
                            stream)
                   ostr)))))
       
(defun read-n-chars (stream n)
  (with-output-to-string (ostr)
    (dotimes (x n)
      (write-char (read-char stream) ostr))))
  
(defun read-json-chars(stream terminators)
  (read-chars-until stream :terminator-fn #'(lambda (ch)
                                              (member ch terminators))
                    :char-converter #'(lambda (ch stream)
                                        (if (char= ch #\\)
                                            (if (char= #\u (peek-char nil stream))
                                                (code-char (parse-integer (read-n-chars stream 5) :start 1 :radix 16))
                                                (json-escaped-char-to-lisp (read-char stream)))
                                            ch))))

(defun camel-case-to-lisp (string)
  "Converts a string in camelCase to the same lisp-friendly syntax used in parenscript.

(camel-case-to-string \"camelCase\") -> \"CAMEL-CASE\"
(camel-case-to-string \"CamelCase\") -> \"*CAMEL-CASE\"
(camel-case-to-string \"dojo.widget.TreeNode\") -> \"DOJO.WIDGET.*TREE-NODE\"
"
  (with-output-to-string (out)
    (loop for ch across string
          with last-char do
          (if (upper-case-p ch)
              (progn 
                (if (and last-char (lower-case-p last-char))
                    (write-char #\- out)
                    (write-char #\* out))
                (write-char ch out))
              (write-char (char-upcase ch) out))
          (setf last-char ch))))
