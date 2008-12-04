;;;; Copyright (c) 2006-2008 Henrik Hjelte
;;;; All rights reserved.
;;;; See the file LICENSE for terms of use and distribution.

(in-package :json)


;;; Token reader

(define-condition json-syntax-error (simple-error stream-error)
  ((stream-file-position :reader stream-error-stream-file-position
                         :initarg :stream-file-position))
  (:report
   (lambda (condition stream)
     (format stream "~? [in ~S~@[ at position ~D~]]"
             (simple-condition-format-control condition)
             (simple-condition-format-arguments condition)
             (stream-error-stream condition)
             (stream-error-stream-file-position condition)))))

(defun json-syntax-error (stream format-control &rest format-arguments)
  (error 'json-syntax-error
         :stream stream
         :stream-file-position (file-position stream)
         :format-control format-control
         :format-arguments format-arguments))

(defun read-json-token (stream)
  (let ((c (peek-char t stream)))
    (case c
      ((#\{ #\[ #\] #\} #\" #\: #\,)
       (values :punct (read-char stream)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\-)
       (read-json-number-token stream))
      (t (if (alpha-char-p c)
             (read-json-symbol-token stream)
             (json-syntax-error stream "Invalid char on JSON input: `~C'"
                                c))))))

(defun read-json-number-token (stream)
  (let ((int (make-array 32 :adjustable t :fill-pointer 0
                         :element-type 'character))
        (frac (make-array 32 :adjustable t :fill-pointer 0
                          :element-type 'character))
        (exp (make-array 32 :adjustable t :fill-pointer 0
                         :element-type 'character))
        (type :integer)
        c)
    (flet ((safe-read-char (stream)
             (handler-case (read-char stream)
               (end-of-file ()
                 (return-from read-json-number-token
                   (values type (concatenate 'string int frac exp)))))))
      (macrolet
          ((read-digits (part)
             (let ((error-fmt
                    (format nil "Invalid JSON number: no ~(~A~) digits"
                            part)))
               `(loop while (char<= #\0 c #\9)
                   with count = 0
                   do (vector-push-extend c ,part 32)
                      (setq c (safe-read-char stream))
                      (incf count)
                   finally
                     (if (zerop count)
                         (json-syntax-error stream ,error-fmt))))))
        (setq c (read-char stream))
        (when (char= c #\-)
          (vector-push c int)
          (setq c (read-char stream)))
        (if (char= c #\0)
            (progn
              (vector-push c int)
              (setq c (safe-read-char stream)))
            (read-digits int))
        (when (char= c #\.)
          (vector-push c frac)
          (setq c (read-char stream)
                type :real)
          (read-digits frac))
        (when (char-equal c #\e)
          (vector-push c exp)
          (setq c (read-char stream)
                type :real)
          (when (or (char= c #\+) (char= c #\-))
            (vector-push c exp)
            (setq c (read-char stream)))
          (read-digits exp))
        (unread-char c stream)
        (values type (concatenate 'string int frac exp))))))

(defun read-json-symbol-token (stream)
  (let ((symbol (make-array 8 :adjustable t :fill-pointer 0
                            :element-type 'character)))
    (loop for c = (read-char stream nil)
       while (and c (alpha-char-p c))
       do (vector-push-extend c symbol 32)
       finally (if c (unread-char c stream)))
    (setq symbol (coerce symbol 'string))
    (if (assoc symbol +json-lisp-symbol-tokens+ :test #'equal)
        (values :boolean symbol)
        (json-syntax-error stream "Invalid JSON symbol: ~A" symbol))))

(define-condition no-char-for-code (error)
  ((offending-code :initarg :code :reader offending-code))
  (:report (lambda (condition stream)
             (format stream "No character corresponds to code #x~4,'0X."
                     (offending-code condition)))))

(defun read-json-string-char (stream)
  (let ((esc-error-fmt "Invalid JSON character escape sequence: ~A~A")
        (c (read-char stream)))
    (case c
      (#\" nil)                         ; End of string
      (#\\ (let ((c (read-char stream)))
             (let ((unescaped (cdr (assoc c +json-lisp-escaped-chars+))))
               (typecase unescaped
                 (char unescaped)
                 (cons
                  (destructuring-bind (len . rdx) unescaped
                    (let ((code
                           (let ((repr (make-string len)))
                             (dotimes (i len)
                               (setf (aref repr i) (read-char stream)))
                             (handler-case (parse-integer repr :radix rdx)
                               (parse-error ()
                                 (json-syntax-error stream esc-error-fmt
                                                    (format nil "\\~C" c)
                                                    repr))))))
                      (if (< code char-code-limit)
                          (code-char code)
                          (restart-case
                              (error 'no-char-for-code :code code)
                            (substitute-char (char)
                              :report "Substitute another char."
                              :interactive
                              (lambda ()
                                (format *query-io* "Char: ")
                                (list (read-char *query-io*)))
                              char)
                            (pass-code ()
                              :report "Pass the code to char handler."
                              c))))))
                 (t (if *use-strict-json-rules*
                        (json-syntax-error stream esc-error-fmt "\\" c)
                        c))))))
      (t c))))


;;; The decoder base

(define-custom-var (:integer *integer-handler*))
(define-custom-var (:real *real-handler*))
(define-custom-var (:boolean *boolean-handler*))

(define-custom-var (:beginning-of-string *beginning-of-string-handler*))
(define-custom-var (:string-char *string-char-handler*))
(define-custom-var (:end-of-string *end-of-string-handler*))

(define-custom-var (:beginning-of-array *beginning-of-array-handler*))
(define-custom-var (:array-element *array-element-handler*))
(define-custom-var (:end-of-array *end-of-array-handler*))

(define-custom-var (:beginning-of-object *beginning-of-object-handler*))
(define-custom-var (:object-key *object-key-handler*))
(define-custom-var (:object-value *object-value-handler*))
(define-custom-var (:end-of-object *end-of-object-handler*))

(define-custom-var (:structure-scope-variables *structure-scope-variables*)
    nil)

(defun decode-json (&optional (stream *standard-input*))
  "Read a JSON value from STREAM."
  (multiple-value-bind (type token) (read-json-token stream)
    (dispatch-on-token type token stream)))

(defun decode-json-from-string (json-string)
  "Read a JSON value from JSON-STRING."
  (with-input-from-string (stream json-string)
    (decode-json stream)))

(defun decode-json-strict (&optional (stream *standard-input*))
  "Same as DECODE-JSON, but allow only objects or arrays on the top
level, no junk afterwards."
  (assert (member (peek-char t stream) '(#\{ #\[)))
  (let ((object (decode-json stream)))
    (assert (eq :no-junk (peek-char t stream nil :no-junk)))
    object))

(defun dispatch-on-token (type token stream)
  (ecase type
    (:punct
     (case token
       (#\" (decode-json-string stream))
       (#\[ (decode-json-array stream))
       (#\{ (decode-json-object stream))
       (t (json-syntax-error stream
                             "Token out of place on JSON input: `~C'"
                             token))))
    (:integer (funcall *integer-handler* token))
    (:real (funcall *real-handler* token))
    (:boolean (funcall *boolean-handler* token))))

(defmacro structure-scope-progv (&body body)
  `(progv *structure-scope-variables*
       (mapcar #'symbol-value *structure-scope-variables*)
     ,@body))
    
(defun decode-json-array (stream)
  (structure-scope-progv
    (funcall *beginning-of-array-handler*)
    (multiple-value-bind (type token) (read-json-token stream)
      (if (and (eql type :punct) (char= token #\]))
          (return-from decode-json-array
            (funcall *end-of-array-handler*))
          (funcall *array-element-handler*
                   (dispatch-on-token type token stream))))
    (loop
       (multiple-value-bind (type token) (read-json-token stream)
         (if (eql type :punct)
             (case token
               (#\] (return-from decode-json-array
                      (funcall *end-of-array-handler*)))
               (#\, (setq token nil))))
         (if token
             (json-syntax-error
              stream
              "Token out of place in array on JSON input: `~A'"
              token)))
       (funcall *array-element-handler* (decode-json stream)))))

(defun decode-json-object (stream)
  (structure-scope-progv
   (loop with key = nil
      for first-time-p = t then nil
      initially (funcall *beginning-of-object-handler*)
      do (multiple-value-bind (type token) (read-json-token stream)
           (if (eql type :punct)
               (case token
                 (#\}
                  (if first-time-p
                      (return-from decode-json-object
                        (funcall *end-of-object-handler*))))
                 (#\"
                  (setq key (decode-json-string stream)))))
           (if key
               (funcall *object-key-handler* key)
               (json-syntax-error
                stream
                "Expected a key string in object on JSON input ~
                 but found `~A'"
                token)))
        (multiple-value-bind (type token) (read-json-token stream)
          (unless (and (eql type :punct) (char= token #\:))
            (json-syntax-error
             stream
             "Expected a `:' separator in object on JSON input ~
              but found `~A'"
             token)))
        (funcall *object-value-handler* (decode-json stream))
        (multiple-value-bind (type token) (read-json-token stream)
          (if (eql type :punct)
              (case token
                (#\} (return-from decode-json-object
                       (funcall *end-of-object-handler*)))
                (#\, (setq key nil))))
          (if key
              (json-syntax-error
               stream
               "Expected a `,' separator or `}' in object on JSON input ~
                but found `~A'"
               token))))))

(defun decode-json-string (stream)
  (structure-scope-progv
   (loop initially (funcall *beginning-of-string-handler*)
      for c = (read-json-string-char stream)
      while c
      do (funcall *string-char-handler* c)
      finally (return (funcall *end-of-string-handler*)))))


;;; The list semantics

(defvar *json-array-type* 'vector
  "The Lisp sequence type to which JSON arrays are to be coerced.")

(defun parse-number (token)
  ;; We can be reasonably sure that nothing but well-formed (both in
  ;; JSON and Lisp sense) number literals gets to this point.
  (read-from-string token))

(defun json-boolean-to-lisp (token)
  ;; We can be reasonably sure that nothing but well-formed boolean
  ;; literals gets to this point.
  (cdr (assoc token +json-lisp-symbol-tokens+ :test #'string=)))

(defvar *accumulator* nil)
(defvar *accumulator-last* nil)

(defun init-accumulator ()
  (let ((head (cons nil nil)))
    (setq *accumulator* head)
    (setq *accumulator-last* head)))

(defun accumulator-add (element)
  (setq *accumulator-last*
        (setf (cdr *accumulator-last*) (cons element nil))))

(defun accumulator-add-key (key)
  (let ((key (json-intern (camel-case-to-lisp key))))
    (setq *accumulator-last*
          (setf (cdr *accumulator-last*) (cons (cons key nil) nil)))))

(defun accumulator-add-value (value)
  (setf (cdar *accumulator-last*) value)
  *accumulator-last*)

(defun accumulator-get-sequence ()
  (coerce (cdr *accumulator*) *json-array-type*))

(defun accumulator-get ()
  (cdr *accumulator*))

(defun init-vector-accumulator ()
  (setq *accumulator*
        (make-array 32 :adjustable t :fill-pointer 0)))

(defun vector-accumulator-add (element)
  (vector-push-extend element *accumulator* (fill-pointer *accumulator*))
  *accumulator*)

(defun vector-accumulator-get-sequence ()
  (coerce *accumulator* *json-array-type*))

(defun vector-accumulator-get-string ()
  (coerce *accumulator* 'string))

(defun set-decoder-simple-list-semantics ()
  (set-custom-vars
   :integer #'parse-number
   :real #'parse-number
   :boolean #'json-boolean-to-lisp
   :beginning-of-array #'init-accumulator
   :array-element #'accumulator-add
   :end-of-array #'accumulator-get-sequence
   :beginning-of-object #'init-accumulator
   :object-key #'accumulator-add-key
   :object-value #'accumulator-add-value
   :end-of-object #'accumulator-get
   :beginning-of-string #'init-vector-accumulator
   :string-char #'vector-accumulator-add
   :end-of-string #'vector-accumulator-get-string
   :structure-scope-variables
     (union *structure-scope-variables*
            '(*accumulator* *accumulator-last*))))

(defmacro with-decoder-simple-list-semantics (&body body)
  `(with-shadowed-custom-vars
     (set-decoder-simple-list-semantics)
     ,@body))


;;; The CLOS semantics

(defvar *prototype-prototype*
  (make-instance 'prototype
    :lisp-class 'prototype
    :lisp-package :json))

(defvar *prototype* nil)

(defun init-accumulator-and-prototype ()
  (init-accumulator)
  (setq *prototype*
        (if (eql *prototype* t) *prototype-prototype* nil)))

(defun accumulator-add-key-or-set-prototype (key)
  (let ((key (camel-case-to-lisp key)))
    (if (and (not *prototype*)
             *prototype-name*
             (string= key (symbol-name *prototype-name*)))
        (setq *prototype* t)
        (setq *accumulator-last*
              (setf (cdr *accumulator-last*) (cons (cons key nil) nil))))
    *accumulator*))

(defun accumulator-add-value-or-set-prototype (value)
  (if (eql *prototype* t)
      (progn
        (assert (typep value 'prototype) (value)
          "Invalid prototype: ~S.  Want to substitute something else?"
          value)
        (setq *prototype* value)
        *accumulator*)
      (accumulator-add-value value)))

(defun accumulator-get-object ()
  (flet ((as-symbol (value)
           (if (stringp value)
               (json-intern (camel-case-to-lisp value))
               value))
         (intern-keys (bindings)
           (loop for (key . value) in bindings
              collect (cons (json-intern key) value))))
    (if (typep *prototype* 'prototype)
        (with-slots (lisp-class lisp-superclasses lisp-package)
            *prototype*
          (let* ((*json-symbols-package*
                  (or (find-package (as-symbol lisp-package))
                      *json-symbols-package*))
                 (class (as-symbol lisp-class))
                 (superclasses (mapcar #'as-symbol lisp-superclasses)))
            (make-object (intern-keys (cdr *accumulator*)) class
                           :superclasses superclasses)))
        (make-object (intern-keys (cdr *accumulator*)) nil))))

(defun set-decoder-simple-clos-semantics ()
  (set-custom-vars
   :integer #'parse-number
   :real #'parse-number
   :boolean #'json-boolean-to-lisp
   :beginning-of-array #'init-vector-accumulator
   :array-element #'vector-accumulator-add
   :end-of-array #'vector-accumulator-get-sequence
   :beginning-of-object #'init-accumulator-and-prototype
   :object-key #'accumulator-add-key-or-set-prototype
   :object-value #'accumulator-add-value-or-set-prototype
   :end-of-object #'accumulator-get-object
   :beginning-of-string #'init-vector-accumulator
   :string-char #'vector-accumulator-add
   :end-of-string #'vector-accumulator-get-string
   :structure-scope-variables
     (union *structure-scope-variables*
            '(*accumulator* *accumulator-last* *prototype*))))

(defmacro with-decoder-simple-clos-semantics (&body body)
  `(with-shadowed-custom-vars
     (set-decoder-simple-clos-semantics)
     ,@body))


;;; List semantics is the default.

(set-decoder-simple-list-semantics)
