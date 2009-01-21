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
             (stream-error-stream-file-position condition))))
  (:documentation
   "Signalled when non-well-formed JSON data are encountered."))

(defun json-syntax-error (stream format-control &rest format-arguments)
  "Signal a JSON-SYNTAX-ERROR condition."
  (error 'json-syntax-error
         :stream stream
         :stream-file-position (file-position stream)
         :format-control format-control
         :format-arguments format-arguments))

(defun read-json-token (stream)
  "Read a JSON token (symbol, number or punctuation char) from the
given STREAM, and return 2 values: the token category (a symbol) and
the token itself, as a string or character."
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

(defun peek-json-token (stream)
  "Return 2 values: the category and the first character of the next
token available in the given STREAM.  Unlike READ-JSON-TOKEN, this
function can not discriminate between integers and reals (hence, it
returns a single :NUMBER category), and cannot check whether the next
available symbol is a valid boolean or not (hence, the category for
such tokens is :SYMBOL)."
  (let ((c (peek-char t stream)))
    (values
     (case c
       ((#\{ #\[ #\] #\} #\" #\: #\,) :punct)
       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\-) :number)
       (t (if (alpha-char-p c)
              :symbol
              (json-syntax-error stream "Invalid char on JSON input: `~C'"
                                 c))))
     c)))

(defun read-json-number-token (stream)
  "Read a JSON number token from the given STREAM, and return 2
values: the token category (:INTEGER or :REAL) and the token itself,
as a string."
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
  "Read a JSON symbol token from the given STREAM, and return 2
values: the token category (:BOOLEAN) and the token itself, as a
string."
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
                     (offending-code condition))))
  (:documentation
   "Signalled when, in a JSON string, an escaped code point (\uXXXX)
is encountered which is greater than the application's CHAR-CODE-LIMIT."))

(defun read-json-string-char (stream)
  "Read a JSON string char (or escape sequence) from the STREAM and
return it.  If an end of string (unescaped quote) is encountered,
return NIL."
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

(define-custom-var (:integer *integer-handler*) (constantly 0)
  "Designator for a function of 1 string argument (integer token).")
(define-custom-var (:real *real-handler*) (constantly 0)
  "Designator for a function of 1 string argument (real token).")
(define-custom-var (:boolean *boolean-handler*) (constantly t)
  "Designator for a function of 1 string argument (boolean token).")

(define-custom-var (:beginning-of-string *beginning-of-string-handler*)
    (constantly t)
  "Designator for a function of no arguments (called at encountering
an opening quote for a string).")
(define-custom-var (:string-char *string-char-handler*) (constantly t)
  "Designator for a function of 1 character argument (string char).")
(define-custom-var (:end-of-string *end-of-string-handler*) (constantly "")
  "Designator for a function of no arguments (called at encountering
a closing quote for a string).")

(define-custom-var (:beginning-of-array *beginning-of-array-handler*)
    (constantly t)
  "Designator for a function of no arguments (called at encountering
an opening bracket for an array).")
(define-custom-var (:array-element *array-element-handler*) (constantly t)
    "Designator for a function of 1 arbitrary argument (decoded array
element).")
(define-custom-var (:end-of-array *end-of-array-handler*) (constantly nil)
    "Designator for a function of no arguments (called at encountering
a closing bracket for an array).")

(define-custom-var (:beginning-of-object *beginning-of-object-handler*)
    (constantly t)
  "Designator for a function of no arguments (called at encountering
an opening brace for an object).")
(define-custom-var (:object-key *object-key-handler*) (constantly t)
  "Designator for a function of 1 string argument (decoded object
key).")
(define-custom-var (:object-value *object-value-handler*) (constantly t)
  "Designator for a function of 1 arbitrary argument (decoded object
element).")
(define-custom-var (:end-of-object *end-of-object-handler*)
    (constantly nil)
  "Designator for a function of no arguments (called at encountering
a closing brace for an object).")

(define-custom-var (:internal-decoder *internal-decoder*) 'decode-json
  "Designator for a function of 1 stream argument called (instead of
DECODE-JSON) to decode an element of an array or of an object.")

(define-custom-var (:object-scope-variables *object-scope-variables*)
    '(*internal-decoder*)
  "A list of symbols naming dynamic variables which should be re-bound
in the scope of every JSON object.")
(define-custom-var (:array-scope-variables *array-scope-variables*)
    '(*internal-decoder*)
  "A list of symbols naming dynamic variables which should be re-bound
in the scope of every JSON array.")
(define-custom-var (:string-scope-variables *string-scope-variables*)
    nil
  "A list of symbols naming dynamic variables which should be re-bound
in the scope of every JSON string.")
(define-custom-var (:aggregate-scope-variables *aggregate-scope-variables*)
    nil
  "A list of symbols naming dynamic variables which should be re-bound
in the scope of every JSON structured value (object, array or string).")

(defun decode-json (&optional (stream *standard-input*))
  "Read a JSON value from STREAM."
  (multiple-value-bind (dispatch-token-type dispatch-token)
      (read-json-token stream)
    (ecase dispatch-token-type
      (:punct
       (case dispatch-token
         (#\" (decode-json-string stream))
         (#\[ (decode-json-array stream))
         (#\{ (decode-json-object stream))
         (t (json-syntax-error stream
                               "Token out of place on JSON input: `~C'"
                               dispatch-token))))
      (:integer (funcall *integer-handler* dispatch-token))
      (:real (funcall *real-handler* dispatch-token))
      (:boolean (funcall *boolean-handler* dispatch-token)))))

(defmacro custom-decoder (&rest customizations)
  "Return a function which is like DECODE-JSON called in a dynamic
environment with the given customizations."
  `(lambda (&optional (stream *standard-input*))
     (bind-custom-vars ,customizations
       (decode-json stream))))

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

(defmacro aggregate-scope-progv (variables &body body)
  "Establish a dynamic environment where all VARIABLES are freshly
bound (to their current values), and execute BODY in it, returning the
result."
  `(progv ,variables (mapcar #'symbol-value ,variables)
     ,@body))

(defun decode-json-array (stream)
  "Read comma-separated list of JSON values until a closing bracket,
calling array handlers as it goes."
  (aggregate-scope-progv *array-scope-variables*
    (aggregate-scope-progv *aggregate-scope-variables*
      (funcall *beginning-of-array-handler*)
      (multiple-value-bind (type token) (peek-json-token stream)
        (if (and (eql type :punct) (char= token #\]))
            (progn
              (read-json-token stream)
              (return-from decode-json-array
                (funcall *end-of-array-handler*)))
            (funcall *array-element-handler*
                     (funcall *internal-decoder* stream))))
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
         (funcall *array-element-handler*
                  (funcall *internal-decoder* stream))))))

(defun decode-json-object (stream)
  "Read comma-separated list of JSON key:value pairs until a closing brace,
calling object handlers as it goes."
  (aggregate-scope-progv *object-scope-variables*
    (aggregate-scope-progv *aggregate-scope-variables*
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
           (funcall *object-value-handler*
                    (funcall *internal-decoder* stream))
           (multiple-value-bind (type token) (read-json-token stream)
             (if (eql type :punct)
                 (case token
                   (#\} (return-from decode-json-object
                          (funcall *end-of-object-handler*)))
                   (#\, (setq key nil))))
             (if key
                 (json-syntax-error
                  stream
                  "Expected a `,' separator or `}' in object on JSON ~
                   input but found `~A'"
                  token)))))))

(defun decode-json-string (stream)
  "Read JSON string characters / escape sequences until a closing
double quote, calling string handlers as it goes."
  (aggregate-scope-progv *string-scope-variables*
    (aggregate-scope-progv *aggregate-scope-variables*
      (loop initially (funcall *beginning-of-string-handler*)
         for c = (read-json-string-char stream)
         while c
         do (funcall *string-char-handler* c)
         finally (return (funcall *end-of-string-handler*))))))


;;; The list semantics

(defvar *json-array-type* 'vector
  "The Lisp sequence type to which JSON arrays are to be coerced.")

(defun parse-number (token)
  "Take a number token and convert it to a numeric value."
  ;; We can be reasonably sure that nothing but well-formed (both in
  ;; JSON and Lisp sense) number literals gets to this point.
  (let ((*read-default-float-format* 'long-float))
    (read-from-string token)))

(defun json-boolean-to-lisp (token)
  "Take a symbol token and convert it to a boolean value."
  ;; We can be reasonably sure that nothing but well-formed boolean
  ;; literals gets to this point.
  (cdr (assoc token +json-lisp-symbol-tokens+ :test #'string=)))

(defvar *accumulator* nil
  "List or vector where elements are stored.")
(defvar *accumulator-last* nil
  "If *ACCUMULATOR* is a list, this refers to its last cons.")

(defun init-accumulator ()
  "Initialize a list accumulator."
  (let ((head (cons nil nil)))
    (setq *accumulator* head)
    (setq *accumulator-last* head)))

(defun accumulator-add (element)
  "Add ELEMENT to the end of the list accumulator."
  (setq *accumulator-last*
        (setf (cdr *accumulator-last*) (cons element nil))))

(defun accumulator-add-key (key)
  "Add a cons whose CAR is KEY to the end of the list accumulator."
  (let ((key (json-intern (funcall *json-identifier-name-to-lisp* key))))
    (setq *accumulator-last*
          (setf (cdr *accumulator-last*) (cons (cons key nil) nil)))))

(defun accumulator-add-value (value)
  "Set the CDR of the most recently accumulated cons to VALUE."
  (setf (cdar *accumulator-last*) value)
  *accumulator-last*)

(defun accumulator-get-sequence ()
  "Return all values accumulated so far in the list accumulator as
*JSON-ARRAY-TYPE*."
  (coerce (cdr *accumulator*) *json-array-type*))

(defun accumulator-get ()
  "Return all values accumulated so far in the list accumulator as a
list."
  (cdr *accumulator*))

(defun init-vector-accumulator ()
  "Initialize a vector accumulator."
  (setq *accumulator*
        (make-array 32 :adjustable t :fill-pointer 0)))

(defun vector-accumulator-add (element)
  "Add ELEMENT to the end of the vector accumulator."
  (vector-push-extend element *accumulator* (fill-pointer *accumulator*))
  *accumulator*)

(defun vector-accumulator-get-sequence ()
  "Return all values accumulated so far in a vector accumulator as
*JSON-ARRAY-TYPE*."
  (coerce *accumulator* *json-array-type*))

(defun vector-accumulator-get-string ()
  "Return all values accumulated so far in a vector accumulator as a
string."
  (coerce *accumulator* 'string))

(defun set-decoder-simple-list-semantics ()
  "Set the decoder semantics to the following:
  * Strings and numbers are decoded naturally, reals becoming floats.
  * The symbol true is decoded to T, false and null to NIL.
  * Arrays are decoded to sequences of the type *JSON-ARRAY-TYPE*.
  * Objects are decoded to alists.  Object keys are converted by the
function *JSON-IDENTIFIER-NAME-TO-LISP* and then interned in the
package *JSON-SYMBOLS-PACKAGE*."
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
   :aggregate-scope-variables (union *aggregate-scope-variables*
                                     '(*accumulator* *accumulator-last*))
   :internal-decoder #'decode-json))

(defmacro with-decoder-simple-list-semantics (&body body)
  "Execute BODY in a dynamic environement where the decoder semantics
is such as set by SET-DECODER-SIMPLE-LIST-SEMANTICS."
  `(with-shadowed-custom-vars
     (set-decoder-simple-list-semantics)
     ,@body))


;;; The CLOS semantics

(defvar *prototype-prototype*
  (make-instance 'prototype
    :lisp-class 'prototype
    :lisp-package :json)
  "The prototype for a prototype object.")

(defvar *prototype* nil
  "When NIL, the object being decoded does not (yet?) have a prototype.
When T, the decoder should get ready to decode a prototype field.
Otherwise, the value should be a prototype for the object being decoded.")

(defun init-accumulator-and-prototype ()
  "Initialize a list accumulator and a prototype."
  (init-accumulator)
  (if (eql *prototype* t)
      (setq *prototype* *prototype-prototype*
            *json-array-type* 'list)
      (setq *prototype* nil)))

(defun accumulator-add-key-or-set-prototype (key)
  "If KEY (in a JSON object being decoded) matches *PROTOTYPE-NAME*,
prepare to decode the corresponding value as a PROTOTYPE object.
Otherwise, do the same as ACCUMULATOR-ADD-KEY."
  (let ((key (funcall *json-identifier-name-to-lisp* key)))
    (if (and (not *prototype*)
             *prototype-name*
             (string= key (symbol-name *prototype-name*)))
        (setq *prototype* t)
        (setq *accumulator-last*
              (setf (cdr *accumulator-last*) (cons (cons key nil) nil))))
    *accumulator*))

(defun accumulator-add-value-or-set-prototype (value)
  "If VALUE (in a JSON object being decoded) corresponds to a key
which matches *PROTOTYPE-NAME*, set VALUE to be the prototype of the
object.  Otherwise, do the same as ACCUMULATOR-ADD-VALUE."
  (if (eql *prototype* t)
      (progn
        (assert (typep value 'prototype) (value)
          "Invalid prototype: ~S.  Want to substitute something else?"
          value)
        (setq *prototype* value)
        *accumulator*)
      (accumulator-add-value value)))

(defun accumulator-get-object ()
  "Return a CLOS object, using keys and values accumulated so far in
the list accumulator as slot names and values, respectively.  If the
JSON object had a prototype field infer the class of the object and
the package wherein to intern slot names from the prototype.
Otherwise, create a FLUID-OBJECT with slots interned in
*JSON-SYMBOLS-PACKAGE*."
  (flet ((as-symbol (value)
           (if (stringp value)
               (json-intern (funcall *json-identifier-name-to-lisp* value))
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
            (maybe-add-prototype
             (make-object (intern-keys (cdr *accumulator*)) class
                          :superclasses superclasses)
             *prototype*)))
        (make-object (intern-keys (cdr *accumulator*)) nil))))

(defun set-decoder-simple-clos-semantics ()
  "Set the decoder semantics to the following:
  * Strings and numbers are decoded naturally, reals becoming floats.
  * The symbol true is decoded to T, false and null to NIL.
  * Arrays are decoded to sequences of the type *JSON-ARRAY-TYPE*.
  * Objects are decoded to CLOS objects.  Object keys are converted by
the function *JSON-IDENTIFIER-NAME-TO-LISP*.  If a JSON object has a
field whose key matches *PROTOTYPE-NAME*, the class of the CLOS object
and the package wherein to intern slot names are inferred from the
corresponding value which must be a valid prototype.  Otherwise, a
FLUID-OBJECT is constructed whose slot names are interned in
*JSON-SYMBOLS-PACKAGE*."
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
   :aggregate-scope-variables (union *aggregate-scope-variables*
                                     '(*accumulator* *accumulator-last*))
   :object-scope-variables (union *object-scope-variables*
                                  '(*prototype* *json-array-type*))
   :internal-decoder #'decode-json))

(defmacro with-decoder-simple-clos-semantics (&body body)
  "Execute BODY in a dynamic environement where the decoder semantics
is such as set by SET-DECODER-SIMPLE-CLOS-SEMANTICS."
  `(with-shadowed-custom-vars
     (set-decoder-simple-clos-semantics)
     ,@body))


;;; List semantics is the default.

(set-decoder-simple-list-semantics)


;;; Shallow overriding of semantics.

(defmacro current-decoder (&rest keys)
  "Capture current values of custom variables and return a custom
decoder which restores these values in its extent."
  (let (exterior-bindings customizations)
    (flet ((collect (key var)
             (let ((exterior (gensym)))
               (push (list exterior var) exterior-bindings)
               (push exterior customizations)
               (push key customizations))))
      (if keys
          (loop for key in keys
             do (collect key (custom-key-to-variable key)))
          (loop-on-custom (key var)
             do (collect key var)))
      `(let ,exterior-bindings
         (custom-decoder ,@customizations)))))     

(defmacro with-custom-decoder-level ((&rest customizations) &body body)
  "Execute BODY in a dynamic environment such that, when nested
structures are decoded, the outermost level is decoded with the given
custom handlers (KEY-ARGS) whereas inner levels are decoded in the
usual way."
  `(let ((current-decoder
          (current-decoder
           ,@(loop for (key value) on customizations by #'cddr
                if (eq key :internal-decoder)
                  do (error "~S ~S customization is meaningless in ~
                             the context of WITH-CUSTOM-DECODER-LEVEL."
                            key value)
                else collect key))))
     (bind-custom-vars (:internal-decoder current-decoder ,@customizations)
       ,@body)))
