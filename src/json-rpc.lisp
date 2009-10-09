;;;; Copyright (c) 2006-2008 Henrik Hjelte
;;;; All rights reserved.
;;;; See the file LICENSE for terms of use and distribution.

(in-package :json-rpc)

;; http://json-rpc.org/wiki/specification
;; http://json-rpc.org/wd/JSON-RPC-1-1-WD-20060807.html

(defvar *json-rpc-functions* (make-hash-table :test #'equal))

(defun clear-exported ()
  (clrhash *json-rpc-functions*))

(defmacro defun-json-rpc (name type lambda-list &body body)
  "Defines a function and registers it as a json-rpc target."
  (case type
    (:explicit)
    (:guessing)
    (:streaming)
    (t (error "New version of defun-json-rpc requires a TYPE argument")))
  `(progn
     (defun ,name ,lambda-list ,@body)
     (export-as-json-rpc #',name (lisp-to-camel-case (symbol-name ',name))
                         ,type)))

(defun export-as-json-rpc (func function-name &optional type)
  "Registers a lambda function FUNC as a json-rpc function.
TYPE determines how the return value of FUNC should be interpreted:
:explicit using the explicit encoder syntax,
:guessing using the guessing encode syntax
:streaming as a raw JSON string.
"
  (setf (gethash function-name *json-rpc-functions*) (cons func (or type :guessing))))

(defun make-rpc-response (&key result error id)
  "When the method invocation completes, the service must reply with a response. The response is a single object serialized using JSON.

It has three properties:

    * result - The Object that was returned by the invoked method. This must be null in case there was an error invoking the method.
    * error - An Error object(unspecified in json-rpc 1.0) if there was an error invoking the method. Null if there was no error.
    * id - This must be the same id as the request it is responding to. "
  (with-explicit-encoder
    (json:encode-json-to-string
     `(:object
       (:result . ,result)
       (:error . ,error)
       (:id . ,id)))))

(defun make-json-rpc-error-object-1.1 (message &key code error-object)
  "This code is based on the Working Draft 7 August 2006 of Json-rpc 1.1 specification. 
  http://json-rpc.org/wd/JSON-RPC-1-1-WD-20060807.html
"
  (let ((eo `(:object
              (:name . "JSONRPCError")
              (:code . ,(or code 999))
              (:message . ,message))))
    (if error-object
        (append eo `((:error . ,error-object)))
        eo)))

(defun invoke-rpc (json-source)
  "A remote method is invoked by sending a request to a remote service. The request is a single object serialized using JSON.

It has three properties:

    * method - A String containing the name of the method to be invoked.
    * params - An Array of objects to pass as arguments to the method.
    * id - The request id. This can be of any type. It is used to match the response with the request that it is replying to. "
  (json-bind (method params id) json-source
    (restart-case
        (let ((func-type (gethash method *json-rpc-functions*)))
          (if func-type
              (destructuring-bind (func . type) func-type
                (let ((retval (restart-case (apply func params)
                                                  (use-value (value)
                                                    value)))
                      explicit-retval)
                  (setf explicit-retval
                       (ecase type
                         (:guessing (list :json
                                          (with-guessing-encoder
                                            (encode-json-to-string retval))))
                         (:streaming (if (stringp retval)
                                         (list :json retval)))
                         (:explicit retval)))
                  (make-rpc-response :id id :result explicit-retval)))
              
              (make-rpc-response :id id :error (make-json-rpc-error-object-1.1 "Procedure not found"))))
      (send-error (message &optional code error-object)
        (make-rpc-response :id id :error (make-json-rpc-error-object-1.1 message
                                                                         :code code
                                                                         :error-object error-object)))
      (send-error-object (error-object)
        (make-rpc-response :id id :error error-object))
      (send-nothing ()
        nil)
      (send-internal-error ()
        (make-rpc-response :id id :error (make-json-rpc-error-object-1.1 "Service error"))))))

(defmacro def-restart (restart-name &rest (params))
  `(defun ,restart-name (,@params &optional condition)
     (let ((restart (find-restart ',restart-name condition)))
       (invoke-restart restart ,@params))))

(def-restart send-error (errmsg code))
(def-restart send-error-object (explicit-errobject))
(def-restart send-nothing ())
(def-restart send-internal-error ())
