(in-package :json-rpc)

;; http://json-rpc.org/wiki/specification
;; http://json-rpc.org/wd/JSON-RPC-1-1-WD-20060807.html

(defvar *json-rpc-functions* (make-hash-table :test #'equal))

(defun clear-exported ()
  (clrhash *json-rpc-functions*))

(defmacro defun-json-rpc (name lambda-list &body body)
  "Defines a function and registers it as a json-rpc target."
  `(progn
     (defun ,name ,lambda-list ,@body)
     (export-as-json-rpc #',name (string-downcase (symbol-name ',name)))))

(defun export-as-json-rpc (func function-name)
  (setf (gethash function-name *json-rpc-functions*) func))

(defun make-rpc-response (&key result error id)
  "When the method invocation completes, the service must reply with a response. The response is a single object serialized using JSON.

It has three properties:

    * result - The Object that was returned by the invoked method. This must be null in case there was an error invoking the method.
    * error - An Error object if there was an error invoking the method. It must be null if there was no error.
    * id - This must be the same id as the request it is responding to. "
  (json:encode-json-alist-to-string
    `((:result . ,result)
      (:error . ,error)
      (:id . ,id))))


(defun invoke-rpc (json-string)
  "A remote method is invoked by sending a request to a remote service. The request is a single object serialized using JSON.

It has three properties:

    * method - A String containing the name of the method to be invoked.
    * params - An Array of objects to pass as arguments to the method.
    * id - The request id. This can be of any type. It is used to match the response with the request that it is replying to. "
  (json-bind (method params id) json-string
    (restart-case
        (let ((func (gethash method *json-rpc-functions*)))
          (if func
              (make-rpc-response :id id :result (restart-case (apply func params)
                                                  (use-value (value)
                                                    value)))
              (make-rpc-response :id id :error "Function not found.")))
      (send-error (error-message)
        (make-rpc-response :id id :error error-message))
      (send-nothing ()
        nil)
      (send-internal-error ()
        (make-rpc-response :id id :error "An internal error occurred on the server.")))))
