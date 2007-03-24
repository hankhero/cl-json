(defpackage :json
  (:use :common-lisp)
  (:export
    #:*json-symbols-package*
    #:*json-object-factory*
    #:*json-object-factory-add-key-value*
    #:*json-object-factory-return*
    #:*json-make-big-number*
    
    #:decode-json
    #:decode-json-strict
    #:decode-json-from-string

    #:*use-strict-json-rules*
    #:json-parse-error

    #:encode-json
    #:encode-json-to-string
    #:encode-json-alist
    #:encode-json-alist-to-string

    #:json-bind
    ))

(defpackage :json-rpc
  (:use :common-lisp :json)
  (:export
    #:clear-exported
    #:defun-json-rpc
    #:export-as-json-rpc
    #:invoke-rpc
    ))
