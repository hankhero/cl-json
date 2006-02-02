(defpackage :json
  (:use :common-lisp )
  (:export
    #:*json-object-factory*
    #:*json-object-factory-add-key-value*
    #:*json-object-factory-return*
    #:*json-make-big-number*
    #:decode-json
    #:decode-json-strict
    #:decode-json-from-string

    #:encode-json
    #:encode-json-to-string
    #:encode-json-alist
    ))
