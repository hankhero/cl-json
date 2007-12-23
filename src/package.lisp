(defpackage :json
  (:use :common-lisp)
  (:export
    #:*prototype-name*
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
    )
  (:import-from #+(or mcl openmcl) #:ccl
                #+cmu #:clos-mop
                #+sbcl #:sb-mop
                #+(or clisp ecl lispworks) #:clos
                #+allegro #:mop
    #:slot-definition-name
    #:class-slots
    #:class-direct-superclasses
    #:remove-direct-subclass
    )
   )

(defpackage :json-rpc
  (:use :common-lisp :json)
  (:export
    #:clear-exported
    #:defun-json-rpc
    #:export-as-json-rpc
    #:invoke-rpc

    ;; restarts
    #:send-error
    #:send-error-object
    #:send-nothing
    #:send-internal-error
    ))
