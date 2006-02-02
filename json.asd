(in-package #:cl-user)

(defpackage #:json-system
    (:use #:cl #:asdf))

(in-package #:json-system)

(defsystem :json
  :name "cl-json"
  :description "JSON in Lisp. JSON (JavaScript Object Notation) is a lightweight data-interchange format."
  :version "0.1"
  :author "Henrik Hjelte <henrik@evahjelte.com>"
  :licence "LLGPL,Lisp Lesser GPL"
  :components ((:static-file "json.asd")
               (:module :src
                :components ((:file "package")
                             (:file "common" :depends-on ("package"))
                             (:file "decoder" :depends-on ("common"))
                             (:file "encoder" :depends-on ("common"))))))

(defsystem :json.test
  :depends-on (:json :fiveam)
  :components ((:module :t
               :components ((:file "package")
                            (:file "testjson" :depends-on ("package" "testdecoder" "testencoder"))
                            (:file "testdecoder" :depends-on ("package"))
                            (:file "testencoder" :depends-on ("package"))))))

